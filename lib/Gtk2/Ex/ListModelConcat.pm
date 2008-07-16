# Copyright 2008 Kevin Ryde

# This file is part of Gtk2-Ex-ListModelConcat.
#
# Gtk2-Ex-ListModelConcat is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Gtk2-Ex-ListModelConcat is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-ListModelConcat.  If not, see <http://www.gnu.org/licenses/>.


package Gtk2::Ex::ListModelConcat;
use strict;
use warnings;
use Gtk2;
use Carp;
use List::Util qw(min max);

our $VERSION = 1;

use constant DEBUG => 0;

use Glib::Object::Subclass
  Glib::Object::,
  interfaces => [ Gtk2::TreeModel::,
                  Gtk2::TreeDragSource::,
                  Gtk2::TreeDragDest:: ],
  properties => [ Glib::ParamSpec->scalar
                  ('models',
                   'models',
                   'Arrayref of Gtk2::TreeModel objects to concatenate.',
                   Glib::G_PARAM_READWRITE)
                ];

sub INIT_INSTANCE {
  my ($self) = @_;
  $self->{'models'} = [];
}

sub SET_PROPERTY {
  my ($self, $pspec, $newval) = @_;
  my $pname = $pspec->get_name;
  if (DEBUG) { print "ListModelConcat set '$pname'\n"; }

  if ($pname eq 'models') {
    foreach my $model (@$newval) {
      $model->isa('Gtk2::TreeModel') or croak "not a TreeModel: $model";
    }
    my $models = $self->{'models'};
    @$models = @$newval;  # copy input

    require Glib::Ex::SignalIds;
    require Scalar::Util;
    my @signals;
    $self->{'signals'} = \@signals;
    my %done_reordered;

    foreach my $i (0 .. $#$models) {
      my $model = $models->[$i];
      my $userdata = [ $self, $i ];
      # weaken to avoid a circular reference which would prevent a Concat
      # containing models from being garbage collected
      Scalar::Util::weaken ($userdata->[0]);

      # the reordered signal is only connected once if the model appears
      # multiple times
      my @reordered;
      $done_reordered{$model+0} ||= do {
        push @reordered, $model->signal_connect
          (rows_reordered => \&_do_rows_reordered, $userdata);
        1;
      };
      push @signals, Glib::Ex::SignalIds->new
        ($model,
         $model->signal_connect (row_changed => \&_do_row_changed, $userdata),
         $model->signal_connect (row_deleted => \&_do_row_deleted, $userdata),
         $model->signal_connect (row_inserted=> \&_do_row_inserted,$userdata),
         @reordered);
    }

  } else {
    $self->{$pname} = $newval;  # per default GET_PROPERTY
  }
}

# gtk_tree_model_get_flags
#
sub GET_FLAGS {
  my ($self) = @_;
  return [ 'list-only' ];
}

# gtk_tree_model_get_n_columns
#
sub GET_N_COLUMNS {
  my ($self) = @_;
  if (DEBUG) { print "ListModelConcat get_n_columns\n"; }
  my $model = $self->{'models'}->[0]
    or return 0; # when no models
  return $model->get_n_columns;
}

# gtk_tree_model_get_column_type
#
sub GET_COLUMN_TYPE {
  my ($self, $col) = @_;
  if (DEBUG >= 2) { print "ListModelConcat get_column_type\n"; }
  my $model = $self->{'models'}->[0] or _no_submodels('get_column_type');
  return $model->get_column_type ($col);
}

# gtk_tree_model_get_iter
#
sub GET_ITER {
  my ($self, $path) = @_;
  if (DEBUG >= 2) { print "ListModelConcat get_iter, path='",
                      $path->to_string,"'\n"; }
  if ($path->get_depth != 1) { return undef; }
  my ($index) = $path->get_indices;
  if ($index >= _total_length($self)) { return undef; }
  return _index_to_iter ($self, $index);
}

# gtk_tree_model_get_path
#
sub GET_PATH {
  my ($self, $iter) = @_;
  if (DEBUG >= 2) { print "ListModelConcat get_path\n"; }
  my $index = _iter_to_index ($self, $iter);
  return Gtk2::TreePath->new_from_indices ($index);
}

# gtk_tree_model_get_value
#
sub GET_VALUE {
  my ($self, $iter, $col) = @_;
  if (DEBUG >= 2) { print "ListModelConcat get_value iter=",
                      $iter->[0],",",$iter->[1], " col=$col\n"; }
  my ($model, $subiter) = _iter_to_subiter ($self, $iter);
  return $model->get_value ($subiter, $col);
}

# gtk_tree_model_iter_next
#
sub ITER_NEXT {
  my ($self, $iter) = @_;
  if (DEBUG >= 2) { print "ListModelConcat iter_next\n"; }
  my $index = _iter_to_index ($self, $iter);
  $index++;
  if ($index < _total_length($self)) {
    return _index_to_iter ($self, $index);
  } else {
    return undef;
  }
}

# gtk_tree_model_iter_children
#
sub ITER_CHILDREN {
  my ($self, $iter) = @_;
  if (DEBUG) { print "ListModelConcat iter_children\n"; }
  if (! defined $iter && ITER_HAS_CHILD ($self, undef)) {
    return _index_to_iter ($self, 0);
  } else {
    return undef;
  }
}

# gtk_tree_model_iter_has_child
#
sub ITER_HAS_CHILD {
  my ($self, $iter) = @_;
  if (DEBUG) { print "ListModelConcat has_child ",
                 defined $iter ? $iter->[1] : 'undef', "\n"; }
  if (defined $iter) {
    return 0;  # nothing under rows
  } else {
    # Crib note: prior to Gtk2-Perl 1.200 the return had to be a number, the
    # usual boolean false from != is ok for that
    return (_total_length($self) != 0);
  }
}

# gtk_tree_model_iter_n_children
#
sub ITER_N_CHILDREN {
  my ($self, $iter) = @_;
  if (DEBUG) { print "ListModelConcat iter_n_children\n"; }
  if (defined $iter) {
    return 0;  # nothing under rows
  } else {
    return _total_length($self);
  }
}

# gtk_tree_model_iter_nth_child
#
sub ITER_NTH_CHILD {
  my ($self, $iter, $n) = @_;
  if (DEBUG) { print "ListModelConcat iter_nth_child $n\n"; }
  if (defined $iter) {
    return undef;
  }
  if ($n < _total_length($self)) {
    return _index_to_iter ($self, $n);
  } else {
    return undef;
  }
}

# gtk_tree_model_iter_parent
#
sub ITER_PARENT {
  my ($self, $iter) = @_;
  if (DEBUG) { print "ListModelConcat iter_parent\n"; }
  return undef;
}

#------------------------------------------------------------------------------
# our iters

sub _index_to_iter {
  my ($self, $index) = @_;
  return [ $self+0, $index, undef, undef ];
}
sub _iter_to_index {
  my ($self, $iter) = @_;
  if (! defined $iter) { return undef; }
  if ($iter->[0] != $self+0) {
    croak "iter is not for this ", ref($self),
      " (stamp ", $iter->[0], " want ", $self+0, ")\n";
  }
  return $iter->[1];
}

sub _iterobj_to_index {
  my ($self, $iterobj) = @_;
  if (! defined $iterobj) { croak 'ListModelConcat: iter cannot be undef'; }
  return _iter_to_index ($self, $iterobj->to_arrayref ($self+0));
}
sub _index_to_iterobj {
  my ($self, $index) = @_;
  return Gtk2::TreeIter->new_from_arrayref (_index_to_iter ($self, $index));
}

sub _iterobj_to_subiter {
  my ($self, $iterobj) = @_;
  return _index_to_subiter ($self, _iterobj_to_index ($self, $iterobj));
}


#------------------------------------------------------------------------------
# sub-model lookups

sub _model_positions {
  my ($self) = @_;
  return ($self->{'positions'} ||= do {
    my $models = $self->{'models'};
    my $pos = 0;
    return ($self->{'positions'}
            = [ 0, map { $pos += $_->iter_n_children(undef) } @$models ]);
  });
}
sub _model_offset {
  my ($self, $mnum) = @_;
  my $positions = _model_positions ($self);
  return $positions->[$mnum];
}
sub _total_length {
  my ($self) = @_;
  return _model_positions($self)->[-1];
}

# return ($model, $subiter, $mnum)
sub _iter_to_subiter {
  my ($self, $iter) = @_;
  my $index = _iter_to_index ($self, $iter);
  return _index_to_subiter ($self, $index);
}

# return ($model, $subiter, $mnum)
sub _index_to_subiter {
  my ($self, $index) = @_;
  my ($model, $subindex, $mnum) = _index_to_subindex ($self, $index);
  return ($model, $model->iter_nth_child(undef,$subindex), $mnum);
}

# return ($model, $subindex, $mnum)
sub _index_to_subindex {
  my ($self, $index) = @_;
  if ($index < 0) {
    croak 'ListModelConcat: invalid iter (negative index)';
  }
  my $models = $self->{'models'};
  my $positions = _model_positions ($self);
  if ($index >= $positions->[-1]) {
    croak 'ListModelConcat: invalid iter (index too big)';
  }
  for (my $i = $#$positions - 1; $i >= 0; $i--) {
    if ($positions->[$i] <= $index) {
      return ($models->[$i], $index - $positions->[$i], $i);
    }
  }
  croak 'ListModelConcat: invalid iter (no sub-models at all now)';
}

# sub _bsearch {
#   my ($aref, $target) = @_;
#   my $lo = 0;
#   my $hi = @$aref;
#   for (;;) {
#     my $mid = int (($lo + $hi) / 2);
#     if ($mid == $lo) { return $mid; }
# 
#     my $elem = $aref->[$mid];
#     if ($elem > $target) {
#       $hi = $mid;
#     } elsif ($elem < $target) {
#       $lo = $mid+1;
#     } else {
#       return $mid;
#     }
#   }
# }

sub _no_submodels {
  my ($operation) = @_;
  croak "ListModelConcat: no sub-models to $operation";
}


#------------------------------------------------------------------------------

# 'row-changed' on the submodels
# called multiple times if a model is present multiple times
#
sub _do_row_changed {
  my ($model, $subpath, $subiter, $userdata) = @_;
  if (DEBUG) { print "ListModelConcat row_changed handler\n";}
  my ($self, $mnum)= @$userdata;
  if (! $self) { return; }
  if ($self->{'suppress_signals'}) { return; }
  if ($subpath->get_depth != 1) { return; }  # ignore non-toplevel

  my ($subindex) = $subpath->get_indices;
  my $index = $subindex + _model_offset($self,$mnum);
  my $path = Gtk2::TreePath->new_from_indices ($index);
  my $iterobj = _index_to_iterobj ($self, $index);
  $self->row_changed ($path, $iterobj);
}

# 'row-inserted' on the submodels
# called multiple times if a model is present multiple times, going from
# first to last, which should present the positions correctly to the
# listeners, even if the data has all the inserts already done
#
sub _do_row_inserted {
  my ($model, $subpath, $subiter, $userdata) = @_;
  if (DEBUG) { print "ListModelConcat row_inserted handler\n";}
  my ($self, $mnum) = @$userdata;
  if (! $self) { return; }
  if ($self->{'suppress_signals'}) { return; }
  if ($subpath->get_depth != 1) { return; }  # ignore non-toplevel

  if (my $positions = $self->{'positions'}) {
    foreach my $i ($mnum+1 .. $#$positions) {
      $positions->[$i] ++;
    }
  }

  my ($subindex) = $subpath->get_indices;
  my $index = $subindex + _model_offset($self,$mnum);
  my $path = Gtk2::TreePath->new_from_indices ($index);
  my $iterobj = _index_to_iterobj ($self, $index);
  $self->row_inserted ($path, $iterobj);
}

# 'row-deleted' on the submodels
# called multiple times if a model is present multiple times, going from
# first to last, which should present the positions correctly to the
# listeners, even if the data has all the inserts already done
#
sub _do_row_deleted {
  my ($model, $subpath, $userdata) = @_;
  my ($self, $mnum) = @$userdata;
  if (DEBUG) { print "ListModelConcat row_deleted handler\n";}
  if (! $self) { return; }
  if ($self->{'suppress_signals'}) { return; }
  if ($subpath->get_depth != 1) { return; }  # ignore non-toplevel

  if (my $positions = $self->{'positions'}) {
    foreach my $i ($mnum+1 .. $#$positions) {
      $positions->[$i] --;
    }
  }

  my ($subindex) = $subpath->get_indices;
  my $index = $subindex + _model_offset($self,$mnum);
  my $path = Gtk2::TreePath->new_from_indices ($index);
  $self->row_deleted ($path);
}

# 'rows-reordered' on the submodels
# called just once if a model is present multiple times, and a single
# rows-reordered with all changes generated here for listeners
#
sub _do_rows_reordered {
  my ($model, $path, $iter, $subaref, $userdata) = @_;
  my ($self, $mnum) = @$userdata;
  if (! $self) { return; }
  if (DEBUG) { print "ListModelConcat rows_reordered handler\n";}
  if ($self->{'suppress_signals'}) { return; }
  if ($path->get_depth != 0) { return; } # ignore non-toplevel

  # array[newpos] = oldpos, ie. the array elem says where the row used to be
  # before the reordering.  $subaref says that of its sub-model portion of
  # @array.
  #
  my @array = (0 .. _total_length($self) - 1);
  my $models = $self->{'models'};
  my $positions = _model_positions($self);
  foreach my $i (0 .. $#$models) {
    if ($models->[$i] == $model) {
      my $offset = $positions->[$i];
      foreach my $i (0 .. $#$subaref) {
        $array[$offset + $i] = $subaref->[$i] + $offset;
      }
    }
  }
  $self->rows_reordered ($path, undef, @array);
}


#------------------------------------------------------------------------------

# gtk_list_store_append
# new row at end, return iter pointing to it
sub append {
  my ($self) = @_;
  my $model = $self->{'models'}->[-1] or _no_submodels('append');
  return $model->append
    && _index_to_iterobj ($self, _total_length($self) - 1);
}

# gtk_list_store_prepend
# new row at start, return iter pointing to it
sub prepend {
  my ($self) = @_;
  my $model = $self->{'models'}->[0] or _no_submodels('prepend');
  return $model->prepend
    && _index_to_iterobj ($self, 0);
}

# The sub-models should generate row-deleted signals like Gtk2::ListModel
# does.  Normally it's just repeated delete of item 0, though if a model
# appears more than once in the Concat the copies further on are reported
# too, which leads to a strange, though correct, sequence.
sub clear {
  my ($self) = @_;
  foreach my $model (@{$self->{'models'}}) {
    $model->clear;
  }
}

sub set_column_types {
  my ($self, @types) = @_;
  foreach my $model (@{$self->{'models'}}) {
    $model->set_column_types (@types);
  }
}

sub set {
  my ($self, $iterobj, @pairs) = @_;
  if (DEBUG) { print "ListModelConcat set()\n";}
  my ($model, $subiter) = _iterobj_to_subiter ($self, $iterobj);
  $model->set ($subiter, @pairs);
}

# insert before $index, or append if $index past last existing row
# insert_with_values the same, taking col=>value pairs
sub insert {
  unshift @_, 'insert';
  goto &_insert;
}
sub insert_with_values {
  unshift @_, 'insert_with_values';
  goto &_insert;
}
sub _insert {
  my ($method, $self, $index, @args) = @_;
  my ($model, $subindex, $mnum);
  my $total_length = _total_length ($self);
  if ($index >= $total_length) {
    $index = $total_length; # in case wildly past end
    my $models = $self->{'models'};
    $model = $self->{'models'}->[-1]
      or _no_submodels($method);
    $mnum = $#$models;
    $subindex = $index; # past end
  } else {
    ($model, $subindex, $mnum) = _index_to_subindex ($self, $index);
  }
  my $subiter = $model->$method ($subindex, @args);
  return _subiter_to_iterobj ($self, $model, $subiter, $mnum);
}

# insert after $iterobj, or at beginning if $iterobj undef (yes, the beginning)
sub insert_after {
  unshift @_, 'insert_after', 0;
  goto &_insert_beforeafter;
}
sub insert_before {
  unshift @_, 'insert_before', -1;
  goto &_insert_beforeafter;
}
sub _insert_beforeafter {
  my ($method, $mnum, $self, $iterobj) = @_;
  my ($model, $subiter);
  if ($iterobj) {
    ($model, $subiter, $mnum) = _iterobj_to_subiter ($self, $iterobj);
  } else {
    my $models = $self->{'models'};
    $model = $models->[$mnum] or _no_submodels($method);
    if ($mnum) { $mnum = $#$models; }
    $subiter = undef;
  }
  $subiter = $model->$method ($subiter);
  return _subiter_to_iterobj ($self, $model, $subiter, $mnum);
}

sub _subiter_to_iterobj {
  my ($self, $model, $subiter, $mnum) = @_;
  my $positions = _model_positions ($self);
  my ($subindex) = $model->get_path($subiter)->get_indices;
  my $index = $positions->[$mnum] + $subindex;
  return _index_to_iterobj ($self, $index);
}

sub iter_is_valid {
  my ($self, $iter) = @_;
  my $a = eval { $iter->to_arrayref($self+0) };
  return ($a && $a->[1] < _total_length($self));
}

# gtk_list_store_move_after
# $dst_iterobj undef means the start (yes, the start) of the list
sub move_after {
  my ($self, $src_iterobj, $dst_iterobj) = @_;
  my $src_index = _iterobj_to_index ($self, $src_iterobj);
  my ($src_model, $src_subiter) = _index_to_subiter ($self, $src_index);

  my ($dst_index, $dst_model, $dst_subindex);
  if (defined $dst_iterobj) {
    $dst_index = _iterobj_to_index ($self, $dst_iterobj);
    ($dst_model, $dst_subindex) = _index_to_subindex ($self, $dst_index);
  } else {
    $dst_index = -1;
    $dst_model = $self->{'models'}->[0] or _no_submodels('insert_after');
    $dst_subindex = 0;
  }

  if ($src_model == $dst_model) {
    my $dst_subiter
      = $dst_iterobj && $dst_model->iter_nth_child (undef, $dst_subindex);
    $src_model->move_after ($src_subiter, $dst_subiter);

  } else {
    my $rem = _need_method ($src_model, 'remove');
    my $ins = _need_method ($dst_model, 'insert_with_values');
    my @row = _treemodel_extract_row ($src_model, $src_subiter);
    my $dst_ins_subindex = ($dst_iterobj ? $dst_subindex + 1 : 0);

    { local $self->{'suppress_signals'} = 1;
      $ins->($dst_model, $dst_ins_subindex, @row);
      $rem->($src_model, $src_subiter);
    }
    delete $self->{'positions'};  # recalculate

    _move_after_reorder ($self, $src_index, $dst_index);
  }
}

# Emit a 'rows-reordered' signal for a move of row $src_index to after
# $dst_index.  $dst_index can be -1 for the very start.
sub _move_after_reorder {
  my ($self, $src_index, $dst_index) = @_;
  my $path = Gtk2::TreePath->new;
  my $last_index = _total_length($self) - 1;

  if ($dst_index >= $src_index) {
    # upwards move eg. 0 to after 4 becomes 1,2,3,4,0
    $self->rows_reordered
      ($path, undef,
       0 .. $src_index-1,            # before, unchanged
       $src_index+1 .. $dst_index,   # shifted
       $src_index,                   # moved row
       $dst_index+1 .. $last_index); # after, unchanged

  } else {
    # downwards move eg. 4 to after 0 becomes 0,4,1,2,3
    $self->rows_reordered
      ($path, undef,
       0 .. $dst_index,              # before, unchanged
       $src_index,                   # moved row
       $dst_index+1 .. $src_index-1, # shifted
       $src_index+1 .. $last_index); # after, unchanged
  }
}

# gtk_list_store_move_before
# $dst_iterobj undef means the end (yes, the end) of the list
sub move_before {
  my ($self, $src_iterobj, $dst_iterobj) = @_;
  my $src_index = _iterobj_to_index ($self, $src_iterobj);
  my ($src_model, $src_subiter) = _index_to_subiter ($self, $src_index);

  my ($dst_index, $dst_model, $dst_subindex);
  if ($dst_iterobj) {
    $dst_index = _iterobj_to_index ($self, $dst_iterobj);
    ($dst_model, $dst_subindex) = _index_to_subindex ($self, $dst_index);
  } else {
    $dst_index = _total_length($self);
    $dst_model = $self->{'models'}->[-1] or _no_submodels('insert_after');
    $dst_subindex = $dst_index;
  }

  if ($src_model == $dst_model) {
    my $dst_subiter
      = $dst_iterobj && $dst_model->iter_nth_child (undef, $dst_subindex);
    $src_model->move_before ($src_subiter, $dst_subiter);

  } else {
    my $rem = _need_method ($src_model, 'remove');
    my $ins = _need_method ($dst_model, 'insert_with_values');
    my @row = _treemodel_extract_row ($src_model, $src_subiter);

    { local $self->{'suppress_signals'} = 1;
      $ins->($dst_model, $dst_subindex, @row);
      $rem->($src_model, $src_subiter);
    }
    delete $self->{'positions'};  # recalculate

    _move_after_reorder ($self, $src_index, $dst_index-1);
  }
}

sub _need_method {
  my ($model, $name) = @_;
  return ($model->can($name)
          || croak "ListModelConcat: submodel doesn\'t support '$name'");
}

# gtk_list_store_remove
#
# ENHANCE-ME: the real list store invalidates the given iter by zapping the
# stamp
#
sub remove {
  my ($self, $iterobj) = @_;
  if (! defined $iterobj) { croak 'Cannot remove iter \'undef\''; }
  my $index = _iterobj_to_index ($self, $iterobj);
  my ($model, $subiter) = _index_to_subiter ($self, $index);
  $model->remove ($subiter);

  # return true if more rows, with $iterobj pointing at the next one, which
  # for us that means no change to the index in $iterobj
  return ($index < _total_length($self));
}

sub _pre_duplicates {
  my ($self, $model, $index) = @_;

  my $models = $self->{'models'};
  my $positions = $self->{'positions'};
  my $pre_dups = 0;
  foreach my $i (0 .. $#$positions - 1) {
    if ($positions->[$i] > $index) { last; }
    if ($models->[$i] == $model) { $pre_dups++; }
  }
  return $pre_dups - 1;
}

# gtk_list_store_reorder
#
sub reorder {
  my ($self, @neworder) = @_;

  my $len = _total_length($self);
  if (@neworder != $len) {
    croak 'ListModelConcat: new order array wrong length';
  }

  my @row;
  foreach my $newpos (0 .. $#neworder) {
    my $oldpos = $neworder[$newpos];
    if ($oldpos < 0 || $oldpos >= $len) {
      croak "ListModelConcat: invalid old position in order array: $oldpos";
    }
    if ($oldpos != $newpos) {
      my ($model, $subiter) = _index_to_subiter ($self, $oldpos);
      $row[$oldpos] = [ _treemodel_extract_row ($model, $subiter) ];
    }
  }
  { local $self->{'suppress_signals'} = 1;
    foreach my $newpos (0 .. $#neworder) {
      my $oldpos = $neworder[$newpos];
      if ($oldpos != $newpos) {
        my $data = $row[$oldpos];
        my ($model, $subiter) = _index_to_subiter ($self, $newpos);
        $model->set ($subiter, @$data);
      }
    }
  }
  $self->rows_reordered (Gtk2::TreePath->new, undef, @neworder);


  # If there's cycles wholely within a single sub-model then they can be
  # applied with the submodel's reorder method, if it's got one.  The
  # advantage would be that small swaps or shuffles can be delegated,
  # instead of a lot of data copying.
  #
  #   my @seen;
  #     my $models = $self->{'models'};
  #   my $positions = _model_positions ($self);
  #   foreach my $mnum (0 .. $#$models) {
  #     my $model = $models->[$mnum];
  #     my $reorder = $model->can('reorder') or next;
  #     my $lo = $positions->[$mnum];
  #     my $hi = $positions->[$mnum+1] - 1;
  #     my @subarray = (0 .. $hi-$lo);
  #     my $diff = 0;
  #     foreach my $index ($lo .. $hi) {
  #
  #     }
  #     $diff or next;
  #     { local $self->{'suppress_signals'} = 1;
  #       $reorder->(@subarray);
  #     }
  #   }
}

sub swap {
  my ($self, $iterobj_a, $iterobj_b) = @_;
  my $index_a = _iterobj_to_index ($self, $iterobj_a);
  my $index_b = _iterobj_to_index ($self, $iterobj_b);

  my ($model_a, $subiter_a) = _index_to_subiter ($self, $index_a);
  my ($model_b, $subiter_b) = _index_to_subiter ($self, $index_b);
  if ($model_a == $model_b) {
    $model_a->swap ($subiter_a, $subiter_b);

  } else {
    my @row_a = _treemodel_extract_row ($model_a, $subiter_a);
    my @row_b = _treemodel_extract_row ($model_b, $subiter_b);
    { local $self->{'suppress_signals'} = 1;
      $model_a->set ($subiter_a, @row_b);
      $model_b->set ($subiter_b, @row_a); }

    my @array = (0 .. _total_length($self) - 1);
    $array[$index_a] = $index_b;   # $array[newpos] == oldpos
    $array[$index_b] = $index_a;
    $self->rows_reordered (Gtk2::TreePath->new, undef, @array);
  }
}

# return a list of values (0, 'col0', 1, 'col1', ...) which is the column
# number and its contents
sub _treemodel_extract_row {
  my ($model, $iter) = @_;
  return map {; ($_, $model->get_value($iter,$_)) }
    0 .. $model->get_n_columns - 1;
}
# multi-column extraction from a perl-code model broken in Gtk2-Perl 1.183
# sub _treemodel_extract_row {
#   my ($model, $iter) = @_;
#   my @row = $model->get($iter);
#   return map {; ($_,$row[$_]) } 0 .. $#row;
# }

sub set_value {
  my ($self, $iterobj, $col, $val) = @_;
  my $iter = $iterobj->to_arrayref ($self+0);
  my ($model, $subiter) = _iter_to_subiter ($self, $iter);
  $model->set_value ($subiter, $col, $val);
}

#------------------------------------------------------------------------------
# drag source

# gtk_tree_drag_source_row_draggable ($self, $path)
#
sub ROW_DRAGGABLE {
  unshift @_, 'row_draggable';
  goto &_drag_source;
}

# gtk_tree_drag_source_drag_data_delete ($self, $path)
#
sub DRAG_DATA_DELETE {
  unshift @_, 'drag_data_delete';
  goto &_drag_source;
}

# gtk_tree_drag_source_drag_data_get
#
sub DRAG_DATA_GET {
  my ($self, $path, $sel) = @_;
  # hack pending Gtk2-Perl support for $sel arg to drag_data_get()
  my $method = sub {
    my ($model, $subpath, $sel) = @_;
    return eval { $model->drag_data_get ($subpath, $sel); 1 }
      || do {
        if (DEBUG) {print "  fallback own set_row_drag_data\n"; }
        $sel->set_row_drag_data ($self, $path);
      };
  };
  unshift @_, $method;
  goto &_drag_source;
}

sub _drag_source {
  my ($method, $self, $path, @sel) = @_;
  if (DEBUG) { print "ListModelConcat ",uc($method)," path=",
                 $path->to_string,"\n"; }

  if ($path->get_depth != 1) {
    if (DEBUG) { print "  no, not a toplevel row\n"; }
    return 0;
  }
  my ($index) = $path->get_indices;
  my ($model, $subindex) = _index_to_subindex ($self, $index);

  if (! $model->isa('Gtk2::TreeDragSource')) {
    if (DEBUG) { print "  no, submodel not a TreeDragSource\n"; }
    return 0;
  }
  my $subpath = Gtk2::TreePath->new_from_indices ($subindex);
  if (DEBUG) { print "  submodel row_draggable subpath=",
                 $subpath->to_string,"\n"; }
  my $ret = $model->$method ($subpath, @sel);
  if (DEBUG) { print "  ",$ret?"yes":"no","\n"; }
  return $ret;
}

#------------------------------------------------------------------------------
# drag destination

# gtk_tree_drag_dest_row_drop_possible
#
sub ROW_DROP_POSSIBLE {
  my ($self, $path, $sel) = @_;
  if (DEBUG) { print "ListModelConcat ROW_DROP_POSSIBLE, to path=",
                 $path->to_string," type=",$sel->type->name,"\n"; }
  if ($path->get_depth != 1) {
    if (DEBUG) { print "  no, not a toplevel row\n"; }
    return 0;
  }
  my ($index) = $path->get_indices;
  my ($model, $subindex) = _index_to_subindex_post ($self, $index);

  # if the submodel implements DragDest and it's prepared to accept $sel
  # directly
  if ($model->isa('Gtk2::TreeDragDest')) {
    my $subpath = Gtk2::TreePath->new_from_indices ($subindex);
    if ($model->row_drop_possible ($subpath, $sel)) {
      if (DEBUG) { print "  yes, submodel row_drop_possible()\n"; }
      return 1;
    }
  }

  # otherwise we can copy a row to any submodel implementing
  # 'insert_with_values'
  if ($sel->type->name eq 'GTK_TREE_MODEL_ROW'
      && $model->can('insert_with_values')) {
    my ($src_model, $src_path) = $sel->get_row_drag_data;
    if (_column_types_compatible ($src_model, $model)) {
      if (DEBUG) { print "  yes, submodel insert_with_values()\n"; }
      return 1;
    }
  }

  if (DEBUG) { print "  no\n"; }
  return 0;
}

# gtk_tree_drag_dest_drag_data_received
#
sub DRAG_DATA_RECEIVED {
  my ($self, $dst_path, $sel) = @_;
  if (DEBUG) { print "ListModelConcat DRAG_DATA_RECEIVED, to path=",
                 $dst_path->to_string," type=",$sel->type->name,"\n";
               if ($sel->type->name eq 'GTK_TREE_MODEL_ROW') {
                 my ($src_model, $src_path) = $sel->get_row_drag_data;
                 print "  src_model=$src_model src_path=",
                   $src_path->to_string,"\n";
               }}

  if ($dst_path->get_depth != 1) {
    if (DEBUG) { print "  no, not a toplevel row\n"; }
    return 0;
  }
  my ($dst_index) = $dst_path->get_indices;
  my ($dst_submodel, $dst_subindex) = _index_to_subindex_post ($self,
                                                               $dst_index);
  my $dst_subpath = Gtk2::TreePath->new_from_indices ($dst_subindex);

  # maybe the submodel will accept the data directly
  if ($dst_submodel->isa('Gtk2::TreeDragDest')) {
    if ($dst_submodel->drag_data_received ($dst_subpath, $sel)) {
      if (DEBUG) { print "  accepted by submodel\n";}
      return 1;
    }
  }

  # otherwise we can handle a row
  my ($src_model, $src_path) = $sel->get_row_drag_data;
  if (! $src_model) {
    if (DEBUG) { print "  no, source is not a model row\n";}
    # gtkliststore.c has a fixme about perhaps handling some data targets,
    # the same would apply here, a drop of plain text into a row with a
    # single Glib::String column would be fairly sensible.  But perhaps
    # better is to leave that to the submodels to handle.
    return 0;
  }

  # if the drag is between different models then copy columns (of the lesser
  # number), provided the types are the same or compatible
  #
  my $cols = min ($self->get_n_columns, $src_model->get_n_columns);
  if (! _column_types_compatible ($src_model, $self, $cols)) {
    return 0;
  }
  my $src_iter = $src_model->get_iter ($src_path);
  if (! $src_iter) {
    if (DEBUG) { print "  no iter for source path\n";}
    return 0;
  }
  my @row = _treemodel_extract_row ($src_model, $src_iter);
  if (@row > 2*$cols) { $#row = 2*$cols - 1; }

  # data receive is supposed to be rugged against dodgy stuff, so protect
  # here with an eval
  if (! eval { $self->insert_with_values ($dst_index, @row); 1 }) {
    if (DEBUG) { print "  error from insert_with_values: $@\n"; }
    return 0;
  }
  return 1;
}

# return true if the first $cols many columns of $src_model and $dst_model
# are compatible enough for a data copy
#
# must check "eq" of types, not just isa(), because some types like
# Glib::Float are not perl classes as such (as of Glib-Perl 1.190 at least)
#
sub _column_types_compatible {
  my ($src_model, $dst_model, $cols) = @_;
  if ($src_model == $dst_model) { return 1; }

  if (! defined $cols) {
    $cols = min ($src_model->get_n_columns, $dst_model->get_n_columns);
  }
  foreach my $i (0 .. $cols-1) {
    my $src_type = $src_model->get_column_type ($i);
    my $dst_type = $dst_model->get_column_type ($i);
    if (! ($src_type eq $dst_type || $src_type->isa($dst_type))) {
      if (DEBUG) { print "  different types in column $i:",
                     " $src_type, $dst_type\n"; }
      return 0;
    }
  }
  return 1;
}

# return ($model, $subindex), and allowing a $index which is past the end of
# $self to likewise give a subindex beyond the end of the submodel
#
sub _index_to_subindex_post {
  my ($self, $index) = @_;
  my $positions = _model_positions ($self);
  if ($index < $positions->[-1]) {
    return _index_to_subindex ($self, $index);
  }
  my $model = $self->{'models'}->[-1]
    or return (undef, undef);  # no models at all
  return ($model, $index - $positions->[-2]);
}


1; __END__

=head1 NAME

Gtk2::Ex::ListModelConcat -- concatenated list models

=head1 SYNOPSIS

 use Gtk2::Ex::ListModelConcat;
 my $model = Gtk2::Ex::ListModelConcat->new (models => [$m1,$m2]);

=head1 OBJECT HIERARCHY

C<Gtk2::Ex::ListModelConcat> is a subclass of C<Glib::Object>.

    Glib::Object
      Gtk2::Ex::ListModelConcat

and implements the interfaces

    Gtk2::TreeModel
    Gtk2::TreeDragSource
    Gtk2::TreeDragDest

=head1 DESCRIPTION

C<Gtk2::Ex::ListModelConcat> presents a set of list-type TreeModels
concatenated together as a single list.  A Concat doesn't hold any data
itself, it just presents the sub-models' content.  C<Gtk2::ListStore>
objects are suitable as the sub-models, but any similar list-type model can
be used too.

Changes in the sub-models are reported up through the Concat with the usual
C<row-changed> etc signals.  Conversely change methods are implemented by
Concat in the style of C<Gtk2::ListStore> and if the sub-models have those
functions too (eg. if they're ListStores) then changes on the Concat are
applied down to the sub-models.

The sub-models should all have the same number of columns and the same
column types (or compatible types), though currently ListModelConcat doesn't
try to enforce that.  It works to put one Concat inside another, except of
course it cannot be inside itself (directly or indirectly).

=head2 Drag and Drop

ListModelConcat implements TreeDragSource and TreeDragDest, allowing rows to
be moved by dragging in a C<Gtk2::TreeView> or similar.

A row can be dragged if its sub-model implements the TreeDragSource
interface.  A position can be a drag destination if its sub-model implements
either the TreeDragDest interface or a C<Gtk2::ListStore> style
C<insert_with_values> method.  TreeDragDest is preferred, letting the
submodel take care of data conversion, but the C<insert_with_values>
fallback is needed for drags between different ListStores, since its
TreeDragDest only accepts its own rows.

=head1 PROPERTIES

=over 4

=item C<models> (array reference, default empty C<[]>)

Arrayref of sub-models to concatenate.  The sub-models can be any object
implementing the C<Gtk2::TreeModel> interface.  They should be C<list-only>
type, though currently ListModelConcat doesn't enforce that.

Currently when the C<models> property is changed there's no C<row-inserted>
/ C<row-deleted> etc signals emitted by the Concat to announce the new or
altered data presented.  Perhaps this will change.  The disadvantage would
be that adding or removing a big model could generate thousands of fairly
pointless signals.  The suggestion is to treat C<models> as if it were
"construct-only" and make a new Concat for a new set of models.

=back

=head1 FUNCTIONS

=over 4

=item C<< $concat = Gtk2::Ex::ListModelConcat->new (key=>value,...) >>

Create and return a new Concat object.  Optional key/value pairs set initial
properties as per C<< Glib::Object->new >>.  Eg.

 my $concat = Gtk2::Ex::ListModelConcat->new (models => [$m1,$m2]);

=back

=head1 LISTSTORE METHODS

The following functions are implemented in the style of C<Gtk2::ListStore>
and they call down to corresponding functions in the sub-models.  Those
sub-models don't have to be C<Gtk2::ListStore> objects, they can be some
other class implementing the same methods.

=over 4

=item C<< $concat->clear >>

=item C<< $concat->set_column_types >>

These are applied to all sub-models, so C<clear> clears all the models or
C<set_column_types> sets the types in all the models.

In the current implementation Concat doesn't keep column types itself, but
asks the sub-models when required (using the first sub-model, curently).

=item C<< $iter = $concat->append >>

=item C<< $iter = $concat->insert ($pos) >>

=item C<< $iter = $concat->insert_with_values ($pos, $col,$val, ...) >>

=item C<< $iter = $concat->insert_after ($iter) >>

=item C<< $iter = $concat->insert_before ($iter) >>

=item C<< bool = $concat->iter_is_valid ($iter) >>

=item C<< $concat->move_after ($iter, $iter_from, $iter_to) >>

=item C<< $concat->move_before ($iter, $iter_from, $iter_to) >>

=item C<< $iter = $concat->prepend >>

=item C<< bool = $concat->remove ($iter) >>

=item C<< $concat->reorder (@neworder) >>

=item C<< $concat->swap ($iter_a, $iter_b) >>

=item C<< $concat->set ($iter, $col,$val, ...) >>

=item C<< $concat->set_value ($iter, $col, $val) >>

These are per the C<Gtk2::ListStore> methods.

Note C<set> overrides the C<set> in C<Glib::Object> which normally sets
object properties.  You can use the C<set_property> alias instead.

    $model->set_property ('propname' => $value);

=back

=head1 SIGNALS

The TreeModel interface (implemented by ListModelConcat) provides the
following usual signals

    row-changed    ($concat, $path, $iter, $userdata)
    row-inserted   ($concat, $path, $iter, $userdata)
    row-deleted    ($concat, $path, $userdata)
    rows-reordered ($concat, $path, $iter, $arrayref, $userdata)

Because ListModelConcat is C<list-only>, the path to C<row-changed>,
C<row-inserted> and C<row-deleted> always has depth 1, and the path to
C<rows-reordered> is always depth 0 and the iter there is always C<undef>.

When a change occurs in a sub-model the corresponding signal is reported up
through Concat too.  Of course the path and iter reported by the Concat are
in its "concatenated" coordinates and iters, not the sub-model's.

=head1 BUGS

C<ref_node> and C<unref_node> are no-ops.  The intention is to apply them
down on the sub-models, but hopefully without needing lots of bookkeeping in
the Concat as to what's currently reffed.

It mostly works to have a sub-model appear more than once in a Concat.  The
only outright bug is in the C<remove> method which doesn't update its iter
correctly when removing a row from a second or subsequent copy of a
submodel.  The C<row-deleted> and C<row-inserted> signals are emitted on the
Concat the right number of times, but the multiple inserts/deletes are all
present in the data as of the first emit, which might confuse handler code.
(The idea could be some sort of temporary index mapping to make the changes
seem one-at-a-time for the handlers.)

What does work fine is to have multiple TreeModelFilters selecting different
parts of a single underlying model.  As long as a given row only appears
once it doesn't matter where its ultimate storage location is.

=head1 SEE ALSO

L<Gtk2::TreeModel>, L<Gtk2::TreeDragSource>, L<Gtk2::TreeDragDest>,
L<Gtk2::ListStore>, L<Glib::Object>

=head1 HOME PAGE

L<http://www.geocities.com/user42_kevin/gtk2-ex-listmodelconcat/>

=head1 COPYRIGHT

Copyright 2008 Kevin Ryde

Gtk2-Ex-ListModelConcat is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option) any
later version.

Gtk2-Ex-ListModelConcat is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along with
Gtk2-Ex-ListModelConcat.  If not, see L<http://www.gnu.org/licenses/>.

=cut
