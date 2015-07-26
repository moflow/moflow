(** Worklists with in place modification *)

(** The type of a worklist *)
type 'a t

exception Empty
(** Raised when [take] or [peek] is applied to an empty worklist. *)

val create : unit -> 'a t
(** Return a new worklist, initially empty. *)

val add : 'a -> 'a t -> unit
(** [add x w] adds the element [x] at the end of the worklist [w] if
    [x] is not already in [w]. If [x] is already in [w], the worklist does
    not change.

    The current implementation of [add] is [O(n)], where [n = length
    w].
*)

val push : 'a -> 'a t -> unit
(** [push] is a synonym for [add]. *)

val add_list : 'a list -> 'a t -> unit
(** [add_list l w] adds the elements in [l] to the worklist [w].
*)

val filter : ('a -> bool) -> 'a t -> unit
(** [filter f w] removes any element e from [w] when [f e] is
    false. *)

val take : 'a t -> 'a
(** [take w] removes and returns the first element of worklist [w], or
    raises [Empty] if the worklist is empty. *)

val pop : 'a t -> 'a
(** [pop] is a synonym for [take]. *)

val peek : 'a t -> 'a
(** [peek w] returns the first element in worklist [w], without
    removing it from the worklist, or raises [Empty] if the worklist is
    empty. *)

val top : 'a t -> 'a
(** [top] is a synonym for [peek]. *)

val all : 'a t -> 'a list
(** [all w] returns all elements in worklist [w]. *)

val clear : 'a t -> unit
(** Discard all elements from a worklist. *)

val is_empty : 'a t -> bool
(** Return [true] if the given worklist is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a worklist. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The worklist itself is unchanged. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f accu q] is equivalent to [List.fold_left f accu l],
   where [l] is the list of [q]'s elements. The worklist remains
   unchanged. *)
