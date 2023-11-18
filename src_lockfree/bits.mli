(** Operations dealing with the bitwise representation of integers. *)

val ceil_pow_2_minus_1 : int -> int
(** [ceil_pow_2_minus_1 n] returns the smallest value of the form [m = (1 lsl s) - 1]
    such that [n <= m].

    ⚠️ This treats integers as if they were non-negative and overflow is
    possible. *)

val ceil_pow_2 : int -> int
(** [ceil_pow_2 n] returns the smallest value of the form [m = 1 lsl s] such
    that [n <= m].

    ⚠️ This treats integers as if they were non-negative and overflow is
    possible. *)

val floor_pow_2 : int -> int
(** [floor_pow_2 n] returns the largest value of the form [m = 1 lsl s] such
    that [m <= n].

    ⚠️ This treats integers as if they were non-negative and overflow is
    possible. *)
