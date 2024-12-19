(** {1 Fraction Type} *)

(** The fraction type, which is either:
    - a valid fraction, with non-zero denominator
    - positive infinity
    - negative infinity
    - not a number (the indeterminate forms, like [0]/[0])
*)
type t

(** This exception is raised whenever an attempt is made to reduce
    an indeterminate fraction into another type which does not
    support indeterminate forms.
*)
exception NaN

(** Positive infinity
*)
val infinity : t

(** Negative infinity
*)
val neg_infinity : t

(** Special fraction denoting indeterminate forms like [0]/[0] or [0]*{!infinity}
*)
val nan : t

(** [is_infinity f] checks if [f] is equal to positive infinity.
*)
val is_infinity : t -> bool

(** [is_neg_infinity f] checks if [f] is equal to negative infinity.
*)
val is_neg_infinity : t -> bool

(** [is_nan f] checks if [f] is indeterminate.
*)
val is_nan : t -> bool

(** [is_integer f] checks if [f] is an integer.
*)
val is_integer : t -> bool


(** {1 Constructors} *)

(** [of_int i] is the fraction equivalent of integer [i].
*)
val of_int : int -> t

(** [of_tuple t] converts the tuple [(i1, i2)] into a fraction.
*)
val of_tuple : int * int -> t


(** {1 Deconstructors} *)

(** [to_int f] is the integer part of [f].

    @raise Stdlib.Division_by_zero If [f] is infinity.
    @raise NaN If [f] is not a number.
*)
val to_int : t -> int

(** [to_tuple f] converts the fraction [f] into a tuple,
    with the first value being the numerator and second being the denominator.
    
    @raise Stdlib.Division_by_zero If [f] is infinity.
    @raise NaN If [f] is not a number.
*)
val to_tuple : t -> int * int

(** [to_float f] converts the fraction [f] into a float.
    It does not raise. It converts 
    - {!infinity} into {!Stdlib.Float.infinity}
    - {!neg_infinity} into {!Stdlib.Float.neg_infinity}
    - {!nan} into {!Stdlib.Float.nan}
*)
val to_float : t -> Float.t


(** {1 Constants} *)

(** The fraction for [0].
*)
val zero : t

(** The fraction for [1].
*)
val one : t

(** The fraction for [-1].
*)
val minus_one : t


(** {1 Reduction} *)

(** [reduce f] reduces [f] to its lowest fractional representation.
*)
val reduce : t -> t

(** [reduce_strict f] reduces [f] if it is a finite fraction.

    @raise Stdlib.Division_by_zero If [f] is infinity.
    @raise NaN If [f] is not a number.
*)
val reduce_strict : t -> t

(** [reduce_strict_opt f] reduces [f] if it is a finite fraction.
    It returns [None] otherwise.
*)
val reduce_strict_opt : t -> t option


(** {1 Comparison} *)

(** [equal f1 f2] checks if [f1] and [f2] are equal.
*)
val equal : t -> t -> bool

(** [compare f1 f2] compares [f1] and [f2] and is:
    - [-1] if [f1] < [f2]
    - [0] if [f1] = [f2]
    - [1] if [f1] > [f2]
*)
val compare : t -> t -> int

(** [sigfrac f] is the same as {!compare}[ f ]{!zero}.
*)
val sigfrac : t -> int

(** [sign f] is
    - [+] if [f > 0]
    - [-] if [f < 0]
    - [ ] if [f = 0]
*)
val sign : t -> string

(** [sign_no_plus f] is
    - [-] if [f < 0]
    - [ ] otherwise
*)
val sign_no_plus : t -> string

(** [min f1 f2] is the minimum of [f1] and [f2]. It returns {!nan}
    whenever either [f1] or [f1] are {!nan}.
*)
val min : t -> t -> t

(** [max f1 f2] returns the maximum of [f1] and [f2]. It returns {!nan}
    whenever either [f1] or [f1] are {!nan}.
*)
val max : t -> t -> t

(** [min_max f1 f2] is [(]{!min}[ f1 f2, ]{!max}[ f1 f2)].
*)
val min_max : t -> t -> t * t

(** [min_num f1 f2] returns the minimum of [f1] and [f2] treating {!nan} as missing values.
    If both [f1] and [f2] are {!nan}, {!nan} is returned.
*)
val min_num : t -> t -> t

(** [max_num f1 f2] returns the maximum of [f1] and [f2] treating {!nan} as missing values.
    If both [f1] and [f2] are {!nan}, {!nan} is returned.
*)
val max_num : t -> t -> t

(** [min_max_num f1 f2] is [(]{!min_num}[ f1 f2, ]{!max_num}[ f1 f2)].
*)
val min_max_num : t -> t -> t * t

(** {1 Arithmetic Operations} *)

(** [negate f] is the additive inverse of [f].
*)
val negate : t -> t

(** [add f1 f2] is the sum of [f1] and [f2].
*)
val add : t -> t -> t

(** [sub f1 f2] is equivalent to {!add}[ f1 (]{!negate}[ f2)].
*)
val sub : t -> t -> t

(** [inverse f] is the multiplicative inverse of [f].
*)
val inverse : t -> t

(** [mul f1 f2] is the product of [f1] and [f2].
*)
val mul : t -> t -> t

(** [div f1 f2] is equivalent to {!mul}[ f1 (]{!inverse}[ f2)].
*)
val div : t -> t -> t


(** {1 Pretty Printer} *)

(** [pretty f] is the pretty print of [f].
    - If [f] is {!zero}, then it displays a ["+0"].
    - If [f] is {!is_integer}, then it displays the integer, preceded by its sign.
    - If [f] is finite, then it displats the fraction, preceded by its sign.
    - If [f] is positive infinity, then it displays ["infinity"]
    - If [f] is negative infinity, then it displays ["neg_infinity"]
    - If [f] is not a number, then it displays ["nan"]
*)
val pretty : t -> string

(** [pretty_no_plus f] is same as {!pretty }[f], except the ["+"] sign is droped.
*)
val pretty_no_plus : t -> string

(** [pretty_unsigned f] is same as {!pretty }[f], except all signs are dropped.
*)
val pretty_unsigned : t -> string

(** [pp fmt f] is a custom printer for the fraction type, and can be used with {!Stdlib.Format.printf}.
*)
val pp : Format.formatter -> t -> unit
