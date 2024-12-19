(** [signum n] is the same as [Int.compare n 0].
*)
val signum : int -> int

(** [sign n] is
    - [+] if [n > 0]
    - [-] if [n < 0]
    - [ ] if [n = 0]
*)
val sign : int -> string

(** [sign_no_plus n] is
    - [-] if [n < 0]
    - [ ] otherwise
*)
val sign_no_plus : int -> string

(** [gcd a b] is the GCD of integers [a] and [b].
*)
val gcd : int -> int -> int

(** [gcd_all il] is the GCD of the integers in the list [il].
*)
val gcd_all : int list -> int

(** [lcm a b] is the LCM of the integers [a] and [b].
*)
val lcm : int -> int -> int

(** [lcm_all il] is the LCM of the integers in the list [il].
*)
val lcm_all : int list -> int