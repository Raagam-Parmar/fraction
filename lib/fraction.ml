type t =
    | Fraction of 
        {
            numerator : int;
            denominator : int;
        }
    | Infinity
    | Neg_infinity
    | Not_a_number

exception NaN

let infinity = Infinity
let neg_infinity = Neg_infinity
let nan = Not_a_number

let is_infinity f = 
    match f with
    | Infinity     -> true
    | Fraction _   -> false
    | Neg_infinity -> false
    | Not_a_number -> false

let is_neg_infinity f = 
    match f with
    | Neg_infinity -> true
    | Fraction _   -> false
    | Infinity     -> false
    | Not_a_number -> false

let is_nan f = 
    match f with
    | Not_a_number -> true
    | Fraction _   -> false
    | Infinity     -> false
    | Neg_infinity -> false

let is_integer f = 
    match f with
    | Fraction f'  -> Int.abs (f'.denominator) = 1
    | Infinity     -> false
    | Neg_infinity -> false
    | Not_a_number -> false


(** Constructors *)

let of_int i = 
    Fraction
    {
        numerator = i;
        denominator = 1;
    }

let _of_float _i = 
    failwith "TODO"

let of_tuple t = 
    let num = fst t in
    let den = snd t in
    if den = 0 then
        if num = 0 then
            Not_a_number
        else if num > 0 then
            Infinity
        else
            Neg_infinity
    else
        let gcd = IntUtils.gcd num den in
        Fraction {
            numerator = num / gcd;
            denominator = den / gcd;
        }


(** Deconstructors *)

let to_int f = 
    match f with
    | Fraction f'  -> f'.numerator / f'.denominator
    | Infinity     -> raise Division_by_zero
    | Neg_infinity -> raise Division_by_zero
    | Not_a_number -> raise NaN

let to_float f = 
    match f with
    | Fraction f'  -> float_of_int f'.numerator /. float_of_int f'.denominator
    | Infinity     -> Float.infinity
    | Neg_infinity -> Float.neg_infinity
    | Not_a_number -> Float.nan

let to_tuple f = 
    match f with
    | Fraction f'  -> (f'.numerator, f'.denominator)
    | Infinity     -> raise Division_by_zero
    | Neg_infinity -> raise Division_by_zero
    | Not_a_number -> raise NaN


(** Constants *)

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)


(** Reduction *)

let reduce f = 
    match f with
    | Fraction f'  -> 
        let num = f'.numerator in
        let den = f'.denominator in
        let gcd = IntUtils.gcd num den in
        Fraction {
            numerator = num / gcd;
            denominator = den / gcd;
        }
    | Infinity     -> Infinity
    | Neg_infinity -> Neg_infinity
    | Not_a_number -> Not_a_number

let reduce_strict f = 
    match reduce f with
    | Fraction f'  -> Fraction f'
    | Infinity     -> raise Division_by_zero
    | Neg_infinity -> raise Division_by_zero
    | Not_a_number -> raise NaN

let reduce_strict_opt f = 
    match reduce f with
    | Fraction f'  -> Some (Fraction f')
    | Infinity     -> None
    | Neg_infinity -> None
    | Not_a_number -> None


(** Comparison *)

let equal f1 f2 = 
    match (f1, f2) with
    | (Fraction f1', Fraction f2') ->
        if f1'.numerator = 0 && f2'.numerator = 0 then true
        else 
            let n1 = f1'.numerator * f2'.denominator in
            let n2 = f1'.denominator * f2'.numerator in
            n1 = n2
    | (Fraction _, Infinity)       -> false
    | (Fraction _, Neg_infinity)   -> false
    | (Fraction _, Not_a_number)   -> false

    | (Infinity, Fraction _)       -> false
    | (Infinity, Infinity)         -> true
    | (Infinity, Neg_infinity)     -> false
    | (Infinity, Not_a_number)     -> false
    
    | (Neg_infinity, Fraction _)   -> false
    | (Neg_infinity, Infinity)     -> false
    | (Neg_infinity, Neg_infinity) -> true
    | (Neg_infinity, Not_a_number) -> false

    | (Not_a_number, Fraction _)   -> false
    | (Not_a_number, Infinity)     -> false
    | (Not_a_number, Neg_infinity) -> false
    | (Not_a_number, Not_a_number) -> true

let compare f1 f2 = 
    match (f1, f2) with
    | (Fraction f1', Fraction f2') ->
        if f1'.numerator = 0 && f2'.numerator = 0 then 0
        else
            let n1 = f1'.numerator * f2'.denominator in
            let n2 = f1'.denominator * f2'.numerator in
            compare n1 n2
    | (Fraction _, Infinity)       -> -1
    | (Fraction _, Neg_infinity)   -> 1
    | (Fraction _, Not_a_number)   -> 1

    | (Infinity, Fraction _)       -> 1
    | (Infinity, Infinity)         -> 0
    | (Infinity, Neg_infinity)     -> 1
    | (Infinity, Not_a_number)     -> 1
    
    | (Neg_infinity, Fraction _)   -> -1
    | (Neg_infinity, Infinity)     -> -1
    | (Neg_infinity, Neg_infinity) -> 0
    | (Neg_infinity, Not_a_number) -> 1

    | (Not_a_number, Fraction _)   -> -1
    | (Not_a_number, Infinity)     -> -1
    | (Not_a_number, Neg_infinity) -> -1
    | (Not_a_number, Not_a_number) -> 0   

let sigfrac f = 
    compare f (of_int 0)

let sign f = 
    let sf = sigfrac f in
    if sf < 0 then "-"
    else if sf = 0 then ""
    else "+"

let sign_no_plus f = 
    let sf = sigfrac f in
    if sf < 0 then "-"
    else ""


(** Arithmetic Operations *)

let negate f = 
    match f with
    | Fraction f' -> Fraction {
            numerator = (-1) * f'.numerator;
            denominator = f'.denominator;
        }
    | Infinity -> Neg_infinity
    | Neg_infinity -> Infinity
    | Not_a_number -> Not_a_number

let rec mul f1 f2 =     
    match (f1, f2) with
    | (Fraction f1', Fraction f2') ->
        let f = Fraction
        {
            numerator = f1'.numerator * f2'.numerator; 
            denominator = f1'.denominator * f2'.denominator;
        } 
        in reduce f 
    | (Fraction _, Infinity)       -> 
        let sgn = sigfrac f1 in
        if sgn = 0 then Not_a_number
        else if sgn > 0 then Infinity
        else Neg_infinity
    | (Fraction _, Neg_infinity)   -> mul (negate f1) (negate f2)
    | (Fraction _, Not_a_number)   -> Not_a_number

    | (Infinity, Fraction _)       -> mul f2 f1
    | (Infinity, Infinity)         -> Infinity
    | (Infinity, Neg_infinity)     -> Neg_infinity
    | (Infinity, Not_a_number)     -> Not_a_number
    
    | (Neg_infinity, Fraction _)   -> mul f2 f1
    | (Neg_infinity, Infinity)     -> Neg_infinity
    | (Neg_infinity, Neg_infinity) -> Infinity
    | (Neg_infinity, Not_a_number) -> Not_a_number

    | (Not_a_number, Fraction _)   -> Not_a_number
    | (Not_a_number, Infinity)     -> Not_a_number
    | (Not_a_number, Neg_infinity) -> Not_a_number
    | (Not_a_number, Not_a_number) -> Not_a_number

let inverse f = 
    match f with
    | Fraction f' -> Fraction
        {
            numerator = f'.denominator;
            denominator = f'.numerator;
        }
    | Infinity -> of_int 0
    | Neg_infinity -> of_int 0
    | Not_a_number -> Not_a_number

let div f1 f2 = 
    mul f1 (inverse f2)

let add f1 f2 = 
    match (f1, f2) with
    | (Fraction f1', Fraction f2') ->
        let lcm = IntUtils.lcm (f1'.denominator) (f2'.denominator) in
        let f = Fraction
            {
                numerator = ((lcm / f1'.denominator) * f1'.numerator) + ((lcm / f2'.denominator) * f2'.numerator);
                denominator = lcm
            } in
        reduce f
    | (Fraction _, Infinity)       -> Infinity
    | (Fraction _, Neg_infinity)   -> Neg_infinity
    | (Fraction _, Not_a_number)   -> Not_a_number

    | (Infinity, Fraction _)       -> Infinity
    | (Infinity, Infinity)         -> Infinity
    | (Infinity, Neg_infinity)     -> Not_a_number
    | (Infinity, Not_a_number)     -> Not_a_number
    
    | (Neg_infinity, Fraction _)   -> Neg_infinity
    | (Neg_infinity, Infinity)     -> Not_a_number
    | (Neg_infinity, Neg_infinity) -> Neg_infinity
    | (Neg_infinity, Not_a_number) -> Not_a_number

    | (Not_a_number, Fraction _)   -> Not_a_number
    | (Not_a_number, Infinity)     -> Not_a_number
    | (Not_a_number, Neg_infinity) -> Not_a_number
    | (Not_a_number, Not_a_number) -> Not_a_number

let sub f1 f2 = 
    add f1 (negate f2)


(** Pretty Printer *)
    
let pretty f = 
    match f with
    | Fraction f'  ->
        let num = f'.numerator in
        let den = f'.denominator in
        
        if num = 0 then "+0"
        else if den = 1 then Printf.sprintf "%s%d" (IntUtils.sign num) (Int.abs num)
        else
            Printf.sprintf "%s%d/%d" (IntUtils.sign num) (Int.abs num) den
    | Infinity     -> "infinity"
    | Neg_infinity -> "neg_infinity"
    | Not_a_number -> "nan"

let pretty_no_plus f = 
    match f with
    | Fraction f'  ->
        let num = f'.numerator in
        let den = f'.denominator in
        
        if num = 0 then "0"
        else if den = 1 then Printf.sprintf "%s%d" (IntUtils.sign_no_plus num) (Int.abs num)
        else
            Printf.sprintf "%s%d/%d" (IntUtils.sign_no_plus num) (Int.abs num) den
    | Infinity     -> "infinity"
    | Neg_infinity -> "neg_infinity"
    | Not_a_number -> "nan"

let pretty_unsigned f = 
    match f with
    | Fraction f'  ->
        let num = f'.numerator in
        let den = f'.denominator in

        if num = 0 then "0"
        else if den = 1 then Printf.sprintf "%d" (Int.abs num)
        else
            Printf.sprintf "%d/%d" (Int.abs num) (Int.abs den)
    | Infinity     -> "infinity"
    | Neg_infinity -> "neg_infinity"
    | Not_a_number -> "nan"

let pp fmt f = Format.fprintf fmt "%s" (pretty f)
