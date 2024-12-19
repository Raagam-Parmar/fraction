let signum n = Int.compare n 0

let sign n =  
    if n < 0 then "-"
    else if n > 0 then "+"
    else ""

let sign_no_plus n = 
    if n < 0 then "-" else ""

    
let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)

let gcd_all l = List.fold_left gcd (List.hd l) l


let lcm a b = 
    (a * b ) / gcd a b

let lcm_all l = (List.fold_left ( * ) 1 l) / gcd_all l