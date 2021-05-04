
type t = { num : int ; don : int }

let rec _gcd a b =
  if b = 0 then a
  else _gcd b (a mod b)
;;

let normalize { num = num; don = don } =
  if num = 0 then { num = 0; don = 1} else
  let sign = (num / abs num) * (don / abs don) in
  let num = abs num
  and don = abs don in
  let g = _gcd num don in
  { num = sign * num / g; don = don / g }
;;

let make a b =
  if b = 0 then raise (Invalid_argument "denominator mustn't be 0")
  else normalize { num = a ; don = b }
;;

let sum { num = num1; don = don1 } { num = num2; don = don2 } =
  let num = num1 * don2 + num2 * don1
  and don = don1 * don2 in
  normalize { num = num; don = don}
;;

let mult { num = num1; don = don1 } { num = num2; don = don2 } =
  normalize { num = num1 * num2; don = don1 * don2 }
;;

let diff a b =
  sum a (mult b (make (-1) 1))
;;

let div m { num = num; don = don } =
  mult m (make don num)
;;

let inv { num = num; don = don } = 
  make don num

let eq { num = num1; don = don1 } { num = num2; don = don2 } =
  num1 = num2 && don1 = don2
;;

let less { num = num1; don = don1 } { num = num2; don = don2 } =
    num1 * don2 < num2 * don1
;;

let leq a b =
  (eq a b) || (less a b)
;;

let to_string { num = num; don = don } =
  "(" ^ (string_of_int num) ^
  (if don = 1 then "" else (" / " ^ (string_of_int don)))
  ^ ")"
;;

let of_int a =
  make a 1
;;

let to_latex { num = num; don = don } =
  if don = 1 then string_of_int num
  else "\\frac{" ^ string_of_int num ^ "}{" ^ string_of_int don ^ "}"
;;

