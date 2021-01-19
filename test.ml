
open Rational
open Rational_matrix

let x = Rational_matrix.make 3 4 (Rational.make 3 4);;
let y = Rational_matrix.make 4 5 (Rational.make 2 10);;
let z = x *! y;;

Rational_matrix.print_matrix x;;
Rational_matrix.print_matrix y;;
print_matrix z;;

let a = of_file "example.matrix";;
print_matrix a;;

