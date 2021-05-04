(* 
let y1 = Stair.of_file "matrix1.in";;
let a = Stair.of_file "matrix2.in";;
let x2 = Matrix.transpose @@ Stair.of_file "matrix3.in";;

let rational_mult = Matrix.mult (Rational.sum) (Rational.mult) (Rational.of_int 0);;
let print = Matrix.print_matrix (Rational.to_string);;

print y1;;
print a;;
print x2;;

let print_size m =
  let (x, y) = Matrix.size m in 
  Printf.printf "%d %d\n" x y
;;
(* 
print_size y1;;
print_size a;;
print_size x2;; *)

let t1 =  rational_mult y1 a;;
let t2 = rational_mult t1 x2;;

Matrix.print_matrix (Rational.to_string) t2;; *)

let rational_mult = Matrix.mult (Rational.sum) (Rational.mult) (Rational.of_int 0);;
let print = Matrix.print_matrix (Rational.to_string);;

let m = Stair.of_file "example.matrix";;

let d = Stair.stair m (Matrix.to_latex (Rational.to_latex));;

