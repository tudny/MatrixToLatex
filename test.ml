(* wczytanie macierzy [m] z pliku [example.matrix] *)
let m = Stair.of_file "example.matrix";;

(* zeschodkowanie macierzy [m] oraz wypisanie kroków w postaci LaTeX'a *)
let d = Stair.stair m (Matrix.to_latex (Rational.to_latex));;

(* zeschodkowanie macierzy [m] oraz wypisanie kroków w postaci czytelnej *)
let d = Stair.stair m (Matrix.print_matrix (Rational.to_string));;
