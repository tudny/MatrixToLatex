
open Rational

type t = { matrix : Rational.t array array; size : int * int }

let make n m a =
  { matrix = Array.make_matrix n m a; size = (n, m) }

let init n m f =
  let { matrix = arr } as matrix = make n m (f 0 0) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      arr.(i).(j) <- f i j
    done
  done;
  matrix

exception Index_out_of_matrix of int * int

exception Illegal_operation

exception Invalid_file of string

let correct (m, n) (x, y) =
  let in_range x y a =
    x <= a && x < y
  in
  if not (in_range 0 n x && in_range 0 m y) then
    raise (Index_out_of_matrix (x, y))

let get { matrix = matrix; size = size } (x, y) =
  correct size (x, y);
  matrix.(x).(y)

let set { matrix = matrix; size = size } (x, y) ele =
  correct size (x, y);
  matrix.(x).(y) <- ele

let copy { matrix = matrix; size = (n, m) } =
  init n m (fun x y -> matrix.(x).(y))

let (+!) m1 m2 =
  let { matrix = matr1; size = ((x, y) as size1) } = m1
  and { matrix = matr2; size = size2 } = m2 in
  if size1 <> size2 then raise Illegal_operation else
    init x y (fun i j -> sum (get m1 (i, j)) (get m2 (i, j)))

let ( *^ ) m1 scalar =
  let { matrix = matr1; size = ((x, y) as size1) } = m1 in
  init x y (fun i j -> mult scalar (get m1 (i, j)))

let ( *! ) m1 m2 =
  let { matrix = matr1; size = (x1, y1) } = m1
  and { matrix = matr2; size = (x2, y2) } = m2 in
  if y1 <> x2 then raise Illegal_operation else
    let matr = make x1 y2 (Rational.make 0 1) in
    for h = 0 to x1 - 1 do
      for w = 0 to y2 - 1 do
        for k = 0 to y1 - 1 do
          set matr (h, w) ( sum (get matr (h, w)) (mult matr1.(h).(k)  matr2.(k).(w)) )
        done
      done
    done;
    matr

let print_matrix ({ matrix = matr; size = (x, y) } as ma) =
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      Printf.printf "%s " (Rational.to_string (get ma (i, j)))
    done;
    Printf.printf "\n"
  done

let of_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  let rows = String.split_on_char '\n' s in
  let int_row row =
    let str = String.split_on_char ' ' row in
    List.fold_left (fun acc ele ->
        try (int_of_string ele) :: acc with e -> acc ) [] str |> List.rev
  in
  let ints = List.map (int_row) rows
             |> Array.of_list
             |> Array.map (Array.of_list) in
  let rats = Array.map (Array.map (Rational.of_int)) ints in
  let size = Array.length rats, Array.length (rats.(0)) in
 { matrix = rats; size = size }









