
exception Index_out_of_matrix of int * int
exception Illegal_operation of string

type 'a t = { matrix : 'a array array; size : int * int }

let make n m a = 
  { matrix = Array.make_matrix n m a; size = (n, m) }

let init n m f = 
  let { matrix = arr } as matrix = make n m (f 0 0) in
  for y = 0 to n - 1 do 
    for x = 0 to m - 1 do 
      arr.(y).(x) <- f y x 
    done
  done;
  matrix

let size { size = size } = size 

let correct (m, n) (x, y) =
  let in_range x y a = 
    x <= a && a < y
  in 
  if not (in_range 0 m x && in_range 0 n y) then
    raise (Index_out_of_matrix (x, y))

let get { matrix = matrix; size = size } y x = 
  correct size (y, x);
  matrix.(y).(x)

let set { matrix = matrix; size = size } y x ele =
  correct size (y, x);
  matrix.(y).(x) <- ele

let copy { matrix = matrix; size = (n, m) } = 
  init n m (fun y x -> matrix.(y).(x))

let __operation _operation ma1 ma2 = 
  let { matrix = matr1; size = (n1, m1) } = ma1
  and { matrix = matr2; size = (n2, m2) } = ma2 in
  if (n1, m1) <> (n2, m2) then raise (Illegal_operation "Bad sizes") else
  init n1 m1 (fun y x ->
    _operation (get ma1 y x) (get ma2 y x))

let add = __operation 
  
let mult_scalar _operation { matrix = matrix; size = (n, m) } scal = 
    init n m (fun y x -> _operation scal matrix.(y).(x))

let diff = __operation

let mult _add _mult _neutral ma1 ma2 = 
  let { matrix = matr1; size = (n1, m1) } = ma1
  and { matrix = matr2; size = (n2, m2) } = ma2 in
  if m1 <> n2 then raise (Illegal_operation "Bad dimensions!") else
  init n1 m2 (fun y x ->
      let sum = ref _neutral in 
      for k = 0 to m1 - 1 do 
        sum := _add !sum (_mult (get ma1 y k) (get ma2 k x))
      done;
      !sum)

let print_matrix to_string ({ matrix = matr; size = (n, m) } as ma) =
  print_string "=============\n";
  for y = 0 to n - 1 do
    for x = 0 to m - 1 do
      Printf.printf "%s " (to_string (get ma y x))
    done;
    Printf.printf "\n"
  done;
  print_string "=============\n"

let of_array arr = 
  let n = Array.length arr
  and m = Array.length arr.(0) in 
  { matrix = arr; size = (n, m) }

let swap_rows a b ma = 
  let { matrix = matrix; size = (h, _) } = ma in 
  if a < 0 || a >= h || b < 0 || b >= h then raise (Index_out_of_matrix (a, b))
  else 
    let tmp = matrix.(a) in 
    matrix.(a) <- matrix.(b);
    matrix.(b) <- tmp

let mult_row _mult ma scal id = 
  let { matrix = matrix; size = (h, _) } = ma in 
  if id < 0 || id >= h  then raise (Index_out_of_matrix (id, -1))
  else 
    matrix.(id) <- Array.map (_mult scal) matrix.(id)

let oper_rows f a b ma = 
  let { matrix = matrix; size = (h, w) } = ma in 
  if a < 0 || a >= h || b < 0 || b >= h then raise (Index_out_of_matrix (a, b))
  else 
    f matrix.(a) matrix.(b)

let to_latex to_latex ({ matrix = matr; size = (n, m) } as ma) =
  print_string "\\begin{bmatrix}\n";
  for y = 0 to n - 1 do
    for x = 0 to m - 1 do
      Printf.printf "%s" (to_latex (get ma y x));
      if x <> m - 1 then Printf.printf " & "
    done;
    Printf.printf " \\\\\n"
  done;
  print_string "\\end{bmatrix}\n"

let transpose ({ matrix = matr; size = (n, m) } as ma) = 
  init m n (fun x y -> get ma y x)