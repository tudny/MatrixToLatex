
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
  Matrix.of_array rats


exception Unstairable
let stair matrix prnt = 
  Printf.printf "==============Your LaTeX==============\n";
  prnt matrix;
  let (h, w) = Matrix.size matrix in
  for k = 0 to min (w - 1) (h - 1) do 
    let id = ref (-1) in
    for i = k to h - 1 do
      if Matrix.get matrix i k |> Rational.eq (Rational.of_int 0) |> not
        && !id = (-1) then 
        id := i
    done;
    if !id = -1 then raise Unstairable;
    if !id <> -1 then begin
      Matrix.mult_row (Rational.mult) matrix
        (Matrix.get matrix !id k |> Rational.inv) !id;
      for i = 0 to h - 1 do
        if i <> !id then
          Matrix.oper_rows ( fun row1 row2 ->
              let how_many = row1.(k) in
              for l = 0 to w - 1 do 
                row1.(l) <- Rational.diff row1.(l) (Rational.mult row2.(l) how_many) 
              done
            ) i !id matrix
      done;
      Matrix.swap_rows !id k matrix;
      prnt matrix
    end
  done;
  Printf.printf "==================END=================\n";