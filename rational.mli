
type t

(* a/b *)
val make : int -> int -> t

(* a/b + c/d *)
val sum : t -> t -> t

(* a/b * c/d *)
val mult : t -> t -> t

(* a/b - c/d *)
val diff : t -> t -> t

(* a/b / c/d *)
val div : t -> t -> t

(* b/a *)
val inv : t -> t

(* a/b = c/d *)
val eq : t -> t -> bool

(* a/b < c/d *)
val less : t -> t -> bool

(* a/b <= c/d *)
val leq : t -> t -> bool

(* string of a/b *)
val to_string : t -> string

(* a/1 of a *)
val of_int : int -> t

val to_latex : t -> string
