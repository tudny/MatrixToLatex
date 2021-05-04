
exception Index_out_of_matrix of int * int
exception Illegal_operation of string

type 'a t

val make : int -> int -> 'a -> 'a t

val init : int -> int -> (int -> int -> 'a) -> 'a t

val size : 'a t -> (int * int)

val get : 'a t -> int -> int -> 'a

val set : 'a t -> int -> int -> 'a -> unit

val copy : 'a t -> 'a t

val add : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val mult_scalar : ('a -> 'a -> 'a) -> 'a t -> 'a -> 'a t

val mult : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t -> 'a t

val print_matrix : ('a -> string) -> 'a t -> unit

val of_array : 'a array array -> 'a t

val swap_rows : int -> int -> 'a t -> unit

val mult_row : ('a -> 'a -> 'a) -> 'a t -> 'a -> int -> unit

val oper_rows : ('a array -> 'a array -> unit) -> int -> int -> 'a t -> unit

val to_latex : ('a -> string) -> 'a t -> unit

val transpose : 'a t -> 'a t