type closure = {
  code_ptr : int;
  env : t array;
}

and t =
  | Atom of string
  | Int of Int.t
  | Nil
  | Cons of t * t
  | Tuple of t array
  | Pid of int
  | Closure of closure

let int n = Int (Int.of_int n)

let big_int n = Int (Int.of_z n)
