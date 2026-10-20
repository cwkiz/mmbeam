type label = int
type t =
  | Load_int of { dst : int; value : Int.t }
  | Move of { dst : int; src : int }
  | Add of { dst : int; left : int; right : int }
  | Label of label
  | Jump of { label : label }
  | Call of { label : label }
  | Ret
  | Halt
