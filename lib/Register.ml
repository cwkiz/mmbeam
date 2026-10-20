type bank = [ `X | `Y | `FR ]

type t = {
  bank : bank;
  index : int;
}

let make bank index =
  if index < 0 then None
  else
    match bank with
    | `X when index < 1024 -> Some { bank; index }
    | `Y -> Some { bank; index }
    | `FR when index < 16 -> Some { bank; index }
    | _ -> None

let bank register = register.bank

let index register = register.index

