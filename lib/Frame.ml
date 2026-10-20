type address = int

type error = Invalid_local_index of int

type t = {
  ys : Type.t option array;
  prev : t option;
  saved_cp : address option;
}

let create ?prev ?saved_cp ~locals () =
  if locals < 0 then invalid_arg "locals must be non-negative";
  { ys = Array.make locals None; prev; saved_cp }

let previous frame = frame.prev

let saved_continuation frame = frame.saved_cp

let get_local frame index =
  if index < 0 || index >= Array.length frame.ys then Error (Invalid_local_index index)
  else Ok frame.ys.(index)

let set_local frame index value =
  if index < 0 || index >= Array.length frame.ys then Error (Invalid_local_index index)
  else (
    frame.ys.(index) <- Some value;
    Ok ())
