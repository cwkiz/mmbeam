type address = int

type error =
  | Invalid_x_register of int
  | No_current_frame
  | Frame_error of Frame.error

type t = {
  x : Type.t option array;
  mutable ip : address;
  mutable cp : address option;
  mutable frame : Frame.t option;
  mutable reductions : int;
}

let create ?(x_registers = 1024) () =
  if x_registers < 0 || x_registers > 1024 then invalid_arg "1024 > X > 0";
  { x = Array.make x_registers None; ip = 0; cp = None; frame = None; reductions = 0 }

let instruction_pointer vm = vm.ip

let continuation vm = vm.cp

let set_continuation vm continuation = vm.cp <- continuation

let current_frame vm = vm.frame

let reductions vm = vm.reductions

let set_instruction_pointer vm address =
  if address < 0 then invalid_arg "address must be non-negative";
  vm.ip <- address

let get_x vm index =
  if index < 0 || index >= Array.length vm.x then Error (Invalid_x_register index)
  else Ok vm.x.(index)

let set_x vm index value =
  if index < 0 || index >= Array.length vm.x then Error (Invalid_x_register index)
  else (
    vm.x.(index) <- Some value;
    Ok ())

let push_frame vm ~locals =
  vm.frame <- Some (Frame.create ?prev:vm.frame ?saved_cp:vm.cp ~locals ())

let pop_frame vm =
  match vm.frame with
  | None -> Error No_current_frame
  | Some frame ->
      vm.frame <- Frame.previous frame;
      vm.cp <- Frame.saved_continuation frame;
      Ok ()

let get_y vm index =
  match vm.frame with
  | None -> Error No_current_frame
  | Some frame -> Result.map_error (fun error -> Frame_error error) (Frame.get_local frame index)

let set_y vm index value =
  match vm.frame with
  | None -> Error No_current_frame
  | Some frame -> Result.map_error (fun error -> Frame_error error) (Frame.set_local frame index value)

let tick vm = vm.reductions <- vm.reductions + 1
