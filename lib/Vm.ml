type address = int

type error =
  | Invalid_x_register of int
  | Uninitialized_x_register of int
  | Type_error
  | Unknown_label of Instruction.label
  | No_current_frame
  | No_continuation
  | Frame_error of Frame.error 
  | Out_of_bounds of int

type t = {
  x : Type.t option array;
  mutable ip : address;
  mutable cp : address option;
  mutable frame : Frame.t option;
  mutable reductions : int;
}

let create ?(x_registers = 1024) () =
  if x_registers < 0 || x_registers > 1024 then invalid_arg "Vm.create: x_registers must be between 0 and 1024";
  { x = Array.make x_registers None; ip = 0; cp = None; frame = None; reductions = 0 }

let instruction_pointer vm = vm.ip

let continuation vm = vm.cp

let set_continuation vm continuation = vm.cp <- continuation

let current_frame vm = vm.frame

let reductions vm = vm.reductions

let set_instruction_pointer vm address =
  if address < 0 then invalid_arg "Vm.set_instruction_pointer: address must be non-negative";
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

let advance vm =
  vm.ip <- vm.ip + 1;
  tick vm

let step (vm : t) (program : Program.program) =
  let instruction = program.code.(vm.ip) in
  match instruction with
  | Instruction.Load_int { dst; value } ->
      set_x vm dst (Type.Int value)
      |> Result.map (fun () -> advance vm)
  | Instruction.Move { dst; src } ->
      (match get_x vm src with
      | Ok (Some value) -> set_x vm dst value |> Result.map (fun () -> advance vm)
      | Ok None -> Error (Uninitialized_x_register src)
      | Error e -> Error e)
  | Instruction.Add { dst; left; right } ->
    (match get_x vm left, get_x vm right with
    | Ok (Some (Type.Int a)), Ok (Some (Type.Int b)) ->
        let answer = Int.add a b in
        set_x vm dst (Type.Int answer)
        |> Result.map (fun () -> advance vm)
    | Ok (Some _), Ok (Some _) -> Error Type_error
    | Ok None, _ -> Error (Uninitialized_x_register left)
    | _, Ok None -> Error (Uninitialized_x_register right)
    | Error e, _ -> Error e
    | _, Error e -> Error e)
  | Instruction.Label _ ->
      advance vm;
      Ok ()
  | Instruction.Jump { label } ->
    (match Hashtbl.find_opt program.labels label with
    | Some address ->
        set_instruction_pointer vm address;
        Ok ()
    | None -> Error (Unknown_label label))
  | Instruction.Call { label; locals } ->
    (match Hashtbl.find_opt program.labels label with
    | Some address ->
        push_frame vm ~locals;
        vm.cp <- Some (vm.ip + 1);
        set_instruction_pointer vm address;
        Ok ()
    | None -> Error (Unknown_label label))
  | Instruction.Ret ->
    (match vm.cp with
    | Some return_address ->
        (match pop_frame vm with
        | Error error -> Error error
        | Ok () ->
            set_instruction_pointer vm return_address;
            Ok ())
    | None -> Error No_continuation)
  | Instruction.Halt ->
      tick vm;
      Ok ()
