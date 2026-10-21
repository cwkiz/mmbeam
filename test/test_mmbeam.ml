let expect_some_int expected = function
  | Ok (Some (Mmbeam.Type.Int (Mmbeam.Int.Small actual))) when actual = expected -> ()
  | _ -> failwith "unexpected VM value"

let () =
  let vm = Mmbeam.Vm.create () in
  (match Mmbeam.Vm.set_x vm 0 (Mmbeam.Type.int 41) with
  | Ok () -> ()
  | Error _ -> failwith "X(0) should exist");
  expect_some_int 41 (Mmbeam.Vm.get_x vm 0);
  Mmbeam.Vm.push_frame vm ~locals:1;
  (match Mmbeam.Vm.set_y vm 0 (Mmbeam.Type.int 1) with
  | Ok () -> ()
  | Error _ -> failwith "Y(0) should exist in the new frame");
  expect_some_int 1 (Mmbeam.Vm.get_y vm 0);
  (match Mmbeam.Vm.pop_frame vm with
  | Ok () -> ()
  | Error _ -> failwith "the frame should be removable");
  match Mmbeam.Vm.get_y vm 0 with
  | Error Mmbeam.Vm.No_current_frame -> ()
  | _ -> failwith "the popped frame must no longer be accessible"


let run vm program =
  match Mmbeam.Vm.step vm program with
  | Ok () -> ()
  | Error _ -> failwith "instruction failed"

let () =
  let vm = Mmbeam.Vm.create () in
  let program =
    Mmbeam.Program.load
      [|
        Mmbeam.Instruction.Load_int {
          dst = 0;
          value = Mmbeam.Int.of_int 40;
        };
        Mmbeam.Instruction.Load_int {
          dst = 1;
          value = Mmbeam.Int.of_int 2;
        };
        Mmbeam.Instruction.Add {
          dst = 2;
          left = 0;
          right = 1;
        };
        Mmbeam.Instruction.Halt;
      |]
  in

  run vm program;
  run vm program;
  run vm program;
  run vm program;

  match Mmbeam.Vm.get_x vm 2 with
  | Ok (Some (Mmbeam.Type.Int (Mmbeam.Int.Small 42))) -> ()
  | _ -> failwith "expected X(2) = 42"

let () =
  let vm = Mmbeam.Vm.create () in
  let program =
    Mmbeam.Program.load
      [|
        Mmbeam.Instruction.Jump { label = 7 };
        Mmbeam.Instruction.Load_int {
          dst = 0;
          value = Mmbeam.Int.of_int 0;
        };
        Mmbeam.Instruction.Label 7;
        Mmbeam.Instruction.Load_int {
          dst = 0;
          value = Mmbeam.Int.of_int 42;
        };
        Mmbeam.Instruction.Halt;
      |]
  in
  run vm program;
  if Mmbeam.Vm.instruction_pointer vm <> 2 then
    failwith "jump should target the label's array index";
  run vm program;
  run vm program;
  match Mmbeam.Vm.get_x vm 0 with
  | Ok (Some (Mmbeam.Type.Int (Mmbeam.Int.Small 42))) -> ()
  | _ -> failwith "expected jump to execute the instruction after the label"

let () =
  let vm = Mmbeam.Vm.create () in
  let program =
    Mmbeam.Program.load
      [|
        Mmbeam.Instruction.Call { label = 10; locals = 1 };
        Mmbeam.Instruction.Halt;
        Mmbeam.Instruction.Label 10;
        Mmbeam.Instruction.Call { label = 20; locals = 2 };
        Mmbeam.Instruction.Ret;
        Mmbeam.Instruction.Label 20;
        Mmbeam.Instruction.Ret;
      |]
  in
  run vm program;
  run vm program;
  (match Mmbeam.Vm.set_y vm 0 (Mmbeam.Type.int 10) with
  | Ok () -> ()
  | Error _ -> failwith "outer call frame should have Y(0)");
  run vm program;
  (match Mmbeam.Vm.get_y vm 0 with
  | Ok None -> ()
  | _ -> failwith "nested call should have a fresh local frame");
  run vm program;
  (match Mmbeam.Vm.set_y vm 0 (Mmbeam.Type.int 20) with
  | Ok () -> ()
  | Error _ -> failwith "inner call frame should have Y(0)");
  run vm program;
  expect_some_int 10 (Mmbeam.Vm.get_y vm 0);
  if Mmbeam.Vm.instruction_pointer vm <> 4 then
    failwith "inner return should resume in the outer call";
  run vm program;
  if Mmbeam.Vm.instruction_pointer vm <> 1 then
    failwith "outer return should resume after the outer call";
  (match Mmbeam.Vm.current_frame vm with
  | None -> ()
  | Some _ -> failwith "all call frames should be popped after returning");
  (match Mmbeam.Vm.continuation vm with
  | None -> ()
  | Some _ -> failwith "continuation should be restored after returning");
  let no_continuation_vm = Mmbeam.Vm.create () in
  let ret_program = Mmbeam.Program.load [| Mmbeam.Instruction.Ret |] in
  (match Mmbeam.Vm.step no_continuation_vm ret_program with
  | Error Mmbeam.Vm.No_continuation -> ()
  | _ -> failwith "Ret without an active call should fail")