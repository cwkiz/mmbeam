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
