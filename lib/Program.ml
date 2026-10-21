type address = int

type program = {
  code : Instruction.t array;
  labels : (Instruction.label, address) Hashtbl.t;
}

let load code =
  let labels = Hashtbl.create 16 in
  Array.iteri (fun i instruction ->
    match instruction with
    | Instruction.Label label -> Hashtbl.add labels label i
    | _ -> ()
  ) code;
  { code; labels }
