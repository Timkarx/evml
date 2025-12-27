open Lib

(* 1. Instantiate the modules using the Functors *)
(* We assume Lib exports a module named 'Stack' that satisfies VMStack *)
module EvmState = State.MakeState(Stack)
module EvmOpcodes = Opcodes.Operations(EvmState)

(* 2. Define the Virtual Machine Execution Loop *)
let run_vm state bytecode =
  let pc = ref 0 in
  let running = ref true in
  let len = String.length bytecode in

  Printf.printf "Starting Execution. Bytecode length: %d\n" len;

  while !running && !pc < len do
    let byte = bytecode.[!pc] in
    incr pc;

    try
      (* Decode the current byte into an Opcode variant *)
      match EvmOpcodes.decode_opcode byte with
      | EvmOpcodes.STOP _ -> 
          (* Handle STOP: We do NOT execute the function inside it 
             (because in your definition it does an ADD), we just break the loop. *)
          print_endline "Opcode: STOP -> Halting";
          running := false

      (* For arithmetic, we extract the function 'f' and apply it to 'state' *)
      | EvmOpcodes.ADD f -> 
          print_endline "Opcode: ADD"; 
          ignore (f state)
      | EvmOpcodes.MUL f -> 
          print_endline "Opcode: MUL"; 
          ignore (f state)
      | EvmOpcodes.SUB f -> 
          print_endline "Opcode: SUB"; 
          ignore (f state)
      | EvmOpcodes.DIV f -> 
          print_endline "Opcode: DIV"; 
          ignore (f state)

    with
    | EvmOpcodes.InvalidOpcode ->
        Printf.printf "Error: Invalid opcode at position %d\n" (!pc - 1);
        running := false
    | Failure msg -> 
        (* Catches "Stack Underflow" from the Stack module *)
        Printf.printf "Error: %s\n" msg;
        running := false
  done

(* 3. The Main Entry Point *)
let () = 
  (* A. Initialize empty state *)
  let state = EvmState.create_empty () in

  (* B. Pre-populate the stack manually 
     Goal: Calculate (10 + 20) * 2 
     
     Stack logic (assuming Last-In-First-Out):
     1. Push 2
     2. Push 20
     3. Push 10
     
     Execution:
     1. ADD pops 10 & 20 -> Pushes 30.  Stack is now [30; 2]
     2. MUL pops 30 & 2  -> Pushes 60.  Stack is now [60]
  *)
  print_endline "--- Pre-populating Stack ---";
  ignore (EvmState.push_stack state 2);
  ignore (EvmState.push_stack state 20);
  ignore (EvmState.push_stack state 10);

  (* C. Define the Bytecode 
     \x01 = ADD
     \x02 = MUL
     \x00 = STOP
  *)
  let code = "\x01\x02\x00" in

  (* D. Run *)
  print_endline "--- Running Bytecode ---";
  run_vm state code;

  (* E. Check Result *)
  print_endline "--- Checking Result ---";
  try
    let result = EvmState.pop_stack state in
    Printf.printf "Final Result: %d\n" result
  with Failure _ ->
    print_endline "Stack was empty!"
