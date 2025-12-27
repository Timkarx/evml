open Lib

(* 1. Instantiate the modules using the Functors *)
(* We assume Lib exports a module named 'Stack' that satisfies VMStack *)
module EvmState = State.MakeState(Stack)
module EvmOpcodes = Opcodes.Operations(EvmState)

(* 2. Define the Virtual Machine Execution Loop *)
let run_vm (state: EvmState.t) bytecode =
  let len = String.length bytecode in

  Printf.printf "Starting Execution. Bytecode length: %d\n" len;

  while state.running && state.pc < len do
    let byte = bytecode.[state.pc] in

    try
      (* Decode the current byte into an Opcode variant *)
      match EvmOpcodes.decode_opcode byte with
      | STOP f | ADD f | MUL f | SUB f | DIV f -> 
            ignore (f state)

    with
    | EvmOpcodes.InvalidOpcode ->
        Printf.printf "Error: Invalid opcode at position %d\n" (state.pc - 1);
        ignore (EvmState.halt_execution state)
    | Failure msg -> 
        (* Catches "Stack Underflow" from the Stack module *)
        Printf.printf "Error: %s\n" msg;
        ignore (EvmState.halt_execution state)
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
  let code = "\x01\x01\x00" in

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
