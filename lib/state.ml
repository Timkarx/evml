open Memory

module type VMStack = sig
    type t
    
    val empty : t
    val push : int -> t -> t
    val pop : t -> (int * t)
    val repr : t -> unit
end

module MakeState (S: VMStack) = struct
    type t = { 
        mutable stack : S.t;
        mutable memory : Memory.t;
        mutable running : bool;
        mutable pc : int;
    }
    let create_empty () = {
        stack = S.empty;
        memory = Memory.default ();
        running = true;
        pc = 0;
    }

    let halt_execution state =
        state.running <- false;
        state

    let increment_pc state =
        state.pc <- state.pc + 1;
        state

    let pop_stack state =
        let value, new_stack = S.pop state.stack in
        state.stack <- new_stack;
        value

    let push_stack state value = 
        let new_stack = S.push value state.stack in
        state.stack <- new_stack;
        state
end
