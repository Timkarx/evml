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
    }
    let create_empty () = {
        stack = S.empty;
        memory = Memory.default ();
    }

    let update_stack state stack =
        state.stack <- stack; state
    
    let pop_stack state =
        let value, new_stack = S.pop state.stack in
        state.stack <- new_stack;
        value

    let push_stack state value = 
        let new_stack = S.push value state.stack in
        state.stack <- new_stack;
        state
end
