module Operations (State : sig
    type t
    val pop_stack : t -> int
    val push_stack : t -> int -> t
end) = struct
    exception InvalidOpcode
    type opcode = 
      | STOP of (State.t -> State.t)
      | ADD of (State.t -> State.t)
      | MUL of (State.t -> State.t)
      | SUB of (State.t -> State.t)
      | DIV of (State.t -> State.t)

    let maths operator state =
      let a = State.pop_stack state in
      let b = State.pop_stack state in
      State.push_stack state (operator a b)

    let decode_opcode byte =
      match byte with
      | '\x00' -> STOP (maths ( + ))
      | '\x01' -> ADD (maths ( + ))
      | '\x02' -> MUL (maths ( * ))
      | '\x03' -> SUB (maths ( - ))
      | '\x04' -> DIV (maths ( / ))
      | _ -> raise InvalidOpcode
end
