module Operations (State : sig
    type t
    val pop_stack : t -> int
    val push_stack : t -> int -> t
    val increment_pc : t -> t
    val halt_execution : t -> t
end) = struct
    exception InvalidOpcode
    type 'a opcode = 
      | STOP of 'a
      | ADD of 'a
      | MUL of 'a
      | SUB of 'a
      | DIV of 'a

    let maths operator state =
      let a = State.pop_stack state in
      let b = State.pop_stack state in
      State.increment_pc (State.push_stack state (operator a b))

    let decode_opcode byte =
      match byte with
      | '\x00' -> STOP (State.halt_execution)
      | '\x01' -> ADD (maths ( + ))
      | '\x02' -> MUL (maths ( * ))
      | '\x03' -> SUB (maths ( - ))
      | '\x04' -> DIV (maths ( / ))
      | _ -> raise InvalidOpcode
end
