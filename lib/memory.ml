open Bytes
open Gas

module Memory : sig
    type t = bytes
    val default : unit -> t
    val access : t -> int -> int -> t
    val load : t -> int -> t
    val store : t -> int -> bytes -> t * int
    val debug_mem : t -> unit
end = struct
    type t = bytes

    let default () = create 32
    let access mem offset size = sub mem offset size
    let load mem offset = access mem offset 32
    let store mem offset value = 
        let (mem, gas_cost) = 
            (if (length mem) < offset + (length value) then
                let expansion_size = offset + (length value) - (length mem) in 
                ((extend mem 0 expansion_size), Gas.price_mem_expand expansion_size)
            else (mem, 0)) in 
            blit value 0 mem offset (length value); (mem, gas_cost)

    let debug_mem bytes =
      Bytes.iter (fun c ->
        Printf.printf "%02x " (Char.code c)
      ) bytes;
      print_newline ()
end
