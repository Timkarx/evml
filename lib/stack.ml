open Printf

module Stack : sig
    type t

    val empty : t
    val push : int -> t -> t
    val pop : t -> (int * t)
    val repr : t -> unit
end = struct
    exception UnderflowError
    type t = int list
    
    let empty = []
    let push value stack = value :: stack
    let pop stack = match stack with
            | value :: tl -> (value, tl)
            | [] -> raise UnderflowError
    let repr stack = 
        print_endline("--TOP--");
        List.iter (printf "%d\n") stack;
        print_endline("--BOTTOM--")
end
