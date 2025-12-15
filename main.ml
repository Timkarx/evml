open Lib.Memory

let () = 
    print_endline "Beggining program";
    let mem = Memory.default () in
    Memory.debug_mem mem
