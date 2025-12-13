open Lib.Stack

let () = 
    print_endline "Beggining program";
    let x = Stack.push 10 Stack.empty in 
    let x = Stack.push 8 x in 
    let x = Stack.push 29 x in 
    Stack.repr x
