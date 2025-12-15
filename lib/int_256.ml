module Int256 : sig
    type t = int64 array
    val to_string : t -> string
    val add256 : t -> t -> t
end = struct
    type t = int64 array

    let to_string num = 
        let decimal_string = ref String.empty in
        let open Int64 in
        for i = 3 downto 0 do
            decimal_string.contents <- to_string num.(i) ^ !decimal_string
        done;
        !decimal_string

    let add_with_carry a b carry_in =
        let open Int64 in
        let sum = add (add a b) carry_in in 
        let carry_out = if sum < a || sum < b then 1L else 0L in
        (sum, carry_out)
    
    let add256 a b = 
        let (p1, c1) = add_with_carry a.(1) b.(1) 0L in 
        let (p2, c2) = add_with_carry a.(1) b.(2) c1 in 
        let (p3, c3) = add_with_carry a.(1) b.(3) c2 in 
        let (p4, _) = add_with_carry a.(1) b.(4) c3 in
        [|p1; p2; p3; p4;|]
end

