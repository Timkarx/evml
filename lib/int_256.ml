module Int256 : sig
    type t
    val add256 : t -> t -> t
    val sub256 : t -> t -> t
    val mul256 : t -> t -> t
    val div256 : t -> t -> t
end = struct
    type t = {
        p1: int64;
        p2: int64;
        p3: int64;
        p4: int64;
    }
    let add_with_carry a b carry_in =
        let open Int64 in
        let sum = add (add a b) carry_in in 
        let carry_out = if sum < a || sum < b then 1L else 0L in
        (sum, carry_out)
    
    let add256 a b = 
        let (p1, c1) = add_with_carry a.p1 b.p1 0L in 
        let (p2, c2) = add_with_carry a.p2 b.p2 c1 in 
        let (p3, c3) = add_with_carry a.p3 b.p3 c2 in 
        let (p4, c4) = add_with_carry a.p4 b.p4 c3 in
        { p1; p2; p3; p4; }
end
