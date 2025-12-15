module Gas : sig
    val price_mem_expand : int -> int
end = struct
    let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n -> a * pow a (n - 1)
    let price_mem_expand int = pow int 2
end
