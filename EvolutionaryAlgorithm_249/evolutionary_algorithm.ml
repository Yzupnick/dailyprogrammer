open Printf
open Core.Std

let _ = Random.self_init()

let hamming_dist s1 s2 = 
    let add_if_not_equal a n1 n2 = if n1 = n2 then a else a + 1 in 
    let s1_list = String.to_list s1 in
    let s2_list = String.to_list s2 in
    List.fold2_exn ~f:add_if_not_equal ~init:0 s1_list s2_list 

let gen_rand_char() = 
    let rand_char =  Random.int(255 -31) in
    Char.to_string( char_of_int (rand_char + 32))

let gen_rand_string length =
    let rec gen_next count =
        match count with
        | 0 -> ""
        | n -> 
            begin
                let new_char = gen_rand_char() in
                let rest_of_string = gen_next (n - 1) in
                new_char ^ rest_of_string
            end

    in
    gen_next length

let () =
    let rand_string = gen_rand_string 15 in
    Printf.printf "%s\n" rand_string

