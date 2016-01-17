open Printf

let rec generate_diffs accum items  = 
    let difference tick1 tick2 = (tick2 -. tick1, tick1, tick2) in
    match items with
        | [] -> accum
        | [fst] -> accum
        | [fst;second] -> accum
        | fst::sec::tl -> 
                let partial_diff = difference fst in 
                begin 
                    let differences = List.map partial_diff tl in
                    let new_accum =  accum @ differences in
                    let new_items =sec::tl in 
                    generate_diffs new_accum new_items 
                end


let print_difference difference = 
    let (_, tick1, tick2) = difference in
    Printf.printf "%f, %f \n" tick1 tick2

let larger_diff a b = 
    let (a_diff, _, _) = a in
    let (b_diff, _, _) = b in
    if a_diff >= b_diff then a else b

let rec max_differences max differences = match differences with
    | [] ->  max
    | [difference] -> larger_diff max difference
    | hd::tail -> 
            begin
                let new_max = larger_diff max hd in 
                max_differences new_max tail 
            end

let () = 
    let args_list = Array.to_list Sys.argv in 
    let program_args = match args_list with
        | [] -> []
        | [first] -> []
        | frst::tail -> tail
    in 
    let differences = generate_diffs [] in
    let curried_max_differences = max_differences (min_float, 0.0, 0.0) in 
    List.map float_of_string program_args
        |> differences
        |> curried_max_differences 
        |> print_difference
