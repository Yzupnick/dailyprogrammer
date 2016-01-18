(* Full problem description: https://www.reddit.com/r/dailyprogrammer/comments/40h9pd/20160111_challenge_249_easy_playing_the_stock *)
(* 
    Let's assume I'm playing the stock market - buy low, sell high. 
    I'm a day trader, so I want to get in and out of a stock before the day is done, and I want to time my trades so that I make the biggest gain possible.
    The market has a rule that won't let me buy and sell in a pair of ticks - I have to wait for at least one tick to go buy. And obviously I can't buy in the future and sell in the past.
    So, given a list of stock price ticks for the day, can you tell me what trades I should make to maximize my gain within the constraints of the market?
    Remember - buy low, sell high, and you can't sell before you buy.
*)

open Printf

(* Function to generate all valid stock purchases and how much would be made on the sale *)
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
