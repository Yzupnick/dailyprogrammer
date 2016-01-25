open Core.Std
open Printf

let get_count_of_digits num =
    let string_num = Int.to_string num in
    let count_digit counts digit = 
        let digit = String.of_char digit in
        let count =
            match Map.find counts digit with
            | None -> 0
            | Some x -> x
        in
        Map.add counts ~key:digit ~data:(count + 1)
    in
    String.fold ~f:count_digit ~init:String.Map.empty string_num

let get_description_count num =
    let string_num = Int.to_string num in
    let count_digit index counts count = 
        let digit = Int.to_string index in
        let count_int = int_of_string( String.of_char count) in
        (digit, count_int) :: counts
    in
    String.foldi ~f:count_digit ~init:[] string_num

let description_match desc counts = 
    let descript_in_counts (digit, descript_count) = 
        let actual_count  = 
            match Map.find counts digit with
            | None -> 0
            | Some x -> x
        in
        actual_count = descript_count
    in
    List.for_all ~f:descript_in_counts desc

let is_self_descr num =
    let count_of_chars = get_count_of_digits num in
    let description = get_description_count num in
    description_match description count_of_chars


let rec range x y =
    if y = x then [x] else 
        begin
            let new_y = y -1 in
            let tail = range x new_y in
            y :: tail
        end


let () = 
    range 10000 99999 
        |> List.filter ~f:is_self_descr
        |> List.iter ~f:(Printf.printf "%d\n") 


