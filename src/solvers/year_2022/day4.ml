open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let split data =
    let lst = List.lines data |> List.map String.trim in
    let rec list_of_lists acc = function
      | [] -> acc
      | x :: xs -> (
          match String.split_on_char ',' x with
          | [] -> acc
          | x :: y :: _ ->
              list_of_lists
                (String.split_on_char '-' y :: String.split_on_char '-' x :: acc)
                xs
          | _ -> acc )
    in
    list_of_lists [] lst |> List.rev

  let[@warning "-8"] primerjaj_seznama [ x1; x2 ] [ y1; y2 ] =
    match ([ x1; x2 ], [ y1; y2 ]) with
    | x1 :: x2 :: _, y1 :: y2 :: _ ->
        if int_of_string x1 > int_of_string y1 then
          if int_of_string x2 <= int_of_string y2 then 1 else 0
        else if int_of_string y1 > int_of_string x1 then
          if int_of_string y2 <= int_of_string x2 then 1 else 0
        else if int_of_string x1 = int_of_string y1 then 1
        else if int_of_string x2 = int_of_string y2 then 1
        else 0
    | _, _ -> 0

  let naloga1 data =
    let seznami = split data in
    let rec stevilo acc = function
      | [] -> acc
      | [ x1; x2 ] :: [ y1; y2 ] :: xs ->
          stevilo (acc + primerjaj_seznama [ x1; x2 ] [ y1; y2 ]) xs
      | _ -> acc
    in
    stevilo 0 seznami |> string_of_int

  let naloga2 data _part1 =
    let seznami = split data in
    let rec stevilo acc = function
      | [] -> acc
      | [ x1; x2 ] :: [ y1; y2 ] :: xs ->
          if
            min (int_of_string x2) (int_of_string y2)
            - max (int_of_string x1) (int_of_string y1)
            >= 0
          then stevilo (acc + 1) xs
          else stevilo acc xs
      | _ -> acc
    in
    stevilo 0 seznami |> string_of_int
end
