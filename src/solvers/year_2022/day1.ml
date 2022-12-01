open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lst = List.lines data |> List.map String.trim in
    let rec list_of_lists (ostali, acc) = function
      | [] -> ostali
      | x :: xs ->
          if String.length x < 1 then list_of_lists (acc :: ostali, 0) xs
          else list_of_lists (ostali, acc + int_of_string x) xs
    in
    list_of_lists ([], 0) lst |> List.maximum |> string_of_int

  let sez data =
    let lst = List.lines data |> List.map String.trim in
    let rec list_of_lists (ostali, acc) = function
      | [] -> ostali
      | x :: xs ->
          if String.length x < 1 then list_of_lists (acc :: ostali, 0) xs
          else list_of_lists (ostali, acc + int_of_string x) xs
    in
    list_of_lists ([], 0) lst

  let naloga2 data _part1 =
    let palcki = sez data in
    let lst = List.rev (List.sort compare palcki) in
    match lst with x :: y :: z :: _ -> string_of_int (x + y + z) | _ -> "Ne"
end
