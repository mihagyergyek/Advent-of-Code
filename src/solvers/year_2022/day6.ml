open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let naloga1 data =
    let input = String.explode_string data in
    let rec najdi i = function
      | [] -> i
      | x :: y :: z :: w :: xs ->
          if List.dupExist [ x; y; z; w ] then najdi (i + 1) (y :: z :: w :: xs)
          else i
      | _ -> failwith "Ups"
    in
    najdi 4 input |> string_of_int

  let naloga2 data _part1 =
    let input = String.explode_string data in
    let rec najdi i = function
      | [] -> i
      | x :: xs ->
          if List.dupExist (List.first 14 (x :: xs)) then najdi (i + 1) xs
          else i
    in
    najdi 14 input |> string_of_int
end
