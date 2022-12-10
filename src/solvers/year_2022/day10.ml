open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let naloga1 data =
    let input = List.lines data |> List.map (String.split_on_char ' ') in
    let rec execute limit (cycle, acc) = function
      | [] -> cycle * acc
      | x :: xs -> (
          match x with
          | "noop" :: _ ->
              if cycle + 1 = limit then execute limit (cycle + 1, acc) []
              else execute limit (cycle + 1, acc) xs
          | "addx" :: num :: _ ->
              if cycle + 1 = limit then execute limit (cycle + 1, acc) []
              else if cycle + 2 = limit then execute limit (cycle + 2, acc) []
              else execute limit (cycle + 2, acc + int_of_string num) xs
          | _ -> failwith "format" )
    in
    execute 20 (0, 1) input
    + execute 60 (0, 1) input
    + execute 100 (0, 1) input
    + execute 140 (0, 1) input
    + execute 180 (0, 1) input
    + execute 220 (0, 1) input
    |> string_of_int

  let between cikel x =
    if cikel >= x then if cikel <= x + 2 then true else false else false

  let insert_every n chr str =
    let lst = str |> String.explode_string |> List.chunkify n in
    let rec aux chr acc = function
      | [] -> acc
      | x :: xs -> aux chr (acc @ x @ [ chr ]) xs
    in
    aux chr [] lst |> List.implode_char_list

  let naloga2 data _part1 =
    let input = List.lines data |> List.map (String.split_on_char ' ') in
    let rec print (cycle, acc) out = function
      | [] -> out
      | x :: xs -> (
          match x with
          | "noop" :: _ ->
              if between (cycle + 1) acc then
                print ((cycle + 1) mod 40, acc) (out ^ "#") xs
              else print ((cycle + 1) mod 40, acc) (out ^ ".") xs
          | "addx" :: num :: _ ->
              if between (cycle + 1) acc then
                if between (cycle + 2) acc then
                  print
                    ((cycle + 2) mod 40, acc + int_of_string num)
                    (out ^ "##") xs
                else
                  print
                    ((cycle + 2) mod 40, acc + int_of_string num)
                    (out ^ "#.") xs
              else if between (cycle + 2) acc then
                print
                  ((cycle + 2) mod 40, acc + int_of_string num)
                  (out ^ ".#") xs
              else
                print
                  ((cycle + 2) mod 40, acc + int_of_string num)
                  (out ^ "..") xs
          | _ -> out )
    in
    print (0, 1) "" input |> insert_every 40 '\n'
end
