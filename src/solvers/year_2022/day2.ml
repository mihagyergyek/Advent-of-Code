open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.map String.trim (List.lines data) in
    let rec tocke acc = function
      | [] -> acc
      | x :: xs -> (
          match x with
          | "A X" -> tocke (acc + 4) xs
          | "A Y" -> tocke (acc + 8) xs
          | "A Z" -> tocke (acc + 3) xs
          | "B X" -> tocke (acc + 1) xs
          | "B Y" -> tocke (acc + 5) xs
          | "B Z" -> tocke (acc + 9) xs
          | "C X" -> tocke (acc + 7) xs
          | "C Y" -> tocke (acc + 2) xs
          | "C Z" -> tocke (acc + 6) xs
          | _ -> tocke acc [] )
    in
    tocke 0 lines |> string_of_int

  let naloga2 data _part1 =
    let lines = List.map String.trim (List.lines data) in
    let rec tocke acc = function
      | [] -> acc
      | x :: xs -> (
          match x with
          | "A X" -> tocke (acc + 3) xs
          | "A Y" -> tocke (acc + 4) xs
          | "A Z" -> tocke (acc + 8) xs
          | "B X" -> tocke (acc + 1) xs
          | "B Y" -> tocke (acc + 5) xs
          | "B Z" -> tocke (acc + 9) xs
          | "C X" -> tocke (acc + 2) xs
          | "C Y" -> tocke (acc + 6) xs
          | "C Z" -> tocke (acc + 7) xs
          | _ -> tocke acc [] )
    in
    tocke 0 lines |> string_of_int
end
