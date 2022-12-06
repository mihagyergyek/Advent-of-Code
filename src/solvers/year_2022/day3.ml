open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let char_to_int chr =
    let abeceda =
      [
        "a";
        "b";
        "c";
        "d";
        "e";
        "f";
        "g";
        "h";
        "i";
        "j";
        "k";
        "l";
        "m";
        "n";
        "o";
        "p";
        "q";
        "r";
        "s";
        "t";
        "u";
        "v";
        "w";
        "x";
        "y";
        "z";
        "A";
        "B";
        "C";
        "D";
        "E";
        "F";
        "G";
        "H";
        "I";
        "J";
        "K";
        "L";
        "M";
        "N";
        "O";
        "P";
        "Q";
        "R";
        "S";
        "T";
        "U";
        "V";
        "W";
        "X";
        "Y";
        "Z";
      ]
    in
    List.index_of chr abeceda + 1

  let split_in_halves str =
    let dolzina_pol = String.length str / 2 in
    let sez_crk = String.explode_string str in
    let rec razdeli (prva_pol, druga_pol, acc) = function
      | [] -> (prva_pol, druga_pol)
      | x :: xs ->
          if acc <= dolzina_pol then
            razdeli (String.make 1 x :: prva_pol, druga_pol, acc + 1) xs
          else razdeli (prva_pol, String.make 1 x :: druga_pol, acc + 1) xs
    in
    razdeli ([], [], 1) sez_crk

  let rec ponovljeni (list1, list2) =
    match list1 with
    | [] -> 0
    | x :: xs ->
        if List.mem x list2 then char_to_int x else ponovljeni (xs, list2)

  let split_3 (str1, str2, str3) =
    let list1 = String.explode_string str1 in
    let list2 = String.explode_string str2 in
    let list3 = String.explode_string str3 in
    ( List.char_list_to_string_list list1,
      List.char_list_to_string_list list2,
      List.char_list_to_string_list list3 )

  let rec match3 (list1, list2, list3) =
    match list1 with
    | [] -> 0
    | x :: xs ->
        if List.mem x list2 then
          if List.mem x list3 then char_to_int x else match3 (xs, list2, list3)
        else match3 (xs, list2, list3)

  let naloga1 data =
    let lines = List.map String.trim (List.lines data) in
    let rec vsota acc = function
      | [] -> acc
      | x :: xs -> vsota (acc + (x |> split_in_halves |> ponovljeni)) xs
    in
    vsota 0 lines |> string_of_int

  let naloga2 data _part1 =
    let lines = List.map String.trim (List.lines data) in
    let rec vsota acc = function
      | [] -> acc
      | x :: y :: z :: xs -> vsota (acc + ((x, y, z) |> split_3 |> match3)) xs
      | _ -> acc
    in
    vsota 0 lines |> string_of_int
end
