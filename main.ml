(* Problem 01 *)
let rec last (xs: 'a list): 'a option =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest

(* Problem 02 *)
let rec last_two (xs: 'a list): ('a * 'a) option =
    match xs with
    | [] -> None
    | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: rest -> last_two rest

(* Problem 03 *)
let rec at (k: int) (xs: 'a list): 'a option =
    match xs with
    | x :: _    when k == 1 -> Some x
    | _ :: rest when k > 1 -> at (k-1) rest
    | _ -> None

(* Problem 04 *)
let rec length (xs: 'a list): int =
    match xs with
    | [] -> 0
    | _ :: rest -> 1 + length rest

(* Problem 04 (tail recursive) *)
let length_tail (xs: 'a list): int =
    let rec length_tail' (xs: 'a list) (acc: int): int =
        match xs with
        | [] -> acc
        | _ :: rest -> length_tail' rest (acc + 1)
    in
    length_tail' xs 0

(* Problem 04 (imperative) *)
let best_length (xs: 'a list): int =
    let acc = ref 0 in
    let ys = ref xs in
    while !ys != [] do
        acc := !acc + 1;
        ys := List.tl !ys;
    done;
    !acc

(* Problem 05 *)
let rev (xs: 'a list): 'a list =
    let rec rev' (xs: 'a list) (result: 'a list): 'a list =
        match xs with
        | [] -> result
        | x :: rest -> rev' rest (x :: result)
    in
    rev' xs []

(* Problem 06 *)
let is_palindrome (xs: 'a list): bool =
    rev xs = xs

(* Problem 08 *)
let rec compress (xs: string list): string list =
    match xs with
    | x :: (y :: _ as rest) -> if x = y then compress rest else x :: compress rest
    | smaller -> smaller

(* Problem 09 *)
let pack (xs: string list): string list list =
    let rec pack_impl (xs: string list) (acc: string list): string list list =
        match xs, acc with
        | [], _ -> [acc]
        | x :: rest, y :: _ -> if x = y then pack_impl rest (x :: acc)
                               else acc :: pack_impl rest [x]
        | x :: rest, _ -> pack_impl rest [x]
    in
    pack_impl xs []

let () =
    print_endline "Hello, world!"
