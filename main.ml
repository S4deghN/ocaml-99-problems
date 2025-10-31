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

(* Problem 07 *)
type 'a node =
    | One of 'a
    | Many of 'a node list

let flatten (xs: 'a node list): 'a list =
    let rec flatten_impl (xs: 'a node list) (result: 'a list): 'a list =
        match xs with
        | [] -> result
        | One x :: xs -> flatten_impl xs (x :: result)
        | Many x :: xs -> flatten_impl xs (flatten_impl x result)
    in
    rev (flatten_impl xs [])


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

let rec filter (f: 'a -> bool) (xs: 'a list): 'a list =
    match xs with
    | [] -> []
    | x :: rest -> if f x then x :: filter f rest
                   else filter f rest
;;


(* Problem 10 *)
let encode (xs: 'a list): (int * 'a) list =
    let rec encode_impl (xs: 'a list) (count: int): (int * 'a) list =
        match xs with
        | x :: (y :: _ as rest) -> if x = y then encode_impl rest (count+1)
                                   else (count, x) :: encode_impl rest 1
        | [x] -> (count, x) :: encode_impl [] count
        | [] -> []
    in
    encode_impl xs 1

(* Problem 11 *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode2 (xs: 'a list): 'a rle list =
    let rec encode_impl (xs: 'a list) (count: int): 'a rle list =
        match xs with
        | x :: (y :: _ as rest) -> if x = y then encode_impl rest (count+1)
                                   else if count > 1 then Many (count, x) :: encode_impl rest 1
                                   else One x :: encode_impl rest 1
        | [x] -> if count > 1 then Many (count, x) :: encode_impl [] count
                 else One x :: encode_impl [] count
        | [] -> []
    in
    encode_impl xs 1

let encode3 (xs: 'a list): 'a rle list =
    let close (x: 'a) (count: int): 'a rle =
        if count > 1 then Many (count, x)
        else One x
    in
    let rec encode_impl (xs: 'a list) (count: int): 'a rle list =
        match xs with
        | [] -> []
        | [x] -> close x (count+1) :: encode_impl [] 0
        | x :: (y :: _ as rest) -> if x = y then encode_impl rest (count+1)
                       else close x (count+1) :: encode_impl rest 0
    in
    encode_impl xs 0

(* Problem 12 *)
let rec decode (xs: 'a rle list): 'a list =
    let rec expand acc n x =
        if n > 0 then expand (x :: acc) (n-1) x
        else acc
    in
    match xs with
    | [] -> []
    | One x :: rest -> x :: decode rest
    | Many (count, x) :: rest -> expand [] count x @ decode rest

(* Problem 13 *)
let duplicate (xs: 'a list): 'a list =
    let rec dup acc = function
        | [] -> acc
        | x :: t -> dup (x :: x :: acc) t
    in
    rev (dup [] xs)

(* Problem 14 *)
let replicate (xs: 'a list) (n: int): 'a list =
    let rec expand acc x n =
        if n > 0 then  expand (x :: acc) x (n-1)
        else acc
    in
    let rec rep acc xs n =
        match xs with
        | [] -> acc
        | x :: t -> rep ((expand [] x n) @ acc) t n
    in
    rev (rep [] xs n)

(* Problem 15 *)
let drop (xs: 'a list) (n'th: int): 'a list =
    let rec drp acc cnt xs =
        match xs with
        | [] -> acc
        | x :: rest -> if (cnt+1) = n'th then drp acc 0 rest
                       else  drp (x :: acc) (cnt+1) rest
    in
    rev (drp [] 0 xs)

(* Problem 16 *)
let split (xs: 'a list) (i: int): 'a list * 'a list =
    let rec aux ys xs i =
        match xs with
        | x :: rest -> if i > 0 then aux (x :: ys) rest (i-1)
                       else rev ys, xs
        | _ -> rev ys, xs
    in
    aux [] xs i


(* Problem 17 *)
let slice xs i k =
    match split xs i with
    | first, second -> match split second (k-i) with
            | first, second -> first

(* Problem 18 *)
let rotate xs n = 
    match split xs n with
    | first, second -> second @ first

(* Problem 19 *)
let remove_at i xs =
    let rec aux i xs acc =
        match xs with 
        | x :: rest -> if i > 0 then aux (i-1) rest (x :: acc)
                       else rev acc @ rest
        | _ -> rev acc @ xs
    in
    if i < 0 then xs
    else aux i xs []


(* Problem 20 *)
let rec insert_at elem i xs =
    match xs with
    | x :: rest -> if i > 0 then x :: insert_at elem (i-1) rest
                   else elem :: xs
    | _ -> [elem]

(* Problem 21 *)
let range a b =
    let rec aux acc a b =
        if a <= b then aux (a :: acc) (a+1) b
        else acc
    in
    if b > a then rev (aux [] a b)
    else aux [] b a

(* Problem 22 *)
let pop xs i =
    let rec aux acc xs i =
        match xs with
            | x :: t -> if i > 0 then aux (x :: acc) t (i-1)
                                 else Some x, (rev acc) @ t
            | _ -> None, xs
    in
    aux [] xs i

let random_select xs n =
    let random_index xs =
        Random.int (List.length xs)
    in
    let rec aux acc xs n =
        if n > 0 then match pop xs (random_index xs) with
            | Some x, rest -> aux (x :: acc) rest (n-1)
            | _ -> raise Not_found
        else acc
    in
    (* Random.init 0; *)
    aux [] xs n

(* Problem 23 *)
let lotto_select n m: int list =
    random_select (range 1 m) n

(* Problem 24 *)
let permutation xs =
    let rec aux acc xs =
        if List.length xs > 0 then
            match pop xs (Random.int (List.length xs)) with
            | Some x, rest -> aux (x :: acc) rest
            | _ -> acc
        else acc
    in
    aux [] xs

let () =
    print_endline "Hello, world!"
