(* Problem 01 *)
let rec last (xs: 'a list): 'a option =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest

let () = print_endline "Hello, world!"
