(* to compile: ocamlc -o outputname practice.ml *)
#load "str.cma";;

let s = Stack.create ()
;;

let pick e = 
  match e with
    | "+" ->  Stack.push (Stack.pop s +. Stack.pop s) s
    | "-" ->  Stack.push (Stack.pop s -. Stack.pop s) s
    | "*" -> Stack.push (Stack.pop s *. Stack.pop s) s
    | "/" -> Stack.push (Stack.pop s /. Stack.pop s) s
    | str -> try (Stack.push (float_of_string str) s) with Failure _ -> print_endline "blowed up real good"
;;

let split line = Str.split (Str.regexp " +") line 
;;

(* Basic loop for getting input until q is entered *)
let condition = ref false in
  while not !condition do
    let line = read_line () in
    if line.[0] = 'q' then
      condition := true
    else let strlist = split line in
    List.iter pick strlist ;
    print_string (string_of_float(Stack.pop s)) ; print_newline ()
  done
;;







