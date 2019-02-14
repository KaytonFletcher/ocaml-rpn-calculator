(* to compile: ocamlc -o outputname practice.ml *)
#load "str.cma";;

let s = Stack.create ()
;;

let catch msg = print_string msg ; ()
;;

let handle msg = print_string msg ; Stack.clear s ; ()
;;

let pick e =  
try
  match e with
    | "+" ->  Stack.push (Stack.pop s +. Stack.pop s) s
    | "-" ->  Stack.push (Stack.pop s -. Stack.pop s) s
    | "*" -> Stack.push (Stack.pop s *. Stack.pop s) s
    | "/" -> Stack.push (Stack.pop s /. Stack.pop s) s
    | "^" -> Stack.push (Stack.pop s ** Stack.pop s) s
    | str -> (Stack.push (float_of_string str) s) ;
  ;
  if (classify_float(Stack.top s) == FP_infinite || classify_float(Stack.top s) == FP_nan) then (handle "Can't divide by zero!\n")
  ;
with 
| Failure _ -> (catch "Not a number!\n")
| Stack.Empty -> (catch  "Bad expression\n")

;;

let rpn line =  
      List.iter pick (Str.split (Str.regexp " +") line)  ;
      if (Stack.length s = 1) then String.concat "" [string_of_float(Stack.pop s) ; "\n"]
      else if (not (Stack.is_empty s)) then (Stack.clear s ; "Bad expression\n")
      else "" (* Must return string -> This case occurs when stack is empty *)
;;


(* Basic loop for getting input until q is entered *)
let rec loop () =
    let line = read_line () in
    if line.[0] != 'q' then
      print_string (rpn line) ; loop ()
;;

loop()


