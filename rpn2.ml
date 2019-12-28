type token = 
  | Num of float
  | Op of string
  | TError of string

let toToken (str: string) : token = 
      match str with
      | "+" | "-" | "*" | "/" | "^" -> Op(str)
      | _ -> let f=float_of_string_opt(str) in 
          match f with
              | Some(flt) -> Num(flt)
              | None -> TError(str)

type state =
  | MyStack of float list
  | SError of string 

let evalOp (s: string) (op1: float)  (op2: float) : float =
  match s with    
      | "+" -> op1+.op2
      | "-" -> op1-.op2
      | "*" -> op1*.op2
      | "/" -> op1/.op2
      | "^" -> op1**op2
      | _ -> 0.0

let nextState (st: state) (t: token) : state =
  match st with
  | SError(str) -> st
  | MyStack(lst) -> 
      match t with
      | Num(f) -> MyStack(f :: lst)
      | Op(s) -> (
          match lst with
              | op2::op1::tail -> MyStack( evalOp s op1 op2 :: tail)
              | _ -> SError("Not enough arguments for " ^ s)
          )
      | TError(s) -> SError("Unknown token " ^ s)

let checkState (st: state) = 
  match st with 
      | SError (str) -> str 
      | MyStack (slist) ->  
          match List.length slist with
            | 1 -> string_of_float(List.hd slist)
            (* If list has more than one element after calculation *) 
            | _ -> "Too many arguments"


let procRPN (str: string)=
  str |> 
  String.split_on_char ' ' |> 
  List.map toToken |>
  List.fold_left nextState (MyStack []) |>
  checkState

 
(* Basic loop for getting input until q is entered *)
let rec loop () =
  match read_line () with
  | "q" -> exit 0
  | str -> print_string (procRPN str ^ "\n") ; loop ()  
;;


loop () ;;    