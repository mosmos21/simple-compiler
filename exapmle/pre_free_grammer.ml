(* 
  S' -> S $       E -> + E E
                  E -> * E E　　
  S  -> id E      E -> id
  S  -> print E   E -> num
*)

type token =
  | ID
  | NUM
  | EQ
  | ADD
  | MUL
  | PRINT
  | EOF

exception Error of string

let tokens = ref [PRINT; ADD; NUM; NUM; EOF]

let error str = raise (Error str)

let string_of_token = function
  | ID    -> "ID"
  | NUM   -> "NUM"
  | EQ    -> "EQ"
  | ADD   -> "ADD"
  | MUL   -> "MUL"
  | PRINT -> "PRINT"
  | EOF   -> "EOF"

let get_token () =
  match !tokens with
    | [] -> exit 0
    | x :: xs -> tokens := xs; x

let tok = ref (get_token ())

let advance () = tok := get_token ()

let check t = 
  if !tok = t then advance () else error "check"

let rec s' () = s (); check(EOF)

and s () = match !tok with
  | EQ    -> check EQ; check ID; e ()
  | PRINT -> check PRINT; e ()
  | _     -> error "s ()"

and e () = match !tok with
  | ADD -> check ADD; e (); e ()
  | MUL -> check MUL; e (); e ()
  | ID  -> check ID
  | NUM -> check NUM
  | _ -> error "e ()"

let () = s' ()