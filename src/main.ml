open Parse
open Lex
open List


let _ =

  try
let lexbuf = Lexing.from_channel stdin in
let rec loop ctx = 
let result = (Parse.toplevel Lex.main lexbuf) in
flush stdout;
let cmds,_ = result ctx in
flush stdout;
let ctx'' = 
  List.fold_right(fun cmd ctx' ->
        match cmd with
          Lambda.Eval(i,t) ->
          let _ = Lambda.typeof ctx' t in
               print_string (Lambda.printnm ctx' (Lambda.eval ctx' t));
               ctx'
         | Lambda.Bind(i,x,b) ->
         let bind' = 
         (match b with
             Lambda.TmAbbBind(t,None) ->
               (Lambda.TmAbbBind(t, Some(Lambda.typeof ctx' t)))
            | Lambda.TmAbbBind(t,Some(tyT)) -> 
               let tyT' = Lambda.typeof ctx' t in
               Lambda.TmAbbBind(t,Some(tyT')) 
               then Lambda.TmAbbBind(t,Some(tyT)) *)
            | bind ->
               Lambda.print_ctx ctx';
               bind) in
           Lambda.print_ctx ctx';
           Lambda.addbinding ctx' x bind'
)
cmds
ctx
 in
print_newline();flush stdout
loop ctx''
in loop []
 with Parsing.Parse_error ->
   print_string "parse error";
    exit 0
