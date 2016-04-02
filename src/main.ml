open Parse
open Lex
open List


let _ =

  try
let lexbuf = Lexing.from_channel stdin in
    while true do
let result = (Parse.toplevel Lex.main lexbuf) in
flush stdout;
let cmds,ctx = result [] in
flush stdout;
let _ = 
      List.fold_right(fun cmd ctx' ->
        match cmd with
          Lambda.Eval(i,t) ->
          let _ = Lambda.typeof ctx' t in
               print_string (Lambda.printnm ctx' (Lambda.eval ctx' t));
               ctx'
         | Lambda.Bind(i,x,b) ->
         (match b with 
          Lambda.TmAbbBind(t,ty) ->
             let t' = Lambda.eval ctx' t in
                 Lambda.addbinding ctx' x (Lambda.TmAbbBind(t',ty))
          | bind ->
          Lambda.addbinding ctx' x bind)
)
cmds
ctx
 in
print_newline();flush stdout
    done
 with Parsing.Parse_error ->
   print_string "parse error";
    exit 0
