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
      let strs = 
      List.map (fun cmd -> 
          print_int (Lambda.ctxlength ctx);
          print_newline();
        match cmd with
           Lambda.Eval(i,t) -> Lambda.printnm [] (Lambda.eval [] t)
          Lambda.Eval(i,t) ->
               let _ = Lambda.typeof [] t in
               Lambda.printnm [] (Lambda.eval [] t)
         | Lambda.Bind(i,str,bind) -> str 
         )
      cmds
      in
      List.iter (fun str ->
        print_string str
      )
      strs;print_newline(); flush stdout
    done
 with Parsing.Parse_error ->
   print_string "parse error";
    exit 0
