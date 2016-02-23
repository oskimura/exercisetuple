{
open Support.Error
open Parse

let reservedWords = [
("lambda" , fun i -> Parse.LAMBDA i);
("." , fun i -> Parse.DOT i);
(";" , fun i -> Parse.SEMI i);
( "(", fun i -> Parse.LPAREN i);
( ")", fun i -> Parse.RPAREN i);
(":", fun i -> Parse.COLON i);
("bool", fun i -> print_string "bool" ;Parse.BOOL i);
("->", fun i -> Parse.ARROW i);
("unit", fun i -> Parse.UNIT i);

]

type buildfun = info -> Parse.token
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    print_string "error";
    Parse.ID {i=i;v=str}

let text = Lexing.lexeme
let info lexbuf =
  FI("", 0, 0)
}

rule main = parse 
 ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '_' '0'-'9']*
  { createID(info lexbuf) (text lexbuf) }
| "->"
   { createID(info lexbuf) (text lexbuf) }
| ['.' ';' '(' ')' ':' ]
   { createID(info lexbuf) (text lexbuf) }
| eof { Parse.EOF(info lexbuf) }
| [' ' '\009' '\012' ]+{ main lexbuf }
| ['\n'] {Parse.EOF(info lexbuf) }
