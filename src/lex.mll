{
open Support.Error
open Parse

let reservedWords = [
("lambda" , fun i -> Parse.LAMBDA i);
("." , fun i -> Parse.DOT i);
(";" , fun i -> Parse.SEMI i);
( "(", fun i -> Parse.LPAREN i);
( ")", fun i -> Parse.RPAREN i);
]

let text = Lexing.lexeme
let info lexbuf =
  FI("", 0, 0)
}

rule main = parse 
 ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z''_' '0'-'9']*
 { Parse.ID{v = (text lexbuf); i = info lexbuf}  }

