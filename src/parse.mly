%{
open Support.Error
open Lambda
%}

%token <string Support.Error.withinfo> LAMBDA
%token <string Support.Error.withinfo> ID
%token <string Support.Error.withinfo> LPAREN
%token <string Support.Error.withinfo> RPAREN
%token <string Support.Error.withinfo> DOT
%token <string Support.Error.withinfo> SEMI

%start toplevel
%type <Lambda.context -> (Lambda.command list * Lambda.context) > toplevel
%%

toplevel:
  prog {$1}
;

prog:
  command SEMI prog
  {fun ctx ->
let cmd1,ctx = $1 ctx in
let cmds,ctx = $3 ctx in
  cmd1::cmds,ctx
  }
;

exp :
  absTerm {$1}
| appTerm {$1}
| varTerm {$1}
;

command :
 exp 
  {fun ctx -> let t = $1 ctx in (Lambda.Eval(Lambda.tmInfo t,t),ctx)}

appTerm:
 LPAREN absTerm exp RPAREN
{
fun ctx ->
 let e1 = $2 ctx in
 let e2 = $3 ctx in
 Lambda.TmApp((Lambda.tmInfo e1),e1, e2)
};


absTerm : 
 LPAREN LAMBDA ID DOT exp RPAREN
 {
  fun ctx ->
  let ctx1 = Lambda.addname ctx $3.v in
  TmAbs({line=0},$3.v, ($5 ctx1))
 };

varTerm : 
ID 
{ fun ctx ->
  match (Lambda.name2index $1.i ctx  $1.v) with
   Some(index) ->
    TmVar({line=0}, index ,  (Lambda.ctxlength ctx))
  | None ->
     raise NoRuleApplies
};
