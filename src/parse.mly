%{
open Support.Error
open Lambda
%}

%token <Support.Error.info> LAMBDA
%token <string Support.Error.withinfo> ID
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> DOT
%token <Support.Error.info> SEMI
%token <Support.Error.info> EOF

%start toplevel
%type <Lambda.context -> (Lambda.command list * Lambda.context) > toplevel
%%

toplevel:
  prog
   {fun ctx -> $1 ctx}
;

prog:
 EOF
   {fun ctx -> [],ctx}
| command SEMI prog
  {fun ctx ->
let cmd1,ctx = $1 ctx in
let cmds,ctx = $3 ctx in
  cmd1::cmds,ctx
  };


command :
 exp 
  {fun ctx -> let t = $1 ctx in (Lambda.Eval(Lambda.tmInfo t,t),ctx)}

exp:
AppTerm {$1}
|LAMBDA ID DOT exp
{ fun ctx ->
  let ctx1 = Lambda.addname ctx $2.v in
  Lambda.TmAbs({line=0}, $2.v, $4 ctx1)
};


AppTerm:
Aterm {$1}
|AppTerm Aterm
{fun ctx ->
let e1 = $1 ctx in
let e2 = $2 ctx in
 Lambda.TmApp((Lambda.tmInfo e1),e1, e2)
};

Aterm:
LPAREN exp RPAREN
 {$2}
| ID
{ 
fun ctx ->
match (Lambda.name2index $1.i ctx $1.v) with
  Some(index) ->
  Lambda.TmVar({line=0}, index ,  (Lambda.ctxlength ctx))
| None ->
   raise NoRuleApplies
};

(*
exp :
  appTerm {fun ctx -> $1 ctx}
| absTerm {fun ctx -> $1 ctx}
| varTerm {print_string "v" ; fun ctx -> $1 ctx}
;

absTerm : 
LAMBDA ID DOT exp 
 {
   print_string "absTerm";
  fun ctx ->
print_string "3";
  let ctx1 = Lambda.addname ctx $2.v in
  TmAbs({line=0},$2.v, ($4 ctx1))
 };

appTerm:
 absTerm Term
{
print_string "appTerm";
fun ctx ->
print_string "2";
 let e1 = $1 ctx in
 let e2 = $2 ctx in
 Lambda.TmApp((Lambda.tmInfo e1),e1, e2)
};

varTerm :
LPAREN exp RPAREN
{$2}
|ID 
{ print_string "varTerm";
fun ctx ->
print_string "1";
  match (Lambda.name2index $1.i ctx  $1.v) with
   Some(index) ->
    TmVar({line=0}, index ,  (Lambda.ctxlength ctx))
  | None ->
     raise NoRuleApplies
};
*)
