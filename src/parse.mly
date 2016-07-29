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
%token <Support.Error.info> COLON
%token <Support.Error.info> BOOL
%token <Support.Error.info> ARROW
%token <Support.Error.info> UNIT
%token <Support.Error.info> STRING
%token <Support.Error.info> FLOAT
%token <Support.Error.info> INT
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> EQ
%token <Support.Error.info> LCURLY
%token <Support.Error.info> RCURLY
%token <Support.Error.info> COMMA
%token <Support.Error.info> AS
%token <Support.Error.info> LT
%token <Support.Error.info> GT
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> DDARROW
%token <Support.Error.info> VBAR
%token <Support.Error.info> LETREC
%token <string Support.Error.withinfo> STRINGV
%token <float Support.Error.withinfo> FLOATV
%token <int Support.Error.withinfo> INTV

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
|ID TyBinder
{
    fun ctx ->
       ((Bind({line=0}, $1.v, $2 ctx)), Lambda.addname ctx $1.v)
}
|ID Binder
{
 fun ctx -> ((Bind({line=0}, $1.v, $2 ctx)), Lambda.addname ctx $1.v)
}
Binder:
COLON Type
{
fun ctx -> VarBind($2 ctx)
}
| EQ exp
{
fun ctx -> TmAbbBind($2 ctx, None)
}

TyBinder :
   {fun ctx -> Lambda.TyVarBind}
| EQ Type
   {fun ctx -> TyAbbBind($2 ctx)}

exp:
LET ID EQ exp IN exp
{ 
TmLet({line=0}, $2.v,($4 (addname ctx $2.v)),($6 (addname ctx $2.v)))}
|AppTerm {$1}
|LAMBDA ID COLON Type DOT exp
{ 
fun ctx ->
let ctx1 = Lambda.addname ctx $2.v in
TmAbs({line=0},$2.v, $4 ctx, ($6 ctx1))
};
| CASE exp OF Cases
{
  fun ctx ->
    Lambda.TmCase({line=0}, $2 ctx, $4 ctx)
}
| LETREC ID COLON Type EQ exp IN exp
{
  fun ctx ->
    let ctx' = Lambda.addname ctx $2.v in
    Lambda.TmLet({line=0}, $2.v,
               TmFix({line=0}, TmAbs({line=0},$2.v, ($4 ctx), ($6 ctx'))),
               ($8 ctx'))
}
;


Type: 
ArrowType {$1}

AType:
LPAREN Type RPAREN
{$2}
|BOOL
{fun ctx -> Lambda.TyBool}
|UNIT
{fun ctx -> Lambda.TyUnit}
|STRING
{fun ctx -> Lambda.TyString}
|FLOAT
{fun ctx -> Lambda.TyFloat}
|STRING
{fun ctx -> Lambda.TyString}
|INT
{fun ctx -> Lambda.TyInt}
|FLOAT
{fun ctx -> Lambda.TyFloat}
| LT FieldTypes GT
   {
     fun ctx ->
        Lambda.TyVariant($2 ctx 1)
   }
| ID
{
  fun ctx ->
 match (Lambda.name2index $1.i ctx $1.v) with
  Some(i) ->
print_string "i: "; print_int i;
    Lambda.TyVar(i, Lambda.ctxlength ctx)
  |None ->
   Lambda.TyId($1.v)
}

FieldTypes :
    { fun ctx i -> [] }
  | NEFieldTypes
     { $1 }

NEFieldTypes :
  FieldType
    { fun ctx i -> [$1 ctx i] }
  | FieldType COMMA NEFieldTypes
    { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType :
   ID COLON Type
    { fun ctx i -> ($1.v, $3 ctx) }
  | Type
    { fun ctx i -> (string_of_int i, $1 ctx) }

ArrowType:
AType ARROW ArrowType
{fun ctx -> TyArr($1 ctx, $3 ctx)}
| AType  {print_string "atype";$1};

AppTerm:
PathTerm {$1}
|Aterm {$1}
|AppTerm Aterm
{
print_string "b";print_newline();
fun ctx ->
let e1 = $1 ctx in
let e2 = $2 ctx in
 Lambda.TmApp((Lambda.tmInfo e1),e1, e2)
};

Aterm:
LPAREN exp RPAREN
 {$2}
| LCURLY Fields RCURLY
{
 fun ctx ->
 let fields = $2 ctx 1 in
 Lambda.TmRecord({line=0}, fields)
}
| ID
{ 
fun ctx ->
match (Lambda.name2index $1.i ctx $1.v) with
  Some(index) ->
  Lambda.TmVar({line=0}, index ,  (Lambda.ctxlength ctx))
| None ->
   raise NoRuleApplies
};
}
| LT ID EQ exp GT AS Type
 {
  fun ctx ->
    TmTag({line=0}, $2.v, $4 ctx, $7 ctx)
 }
| STRINGV
{fun ctx -> Lambda.TmString({line=0},$1.v)}
| INTV
{ fun ctx -> Lambda.TmInt({line=0}, $1.v)}
| FLOATV
{fun ctx -> Lambda.TmFloat({line=0}, $1.v)}
;

Cases :
Case
{
  fun ctx ->
  [$1 ctx]
}
| Case VBAR Cases
{
  fun ctx ->
    ($1 ctx) :: ($3 ctx)
}
Case:
LT ID EQ ID GT DDARROW exp
{
  fun ctx ->
    let ctx1 = Lambda.addname ctx $4.v in
    ($2.v, ($4.v, $7 ctx1))
}
Fields:
{
 fun ctx i -> []
}
| NEFields
{ $1 }

NEFields:
Field
{fun ctx i -> [$1 ctx i] }
|Field COMMA NEFields
{fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1))}

Field:
ID EQ exp
{
fun ctx i ->
 print_string "field " ;
 print_string $1.v ;
($1.v, $3 ctx) 
}
PathTerm:
PathTerm DOT ID
{
fun ctx ->
  TmProj({line=0},$1 ctx,$3.v)
}
|AscribeTerm { $1 }
|Aterm { $1 }
AscribeTerm: 
Aterm AS Type
{
fun ctx -> Lambda.TmAscribe({line=0}, $1 ctx, $3 ctx)
}
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
