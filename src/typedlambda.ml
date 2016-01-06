let (|>) x f = f x
let (|-) f g = fun x -> g (f x)

let getOrElse defVal = function
  | Some(v) -> v
  | None    -> defVal


type ty = TyArr of ty * ty
| TyBool
| TyWrong of string

type binding = NameBind
| VarBind of ty

type term =
  TmVar of int * int * int
| TmAbs of int * string * ty * term
| TmApp of int * term * term
| TmTrue of int
| TmFalse of int
| TmIf of int * term * term * term


let bindding ctx x bind = (x,bind) :: ctx

let getbinding fi ctx n =
  try 
    Some(List.nth ctx n |> snd)
  with _ ->
    None;;

let rec ctxlength ctx =
  List.length ctx;;

let rec index2name fi ctx n =
  if n < ctxlength ctx then
  let name = List.nth ctx n |> fst in Some(name)
 else
   None;;

let getTypeFromContext fi ctx i = 
match (getbinding fi ctx i) with 
  Some(VarBind(tyT)) -> tyT
| _ -> TyWrong ""
 
let addbinding ctx x bind = (x,bind) :: ctx

let rec typeof ctx t = 
match t with
  TmVar(fi,i,_) -> getTypeFromContext fi ctx i
| TmAbs(fi,x,tyT1,t2) ->
   let ctx' = addbinding ctx x (VarBind(tyT1)) in
   let tyT2 = typeof ctx' t2 in
   TyArr(tyT1,tyT2)
| TmApp(fi,t1,t2) ->
   let tyT1 = typeof ctx t1 in
   let tyT2 = typeof ctx t2 in
   (match tyT1 with
     TyArr(tyT11,tyT12) ->
       if (=) tyT2 tyT11 then tyT12 else TyWrong ""
    | _ -> TyWrong "")
| TmTrue(_) ->
   TyBool
| TmFalse(_) ->
   TyBool
| TmIf(fi,t1,t2,t3) ->
  if (=) (typeof ctx t1) TyBool then
   let tyT2 = typeof ctx t2 in
   if (=) tyT2 (typeof ctx t3) then tyT2
   else TyWrong ""
  else TyWrong ""
