(* open hashtbl;; *)
open List;;

type info = {line : int}

type ty = TyArr of ty * ty
| TyBool
| TyUnit
| TyWrong of string
| TyString
| TyFloat
| TyInt

type bind = NameBind
| VarBind of ty

type context = (string * bind) list

let print_ctx ctx =
  List.iter (fun (str,_) -> print_string str) ctx

exception NoRuleApplies
exception NotFound

let (|>) x f = f x
let (|-) f g = fun x -> g (f x)
let getOrElse defVal = function
  | Some(v) -> v
  | None    -> defVal

(* type term = TmVar of info * int * int (\* 最後の値は de bruijn index*\) *)
(*  | TmAbs of info * string * term *)
(*  | TmApp of info * term * term *)


type term =
  TmVar of info * int * int
| TmAbs of info * string * ty * term
| TmApp of info * term * term
| TmTrue of info
| TmFalse of info
| TmIf of info * term * term * term
| TmUnit of info
| TmString of info * string 
| TmFloat of info * float
| TmInt of info * int
| TmLet of info * string * term * term

type command =
  Bind of info * string * bind
| Eval of info * term

let rec findlist f list = 
  match list with
    [] -> None
  | (x :: xs) ->
     if (f x) then
       Some x
     else
       findlist f xs;;

let rec replacelist f x list =
  match list with
    [] -> [x]
  | (y :: ys) ->
     if (f y) then
       x :: ys
     else
       y :: (replacelist f x ys);;


let pickfreshname ctx x =
  match findlist (fun y -> x = (fst y)) ctx  with
    Some (a,b) ->
    (* ((replacelist (fun (c,d) -> a = c) *)
    (*               (a^"'",b) *)
    (*               ctx), *)
    (*  (a ^ "'")) *)
    ((a^ "'",NameBind)::ctx, a^ "'")
  | None ->
     (((x,NameBind)::ctx),x) ;;

let rec ctxlength ctx =
  List.length ctx;;

let rec index2name fi ctx n =
  if n < ctxlength ctx then
  let name = List.nth ctx n |> fst in Some(name)
 else
   None;;

let rec name2index fi ctx x =
 let rec walk fi ctx x n =
   match ctx with
    [] -> None
  | ((y,_)::ys) ->
   if y = x then
     Some(n)
   else
     walk fi ys  x (n+1)
  in
   walk fi ctx x 0
;;


let emptycontext = []
let addbinding ctx x bind = (x,bind)::ctx
let addname ctx x =
addbinding ctx x NameBind

let rec getbinding fi ctx i =
   let (_,bind) = List.nth  ctx i in bind


let getTypeFromContext fi ctx i =
  match getbinding fi ctx i with
    VarBind(ty) -> Some (ty)
  | _ -> None;;

let tmInfo t =
match t with
  TmVar(fi,_,_) -> fi
| TmAbs(fi,_,_) -> fi
| TmAbs(fi,_,_,_) -> fi
| TmApp(fi,_,_) -> fi

| TmUnit(fi) -> fi
| TmFalse(fi) -> fi
| TmTrue(fi) -> fi
| TmIf(fi,_,_,_) -> fi
| TmString(fi,_) -> fi
| TmFloat(fi,_) -> fi
| TmLet(fi,_,_,_) -> fi
| TmInt(fi,_) -> fi

let rec print_type ty =
match ty with
|TyUnit -> "unit"
|TyBool -> "bool"
|TyArr(ty1,ty2) -> print_type ty1 ^ " -> " ^ print_type ty2
| _ -> "error"
 
let rec printnm ctx t = match t with
  TmAbs (fi,x,ty,t1) ->
let (ctx',x') = pickfreshname ctx x in
   "(lambda " ^ x' ^  (print_type ty) ^ " . " ^ (printnm ctx' t1) ^  ")" 
 | TmApp(fi,t1,t2) ->
  "(" ^ printnm ctx t1 ^ " " ^ printnm ctx t1 ^ ")"
 | TmVar (fi, x, n) ->
   if ctxlength ctx = n then
     index2name fi ctx  x |> getOrElse "[]"
   else
     "bad idx" ;;

let termSift d t =
  let rec walk c t = match t with
   TmVar (fi,x,n) -> 
     if x >= c then TmVar(fi,x+d,n+d)
     else TmVar(fi,x,n+d)
   | TmAbs(fi,x,t1) -> TmAbs(fi,x,walk(c+1) t1)
   | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1, walk c t2)
  in walk 0 t;;
     "bad indx"
| TmUnit(fi) -> "unit"
| TmTrue(fi) -> "true"
| TmFalse(fi) -> "false"
| TmIf(fi,t1,t2,t3) ->
"if " ^ (printnm ctx t1) ^ 
" then " ^ (printnm ctx t2) ^ 
" else " ^ (printnm ctx t3);;

| TmString(fi,str) -> str
| TmFloat(fi,f) -> string_of_float f
| TmLet(fi,v,t1,t2) ->
let ctx1 = addname ctx v in
"let " ^  v ^ " = " ^(printnm ctx t1) ^ " in " ^ (printnm ctx1 t2)
| TmInt(fi,n) -> string_of_int n
;;

let tmmap onvar c t =
let rec walk c t = match t with
TmVar(fi,x,n) -> onvar fi c x n
| TmAbs(fi,x,ty,t1) -> TmAbs(fi,x,ty,walk(c+1)t1)
| TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
| TmTrue(fi) as t -> t
| TmFalse(fi) as t -> t
| TmIf(fi,t1,t2,t3) ->TmIf(fi,walk c t1,walk c t2,walk c t3)
| TmUnit(fi) as t -> t
| TmString(fi,str) as t -> t
| TmFloat(fi,f) as t -> t
| TmLet(fi,v,t1,t2) -> TmLet(fi,v, walk c t1, walk c t2)
| TmInt(fi,n) as t -> t
in walk c t

(* let termSift d t = *)
(*   let rec walk c t = match t with *)
(*    TmVar (fi,x,n) ->  *)
(*      if x >= c then TmVar(fi,x+d,n+d) *)
(*      else TmVar(fi,x,n+d) *)
(*    | TmAbs(fi,x,ty,t1) -> TmAbs(fi,x,ty,walk(c+1) t1) *)
(*    | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1, walk c t2) *)
(*   in walk 0 t;; *)

let termShiftAbove d c t =
  tmmap
    (fun fi c x n ->
     if x>=c
     then TmVar(fi,x+d,n+d)
     else TmVar(fi,x,n+d))
    c t

let termShift d t = termShiftAbove d 0 t


(* let termSubst j s t = *)
(*   let rec walk c t = match t with *)
(*     TmVar(fi,x,n) -> if x=j+c then termShift c s else TmVar(fi,x,n) *)
(*   | TmAbs(fi,x,ty,t1) -> TmAbs(fi,x, ty, walk (c+1) t1) *)
(*   | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1, walk c t2) *)
(*   in walk 0 t;; *)

let termSubst j s t =
  tmmap
    (fun fi j x n ->
     if x=j
     then termShift j s
     else TmVar(fi,x,n))
    j t


let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t);;

let rec isval ctx t = match t with
  TmAbs(_,_,_) -> true
  TmAbs(_,_,_,_) -> true
 | TmString(_,_) -> true
 | TmFloat(_,_) -> true
 | TmInt(_,_) -> true
 | TmTrue(_) -> true
 | TmFalse(_) -> true
 | TmUnit(_) -> true
 | _ -> false;;


let rec eval1 ctx t =
print_string "eval1\n";
match t with
  TmApp(fi,TmAbs(_,x,_,t12),v2) when isval ctx v2 ->
   termSubstTop v2 t12
| TmApp(fi,v1,t2) when isval ctx v1 ->
   let t2' = eval1 ctx t2 in TmApp(fi,v1,t2')
| TmApp(fi,t1,t2) ->
    let t1' = eval1 ctx t1 in
    TmApp(fi,t1',t2)
  | _ ->
    raise NoRuleApplies;;
| TmLet(_,_,t1,v) when isval ctx v ->
 termSubstTop v t1

| TmLet(fi,x,v,t2) when isval ctx v ->
   termSubstTop v t2
| TmLet(fi,x,t1,t2) ->
 let t1' = eval1 ctx t1 in
 TmLet(fi,x,t1',t2)
| TmIf(fi,TmTrue(_),t2,t3) ->
  t2
| TmIf(fi,TmFalse(_),t2,t3) ->
  t3
let rec eval ctx t =
try let t' = eval1 ctx t
in  eval ctx t' 
with NoRuleApplies -> t


let rec typeof ctx t = 
match t with
(*
  x:T ∈ Γ
-----------
 Γ |- x:T
*)
(*
  Γ,x:T1 |- t2 :T2
-------------------------
  Γ |- λ x : T1 . t2 : T1 -> T2
*)
 TmAbs(fi,x,tyT1,t2) ->
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
| TmVar(fi,i,_) -> (match (getTypeFromContext fi  ctx i) with
  Some(v) ->v
  | _ -> raise NotFound)
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
| TmUnit(_) -> TyUnit
| TmString(_,_) -> TyString
| TmFloat(_,_) -> TyFloat
| TmLet(fi,v,t1,t2) ->
let ctx' = addbinding ctx v (VarBind (typeof ctx t1)) in
(typeof ctx' t2)
| TmInt(_,_) -> TyInt

(*   *)
let t1 = TmVar({line=1},0,1)
let ta1 = TmAbs({line=1},"x",t1)
let ta2 = TmApp({line=1},ta1,t1)
(* let t1 = TmVar({line=1},0,1) *)
(* let ta1 = TmAbs({line=1},"x",t1) *)
(* let ta2 = TmApp({line=1},ta1,t1) *)

