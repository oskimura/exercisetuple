(* open hashtbl;; *)
open List;;

type info = {line : int}

let getOrElse defVal = function
  | Some(v) -> v
  | None    -> defVal

type term = TmVar of info * int * int
 | TmAbs of info * string * term
 | TmApp of info * term * term

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
    Some (a,b) -> ((replacelist (fun (c,d) -> a = c) (a,(b+1)) ctx),(a ^string_of_int(b+1)))
  | None -> (((x,0)::ctx),(x ^ "0")) ;;

let rec ctxlength ctx =
  List.length ctx;;

let rec index2name fi ctx n =
  if n < ctxlength ctx then
  let name = List.nth ctx n |> fst in Some(name)
 else
   None;;

      
let rec printnm ctx t = match t with 
  TmAbs (fi,x,t1) ->
  let (ctx',x') = pickfreshname ctx x in
   "(lambda" ^ x' ^  ". " ^ (printnm ctx' t1) ^  ")"
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


let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi,x,n) -> if x=j+c then termSift c s else TmVar(fi,x,n)
  | TmAbs(fi,x,t1) -> TmAbs(fi,x, walk (c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1, walk c t2)
  in walk 0 t;;

let termSubstTop s t =
  termSift (-1) (termSubst 0 (termSift 1 s) t);;

let rec isval ctx t = match t with
  TmAbs(_,_,_) -> true
 | _ -> false;;


let rec eval1 ctx t = match t with
  TmApp(fi,TmAbs(_,x,t12),v2) when isval ctx v2 ->
   termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
   let t2' = eval1 ctx t2 in TmApp(fi,v1,t2')
  | TmApp(fi,t1,t2) ->
    let t1' = eval1 ctx t1 in
    TmApp(fi,t1',t2)
  | _ ->
    raise NotRulesApplies;;
