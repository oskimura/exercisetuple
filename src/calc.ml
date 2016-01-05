type term =
  TmTrue of int
| TmFalse of int 
| TmIf of int * term * term * term
| TmZero of int
| TmSucc of int * term
| TmPred of int * term
| TmIsZero of int * term;;

let rec isnumerical t = match t with
TmZero(_) -> true
|TmSucc(_,t1) -> isnumerical t1
| _ -> false;;


let rec isval t = match t with
  TmTrue(_) -> true
| TmFalse(_) -> true
| t when isnumerical t -> true
| _ -> false;;

exception NoRuleApplies
let dummyinfo  = 0;;

let rec eval1 t = match t with
  TmIf(_,TmTrue(_),t2,t3) -> t2
| TmIf(_,TmFalse(_),t2,t3) -> t3
| TmIf(fi,t1,t2,t3) -> let t1' = eval1 t1 in TmIf(fi, t1',t2,t3)
| TmSucc(fi,t1) -> let t1'= eval1 t1 in TmSucc(fi,t1')
| TmPred(_,TmZero(_)) -> TmZero(dummyinfo)
| TmPred(_,TmSucc(_,nv1)) when (isnumerical nv1) -> nv1
| TmPred(fi,t1) -> let t1' = eval1 t1 in TmPred(fi,t1')
| TmIsZero(_,TmZero(_)) -> TmTrue(dummyinfo)
| TmIsZero(_,TmSucc(_,nv1)) when (isnumerical nv1) -> TmFalse(dummyinfo)
| TmIsZero(fi,t1) -> let t1' = eval1 t1 in TmIsZero(fi,t1')
| _ -> raise NoRuleApplies
;;

let rec eval t =
  try let t' = eval t in eval t' with NoRuleApplies -> t;;


