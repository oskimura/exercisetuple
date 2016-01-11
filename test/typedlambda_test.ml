open OUnit

open Typedlambda

let tt = TmTrue(0);;
let tf = TmFalse(0);;
let tif = TmIf(0,tt,tt,tf);;
let tabs = TmAbs(0,"x",TyBool,tt);;
let tapp0 = TmApp(0,tabs,tf);;

let _ = run_test_tt_main begin "lambda.ml" >::: [
  "tif" >:: begin fun () ->
    assert_equal (typeof [] tif) TyBool
  end;
  "tapp" >:: begin fun () ->
    assert_equal (typeof [] tapp0) TyBool
  end
] 
end
