open OUnit

open Lambda

let t1 =  TmVar({line = 1}, 1, 1)
let ta1 = TmAbs({line=1},"x",t1)

let _ = run_test_tt_main begin "lambda.ml" >::: [
  "isval" >:: begin fun () ->
    assert_equal (isval [] ta1) true
  end;
  "findlist" >:: begin fun () ->
     assert_equal (pickfreshname [] "x") ([("x", 0)], "x0")
  end
] 
end