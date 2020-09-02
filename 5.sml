(* assuming statements on 2.sml were evaluated before with: use "2.sml"; *)

fun calculaFechada abertaEmX x v = Let(x, IConst v, abertaEmX);

run (calculaFechada (calculaFechada e2 "a" 1) "x" 2);