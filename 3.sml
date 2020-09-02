datatype expr =
         IConst of int
         | Prim2 of string * expr * expr
         | Var of string
         | Let of string * expr * expr;

exception FreeVar;

fun lookup [] id = raise FreeVar
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

fun eval (e:expr) (st: (string * expr) list) : int =
    case e of
        (IConst i) => i
      | (Var v) => eval (lookup st v) st
      | (Prim2(f, e1, e2)) =>
        let
            val v1 = (eval e1 st);
            val v2 = (eval e2 st) in
        case f of
            ("+") => v1 + v2
          | ("-") => v1 - v2
          | ("*") => v1 * v2
          | ("/") => v1 div v2
          | ("%") => v1 mod v2
          | _ => raise Match
        end
      (* The change made is below. e1 evaluation is delayed to when it is used. *)
      | (Let(x, e1, e2)) => eval e2 ((x, e1)::st);

fun isIn x s =
    case s of
        [] => false
      | (h::t) => if x = h then true else isIn x t;

fun union s1 s2 =
    case s1 of
        [] => s2
      | (h::t) => if isIn h s2 then union t s2 else union t (h::s2);

fun diff s1 s2 =
    case s1 of
        [] => []
      | (h::t) => if isIn h s2 then diff t s2 else h::(diff t s2);

fun freeVars e : string list =
    case e of
        IConst _ => []
      | (Var v) => [v]
      | (Prim2(_, e1, e2)) => union (freeVars e1) (freeVars e2)
      | (Let(v, e1, e2)) =>
        let val v1 = freeVars e1;
            val v2 = freeVars e2
        in
            union v1 (diff v2 [v])
        end;

fun closed e = (freeVars e = []);
exception NonClosed;

fun run e =
    if closed e then
        eval e []
    else
        raise NonClosed;

val a2 = Let(
    "z", IConst 200,
    Prim2(
        "/",
        Var "z",
        IConst 10
    )
);
run a2;

val b2 = Let(
    "x", IConst 10,
    Prim2(
        "%",
        Let(
            "x", IConst 50,
            Prim2(
                "+",
                Var "x",
                Var "x"
            )
        ),
        IConst 3
    )
);
run b2;

val c2 = Let(
    "y", IConst 2,
    Prim2(
        "-",
        Let(
            "z", IConst 15,
            Prim2(
                "*",
                Var "z",
                Var "y"
            )
        ),
        IConst 10
    )
);
run c2;

val d2 = Let(
    "a", IConst 20,
    Prim2(
        "+",
        Let(
            "a", IConst 15,
            Prim2(
                "*",
                IConst 10,
                Var "a"
            )
        ),
        Var "a"
    )
);
run d2;

val e2 = Let(
    "b", IConst 50,
    Prim2(
        "+",
        Let(
            "x", IConst 20,
            Prim2(
                "+",
                Prim2(
                    "*",
                    Var "a",
                    Var "b"
                ),
                IConst 20
            )
        ),
        Var "x"
    )
);
run e2;