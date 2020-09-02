datatype expr =
         IConst of int
         | Prim2 of string * expr * expr
         | Var of string
         | Let of string * expr * expr
         | LetFun of string * string * expr * expr
         | Call of expr * expr;

type 'v env = (string * 'v) list;

datatype value =
         Int of int
         | Closure of string * string * expr * value env;

exception EvalError;
exception FreeVar;

fun lookup [] id = raise FreeVar
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

fun eval (e:expr) (st: (string * value) list) : int =
    case e of
        (IConst i) => i
      | (Var v) =>
        let val vv = lookup st v in
            case vv of
                (Int i) => i
              | _ => raise EvalError
        end
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
      | (Let(x, e1, e2)) => eval e2 ((x, Int (eval e1 st))::st)
      | (LetFun(f, x, e1, e2)) => eval e2 ((f, Closure(f, x, e1, st))::st)
      | (Call(Var f, e)) =>
        let val fv = (lookup st f) in
            case fv of
                (Closure(f, x, e1, fSt)) =>
                let
                    val ev = Int(eval e st);
                    val st' = (x, ev) :: (f, fv) :: fSt
                in
                    eval e1 st'
                end
            | _ => raise EvalError
        end
    ;

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
        end
      | (LetFun(f, x, e1, e2)) =>
        let val v1 = freeVars e1;
            val v2 = freeVars e2
        in
            union (diff v1 [x]) (diff v2 [f])
        end
      | (Call(Var f, e)) => freeVars e;

fun closed e = (freeVars e = []);
exception NonClosed;

fun run e =
    if closed e then
        eval e []
    else
        raise NonClosed;

val test = Let(
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
        Call(Var "y", IConst 10)
    )
);

fun calculaFechadaEmX abertaEmX x v = Let(x, IConst v, abertaEmX);

fun calculaFechadaEmF abertaEmF f arg e = LetFun(f, arg, e, abertaEmF);

run (
    calculaFechadaEmF
    (calculaFechadaEmX test "a" 1)
    "y"
    "z"
    (Prim2("+", Var "z", IConst 1))
);