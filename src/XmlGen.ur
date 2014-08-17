
structure MT = State.Trans(struct con m = Basis.transaction end)

con state = MT.state

fun push [ctx:::{Unit}] (x:xml ctx [] []) : MT.state (xml ctx [] []) {} =
  MT.modify (fn s => <xml>{s}{x}</xml>)

val lift = @@MT.lift

fun nest [a ::: Type] [ctx:::{Unit}] [ctx2 :::{Unit}]
    (f:xml ctx2 [] [] -> xml ctx [] [])
    (x:MT.state (xml ctx2 [] []) a)
      : MT.state (xml ctx [] []) a =
  (xml2,a) <- MT.lift (MT.run <xml/> x);
  push (f xml2);
  return a

fun source [st:::Type] [t:::Type] (x:t) : MT.state st (source t) =
  MT.lift (Basis.source x)

fun run (s : MT.state xbody {}) : transaction xbody =
  (x,_) <- MT.run <xml/> s;
  return x

fun query [ctx ::: {Unit}] [tables ::: {{Type}}] [exps ::: {Type}] [tables ~ exps] [state ::: Type]
  (q:sql_query [] [] tables exps) 
  (st:state)
  (f:$(exps ++ map (fn fields :: {Type} => $fields) tables) -> state -> MT.state (xml ctx [] []) state)
    : MT.state (xml ctx [] []) state =
  x <- MT.get {};
  (x',st') <- MT.lift( Basis.query q (fn r (xx,ss) => MT.run xx (f r ss)) (x,st) );
  MT.set x';
  return st'

fun query_ [ctx ::: {Unit}] [tables ::: {{Type}}] [exps ::: {Type}] [tables ~ exps]
  (q:sql_query [] [] tables exps) 
  (f:$(exps ++ map (fn fields :: {Type} => $fields) tables) -> MT.state (xml ctx [] []) {})
    : MT.state (xml ctx [] []) {} =
  x <- MT.get {};
  x' <- MT.lift( Basis.query q (fn r xx => MT.eval xx (f r)) x);
  MT.set x';
  return {}

fun oneRow1 [st ::: Type] [nm ::: Name] [fs ::: {Type}]
  (q:sql_query [] [] [nm = fs] []) : MT.state st $fs =
    MT.lift ( Top.oneRow1 q )


