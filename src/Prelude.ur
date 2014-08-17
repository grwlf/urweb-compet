
fun swap [a:::Type] [b:::Type] [c:::Type] (f:a->b->c) (y:b) (z:a) : c = f z y

fun ap [a:::Type] [b:::Type] [m:::Type->Type] (_:monad m) (f:a->b) (ma:m a) : m b =
  a <- ma;
  return (f a)

fun forM_ [m ::: (Type -> Type)] (_ : monad m) [a] (ls:list a) (f:a -> m {}) : m {} =
    let
        fun mapM' ls =
            case ls of
              | []      => return {}
              | x :: ls => f x; mapM' ls
    in
        mapM' ls
    end

fun forM [m ::: (Type -> Type)] (_ : monad m) [a] [b] (ls:list a) (f:a -> m b) : m (list b) = List.mapM f ls

fun fst [a:::Type] [b:::Type] ((x,_):(a*b)) : a = x

fun snd [a:::Type] [b:::Type] ((_,y):(a*b)) : b = y

(* val show_pair [a] [b] [show a] [show b] : show (a*b) = mkShow (fn (a,b) => "("^(show a) ^ "," ^ (show b) ^ ")") *)

val show_int_string  : show (int*string) = mkShow (fn (a,b) => "("^(show a) ^ "," ^ (show b) ^ ")")
