
structure B = Bootstrap

val cls = CSS.list

fun swap a b c = a c b

fun ap [a:::Type] [b:::Type] [m:::Type->Type] (_:monad m) (f:a->b) (ma:m a) : m b =
  a <- ma;
  return (f a)

fun template (s:static) (mb:transaction xbody) : transaction page =
  u <- ap show currentUrl;
  let

    Uru.run (
    myHeaders (
    JQuery.add (
    Bootstrap.add (
    Uru.withBody (fn _ =>
      b <- mb;
      return
        <xml>
          <div class={cls (B.navbar :: B.navbar_inverse :: B.navbar_fixed_top :: [])} role="navigation">
            <div class={B.container}>
              <div class={B.navbar_header}>
                <a class={B.navbar_brand} href={s.Main}>ArcCo</a>
              </div>
              <div class={cls (B.collapse :: B.navbar_collapse :: [])}>
                <ul class={cls (B.nav :: B.navbar_nav :: [])}>
                  {active s.Main "Competitions"}
                  {active s.Users "Users"}
                  {active s.Init "Init DB"}
                </ul>
              </div>
            </div>
          </div>

          <div class={B.container}>
          {b}
          </div>
        </xml>
      )))))

  where

    fun active (l:url) (t:string) =
      case strsindex (show l) u of
        |None => <xml><li><a href={l}>{[t]}</a></li></xml>
        |Some _ => <xml><li class={B.active}><a href={l}>{[t]}</a></li></xml>

    fun myHeaders f r = 
      f (swap Uru.addHeader r
        <xml>
          <link rel="stylesheet" href={Compet_css.geturl}/>
        </xml>)

  end

