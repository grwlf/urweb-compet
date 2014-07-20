
structure B = Bootstrap

val cls = CSS.list

fun swap a b c = a c b

fun myHeaders f r = 
  f (swap Uru.addHeader r
    <xml>
      <link rel="stylesheet" href={Compet_css.geturl}/>
    </xml>)

fun template (s:static) (mb:transaction xbody) : transaction page =
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
              <a class={B.navbar_brand} href={s.Main}>Project name</a>
            </div>
            <div class={cls (B.collapse :: B.navbar_collapse :: [])}>
              <ul class={cls (B.nav :: B.navbar_nav :: [])}>
                <li class={B.active}><a href={s.Main}>Competitions</a></li>
                <li><a href={s.Users}>Users</a></li>
                <li><a href={s.About}>About</a></li>
                <li><a href={s.Main}>Contact</a></li>
              </ul>
            </div>
          </div>
        </div>

        <div class={B.container}>
        {b}
        </div>

      </xml>
    )))))

