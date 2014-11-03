structure P = Prelude
structure L = List
structure X = XmlGen
structure B = Bootstrap

val nest = @@X.nest
val lift = @@X.lift
val push_back = @@X.push_back
val push_front = @@X.push_front
val push_back_ = @@X.push_back_
val push_front_ = @@X.push_front_
val push_back_xml = @@X.push_back_xml
val cl = @@CSS.list

fun tnest [a ::: Type]
    (h  : {OnReorder : list (int*int) -> transaction unit})
    (nb : (xtr -> X.state xtable {}) ->(int -> int -> xtr -> X.state xtable {}) -> X.state xtable a)
        : X.state xbody (xbody * a) =

  dragged <- X.source None;
  ch <- X.source [];

  let

  X.nest (fn x =>
    <xml>
      <table class={cl (B.bs3_table :: B.table_striped :: [])}
        (* onmouseout={fn _ => l<-get ch;  h.OnReorder l} *)
      >
        {x}
      </table>
    </xml>) (nb mkhdr mkrow)

  where

    fun mkhdr (x:xtr) : X.state xtable {} =
      push_back_xml <xml><tr><th/>{x}</tr></xml>

    fun mkrow (i:int) (nd:int) (x:xtr) : X.state xtable {} =
      d <- X.source (i,x);
      push_back_xml
      <xml>
        <tr
        onmouseup={fn _ =>set dragged None; l<-get ch; h.OnReorder l}
        onmouseover={fn _ =>
          o <- get dragged;
          (case o of
            |None => return {}
            |Some (ns,s) =>
              (es,src) <- get s;
              (ed,dst) <- get d;

              if ns <> nd then
                set s (ed,dst);
                set d (es,src);
                set dragged (Some (nd,d));

                l <- get ch;
                l <- return (P.insert ed ns l);
                l <- return (P.insert es nd l);
                set ch l
              else
                return {}
          )
        }>
        <td
        onmousedown={fn _ =>set dragged (Some (nd,d))}
        >
        <span class={cl (B.glyphicon :: B.glyphicon_chevron_up ::[])}/>
        </td>
        <dyn signal={(_,x) <- signal d; return x}/>
        </tr>
      </xml>
  end

