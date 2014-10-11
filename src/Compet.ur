structure B = Bootstrap
structure X = XmlGen
structure P = Prelude
structure CSS = CSS
structure Str = String

val swap = @@P.swap
val ap = @@P.ap
val fst = @@P.fst
val snd = @@P.snd

val cl = @@CSS.list

val nest = @@X.nest
val lift = @@X.lift
val push_back = @@X.push_back
val push_front = @@X.push_front
val push_back_ = @@X.push_back_
val push_front_ = @@X.push_front_
val push_back_xml = @@X.push_back_xml

val data = data_attr data_kind
val aria = data_attr aria_kind

style invisible
style visible

(*
 _   _ _   _ _     
| | | | |_(_) |___ 
| | | | __| | / __|
| |_| | |_| | \__ \
 \___/ \__|_|_|___/

*) 

datatype identified_with a = IdWith of a * int

val show_iw [a:::Type] : show (identified_with a) =
  mkShow (fn (IdWith (a,i)) => "idwith#" ^ (show i))
val eq_iw [a:::Type] : eq (identified_with a) =
  mkEq (fn (IdWith (a,i1)) (IdWith (a,i2)) => i1 = i2)

(* fun modify s f = l <- get s; set s (f l) *)

fun checked [x ::: Type] (l : list (source bool * x)) : transaction (list x) =
  List.mapPartialM (fn (s,r) =>
    v <- get s;
    case v of
      |True => return (Some r)
      |False => return None) l

(** XMLGen-based *)

fun tnest [a ::: Type] (nb : X.state xtable a) : X.state xbody (xbody * a) =
  nest (fn x =>
    <xml>
      <table class={cl (B.bs3_table :: B.table_striped :: [])}>
        {x}
      </table>
    </xml>) nb

datatype osd = Hidden | Error of string | Notice of string

fun mkosd {} : X.state xbody (source osd) =
  let
    me <- lift currentUrl;
    s <- X.source Hidden;
    push_back_xml
    <xml><p>
      <dyn signal={v <- signal s;
        case v of
          |Hidden => return <xml></xml>
          |Error s => return <xml>{label (reload me) B.alert_danger s}</xml>
          |Notice s => return <xml>{label <xml/> B.alert_success s}</xml>
        }/>
    </p></xml>;
    return s

  where

    fun reload me = <xml><a href={me}>Reload</a> current page to return to actual state.</xml>

    fun label rel css v = 
      <xml>
        <div class={cl (B.alert :: css :: B.alert_dismissable :: [])} role="alert">
          <button value="" class={B.close} data={data "dismiss" "alert"} onclick={fn _ => return {}}>
            <span data={aria "hidden" "true"}>&times;</span>
            <span class={B.sr_only}>
              Close
            </span>
          </button>
          {[v]} {rel}
        </div>
      </xml>

  end

(* fun info_fail s = push <xml><div class={cl (B.alert :: B.alert_danger :: [])} role="alert">{[s]}</div></xml> *)

(* Bootstrap-based pills *)

fun pills [a ::: Type] (eu:url) (f : (url -> xbody -> X.state xbody {}) -> X.state xbody a) : X.state xbody a =
  let
    push_back( nest (fn x => <xml><ul class={cl (B.nav :: B.nav_pills :: [])}>{x}</ul></xml>) (f pill))
  where
    fun pill (u:url) (x:xbody) : X.state xbody {} =
      (case (show u) = (show eu) of
       |True => push_back_xml <xml><li class={B.active}><a href={u}>{x}</a></li></xml>
       |False => push_back_xml <xml><li><a href={u}>{x}</a></li></xml>);
      return {}
  end

(** Old-styled *)

fun mktab [other ::: {Unit}] [tables ::: {{Type}}] [exps ::: {Type}] [inp ::: {Type}]
           [tables ~ exps] [other ~ [Table,Body]] (q : sql_query [] [] tables exps)
           (hdr : xml (other ++ [Table]) inp [])
           (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> xml (other ++ [Table]) inp [])
              : transaction (xml (other ++ [Body]) inp []) =
  r <- queryX q f;
  return <xml>
    <table class={cl (B.bs3_table :: B.table_striped :: [])}>
      {hdr}
      {r}
    </table>
  </xml>

fun formgroup [nm :: Name] [other ::: {Unit}] [inp ::: {Type}] 
                [other ~ [Body,Form]] [inp ~ [nm=string]]
                (n:string) :
                    xml (other ++ [Body,Form]) inp ([nm=string]) =
  <xml>
    <div class={B.form_group}>
      <label class={B.col_xs_2}>{[n]}</label>
      <div class={B.col_xs_10}>
        <textbox{nm} class={B.form_control}/>
      </div>
    </div>
  </xml>

fun formgroup_def [nm :: Name] [flds:::{Type}] [other ::: {Unit}] [inp ::: {Type}] 
                [other ~ [Body,Form]] [inp ~ [nm=string]] [flds ~ [nm=string]]
                (v:$([nm=string] ++ flds)) (n:string) :
                    xml (other ++ [Body,Form]) inp ([nm=string]) =
  <xml>
    <div class={B.form_group}>
      <label class={B.col_xs_2}>{[n]}</label>
      <div class={B.col_xs_10}>
        <textbox{nm} class={B.form_control} value={v.nm}/>
      </div>
    </div>
  </xml>

fun formgroup2 [other ::: {Unit}] [inp ::: {Type}] [frm ::: {Type}] [other ~ [Body]] [inp ~ frm]
  (x:xml (other ++ [Body]) inp frm) (n:string) :
  xml (other ++ [Body]) inp frm =
  <xml>
    <div class={B.form_group}>
      <label class={B.col_xs_2}>{[n]}</label>
      <div class={B.col_xs_10}>
        {x}
      </div>
    </div>
  </xml>


fun mkform [t:::{Type}] (x:xml form [] t) : xbody =
  <xml>
      <form role="form" class={B.form_horizontal}>
      {x}
      </form>
  </xml>

fun mkrow x = <xml><div class={B.row}><div class={B.col_xs_12}>{x}</div></div> </xml>

(* fun mkcont x = <xml><div class={B.container}>{x}</div></xml> *)


fun hidingpanel (x:xbody) : transaction xbody =
  bt1 <- source invisible;
  bt2 <- source visible;
  return
  <xml>
    <button dynClass={signal bt2} onclick={fn _ => 
      set bt1 visible; set bt2 invisible}>Edit</button>
    <button dynClass={signal bt1} onclick={fn _ => 
      set bt2 visible; set bt1 invisible}>Hide</button>
    <div dynClass={signal bt1}>
      {x}
    </div>
  </xml>


(*
 ____        _              _       __ _       _ _   _                 
|  _ \  __ _| |_ __ _    __| | ___ / _(_)_ __ (_) |_(_) ___  _ __  ___ 
| | | |/ _` | __/ _` |  / _` |/ _ \ |_| | '_ \| | __| |/ _ \| '_ \/ __|
| |_| | (_| | || (_| | | (_| |  __/  _| | | | | | |_| | (_) | | | \__ \
|____/ \__,_|\__\__,_|  \__,_|\___|_| |_|_| |_|_|\__|_|\___/|_| |_|___/

*)

datatype sex = Male | Female

val show_sex : show sex = mkShow (fn x => case x of |Male => "М" |Female => "Ж")
val read_sex : read sex = mkRead (fn s => case Str.mp toupper s of |"М" => Male | "Ж" => Female | _ => error <xml>Field 'Sex' hould be either {[Male]} or {[Female]}</xml>)
                                 (fn s => case Str.mp toupper s of |"М" => Some Male | "Ж" => Some Female | _ => None)


con sportsmen_base = [SName = string, Sex = serialized sex, Bow = string , Birth = string, Club = string, Rank = string]

con sportsmen = ([Id = int] ++ sportsmen_base)

val show_sportsmen : show (record sportsmen) = mkShow (fn s => (show s.Id) ^ " " ^ (show s.SName))

table sportsmen : (sportsmen)
  PRIMARY KEY  Id

sequence sportsmenSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet
  PRIMARY KEY Id

sequence competSeq

con group = [GName = string]

table groups : ([Id = int] ++ group)
  PRIMARY KEY (Id)

sequence groupSeq

fun groups_new_ (gr:record group) : transaction int =
  i<-nextval groupSeq;
  dml(INSERT INTO groups (Id, GName) VALUES ({[i]}, {[gr.GName]}));
  return i

table compet_groups : ([CId = int, GId = int])
  PRIMARY KEY (CId, GId),
  CONSTRAINT CG_C FOREIGN KEY (CId) REFERENCES compet (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT CG_G FOREIGN KEY (GId) REFERENCES groups (Id) ON DELETE CASCADE ON UPDATE RESTRICT

table compet_sportsmen : ([CId = int, SId = int] ++ sportsmen_base ++ [Target = string])
  PRIMARY KEY (CId, SId),
  CONSTRAINT CS_S FOREIGN KEY (SId) REFERENCES sportsmen (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT CS_C FOREIGN KEY (CId) REFERENCES compet (Id) ON DELETE CASCADE ON UPDATE RESTRICT

table scores : ([CId = int, SId = int, Round = int, Score = int])
  PRIMARY KEY (CId, SId, Round),
  CONSTRAINT S_S FOREIGN KEY (SId) REFERENCES sportsmen (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT S_C FOREIGN KEY (CId) REFERENCES compet (Id) ON DELETE CASCADE ON UPDATE RESTRICT

fun compet_new_ fs = 
  i <- nextval competSeq;
  dml(INSERT INTO compet(Id,CName,Hide) VALUES ({[i]}, {[fs.CName]}, {[False]}));
  return i

fun sportsmen_new_ fs =
  i <- nextval sportsmenSeq;
  dml(INSERT INTO sportsmen (Id,SName,Sex,Bow,Birth,Rank,Club)
      VALUES ({[i]}, {[fs.SName]}, {[serialize (readError fs.Sex)]}, {[fs.Bow]}, {[fs.Birth]}, {[fs.Rank]}, {[fs.Club]}));
  return i

fun compet_register_ cid uid : transaction {} =
  u <- oneRow1(SELECT * FROM sportsmen AS U WHERE U.Id = {[uid]});
  dml(INSERT INTO compet_sportsmen (CId, SId, SName, Sex, Bow, Birth, Club, Rank, Target)
      VALUES ({[cid]}, {[u.Id]}, {[u.SName]}, {[u.Sex]}, {[u.Bow]}, {[u.Birth]}, {[u.Club]}, {[u.Rank]}, ""));
  dml(INSERT INTO scores (CId, SId, Round, Score) VALUES ({[cid]}, {[u.Id]}, 0, 0));
  dml(INSERT INTO scores (CId, SId, Round, Score) VALUES ({[cid]}, {[u.Id]}, 1, 0));
  return {}


(*

 _____                    _       _       
|_   _|__ _ __ ___  _ __ | | __ _| |_ ___ 
  | |/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \
  | |  __/ | | | | | |_) | | (_| | ||  __/
  |_|\___|_| |_| |_| .__/|_|\__,_|\__\___|
                   |_|                    

*)

fun template (mb:transaction xbody) : transaction page =
  u <- ap show currentUrl;
  let
    Uru.run (
    JQuery.add (
    Bootstrap.add (
    myHeaders (
    Uru.withBody (fn _ =>
      b <- mb;
      return
        <xml>
          <div class={cl (B.navbar :: B.navbar_inverse :: B.navbar_fixed_top :: [])} role="navigation">
            <div class={B.container}>
              <div class={B.navbar_header}>
                <a class={B.navbar_brand} href={s.Main}>ArcCo</a>
              </div>
              <div class={cl (B.collapse :: B.navbar_collapse :: [])}>
                <ul class={cl (B.nav :: B.navbar_nav :: [])}>
                  {active s.Main "Competitions"}
                  {active s.Sportsmen "Sportsmen"}
                  {active s.Init "Reset DB"}
                </ul>
              </div>
            </div>
          </div>

          <div class={B.container} style="margin-top:50px; margin-bottom:100px">
          {b}
          </div>

          <div style="padding-top:20px;position:absolute;bottom:0; width:100%; height:100px;">
            <div class={B.container} style="text-align:center">
              <hr/>
              <p class={B.text_muted}>Proudly designed by
                <a href={bless "http://github.com/grwlf"}>@grwlf</a>.
                Powerd by <a href={bless "http://impredicative.com/ur/"}>Ur/Web</a> framework.
              </p>
              <p class={B.text_muted}>
              <ul style="padding-left: 0px; margin-top: 20px; color: #999;">
                {StyleSoup.footer_doc_links (
                <xml><a href={bless "http://github.com/grwlf/urweb-compet"}>Sources</a></xml> ::
                <xml><a href={bless "http://impredicative.com/ur/"}>Ur/Web</a></xml> ::
                <xml><a href={bless "http://github.com"}>GiHub</a></xml> ::
                <xml><a href={bless "http://github.com/grwlf"}>Grwlf</a></xml> ::
                []
                )}
              </ul>
              </p>
            </div>
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
          <title>Compet</title>
          <link rel="stylesheet" href={Compet_css.geturl}/>
        </xml>)

    val s = {
      Title = "Competitions",
      Main = url(complist_view {}),
      Sportsmen = url(sportsmen_list {}),
      About = url(about {}),
      Init = url(init {})
      }
  end

(*

 __  __       _                          _   
|  \/  | __ _(_)_ __    _ __   __ _ _ __| |_ 
| |\/| |/ _` | | '_ \  | '_ \ / _` | '__| __|
| |  | | (_| | | | | | | |_) | (_| | |  | |_ 
|_|  |_|\__,_|_|_| |_| | .__/ \__,_|_|   \__|
                       |_|                   

*)

and registered_details (cid:int) (uid:int) : transaction page =
  let
    template  (
      fs <- oneRow1(SELECT * FROM compet_sportsmen AS U WHERE U.SId = {[uid]} AND U.CId = {[cid]});
      return <xml>
        <h3>{[fs.SName]}</h3>
        <p>
          <h4>Update</h4>
          {mkform <xml>
            {formgroup_def [#SName] fs "Name"}
            {formgroup2 <xml><textbox{#Sex} class={B.form_control} value={show (deserialize fs.Sex)}/></xml> "Sex"}
            {formgroup_def [#Birth] fs "Birth"}
            {formgroup_def [#Club] fs "Club"}
            {formgroup_def [#Rank] fs "Rank"}
            {formgroup2 <xml><checkbox{#Propagate} checked={False}/></xml> "Also update sportsmen"}
            <submit action={registered_update} value="Update"/>
          </xml>}
        </p>

        <p>
          <h4>Unregister</h4>
          {mkform <xml>
            <submit action={registered_unregister} value="Unregister"/>
          </xml>}
        </p>
      </xml>
    )
  where
    fun registered_update frm =
      dml(UPDATE compet_sportsmen
          SET SName = {[frm.SName]}, Sex = {[serialize (readError frm.Sex)]}, Birth = {[frm.Birth]},
              Club = {[frm.Club]}, Rank = {[frm.Rank]}
          WHERE CId = {[cid]} AND SId = {[uid]});
      (case frm.Propagate of
       |False => return {}
       |True =>
         dml(UPDATE sportsmen
           SET SName = {[frm.SName]}, Sex = {[serialize (readError frm.Sex)]}, Birth = {[frm.Birth]},
               Club = {[frm.Club]}, Rank = {[frm.Rank]}
           WHERE Id = {[uid]}));
      redirect( url(registered_details cid uid) )

    fun registered_unregister _ =
      dml(DELETE FROM compet_sportsmen WHERE CId = {[cid]} AND SId = {[uid]});
      dml(DELETE FROM scores WHERE CId = {[cid]} AND SId = {[uid]});
      redirect( url (compet_register cid) )
  end

and compet_pills (me:url) cid = pills me (fn pill =>
  pill (url(compet_grps cid)) <xml>Groups</xml>;
  pill (url(compet_register cid)) <xml>Registration</xml>;
  pill (url(compet_targets cid)) <xml>Targets</xml>;
  pill (url(compet_details2 cid)) <xml>Scores</xml>;
  pill (url(compet_admin cid)) <xml>Admin</xml>;
  return {})

and compet_caption cid cap =
  fs <- X.oneRow1 (SELECT * FROM compet AS T WHERE T.Id = {[cid]});
  push_back_xml <xml><h2>{[fs.CName]}</h2></xml>;
  push_back_xml <xml><h3>{[cap]}</h3></xml>;
  return fs

and compet_grps cid =
  let
    me <- currentUrl;
    template ( X.run (
      
      compet_caption_ cid "Groups";

      compet_pills me cid;

      i <- mkosd {};

      (* ch <- lift (source []); *)

      push_back ( tnest (

        push_back_xml
        <xml><tr>
          <th></th>
          <th>Name</th>
        </tr></xml>;

        X.query_
        (
          SELECT *
          FROM groups AS G LEFT OUTER JOIN compet_groups AS CG ON CG.GId = G.Id
        )
        (fn fs =>
          s <- X.source (case fs.CG.CId of |Some x => if x = cid then True else False |_=>False);

          push_back_xml
          <xml><tr>
            (* <td><ccheckbox source={s} onchange={modify ch (P.add1 eq (IdWith (s,fs.G.Id)))}/></td> *)
            <td><ccheckbox source={s} onchange={
              v <- get s;
              r <- (case v of
                |True => tryRpc (compet_groups_add fs.G.Id)
                |False => tryRpc (compet_groups_rm fs.G.Id));
              case r of
                |Some _ => set i Hidden
                |None => set i (Error "Failed to update the database.")
            }/></td>
            <td>{[fs.G.GName]}</td>
          </tr></xml>;
          return {}
        )
      ));

      push_back_xml
        <xml>
          <p>
            <h3>Add new group</h3>
            {mkform <xml>
              {formgroup [#GName] "Group name"}
              <submit action={compet_groups_new} value="New group"/>
              </xml>}
          </p>
        </xml>
    ))
  where
    fun compet_groups_add (gid:int) : transaction unit =
      dml(INSERT INTO compet_groups (CId, GId) VALUES ({[cid]}, {[gid]}))

    fun compet_groups_rm (gid:int) : transaction unit =
      dml(DELETE FROM compet_groups WHERE CId = {[cid]} AND GId = {[gid]})

    fun compet_groups_new frm : transaction page =
      g <- oneOrNoRows(SELECT * FROM groups AS G WHERE G.GName = {[frm.GName]} ORDER BY G.GName LIMIT 1);
      gid <- (case g of |Some g => return g.G.Id |None => groups_new_ frm);
      dml(INSERT INTO compet_groups (CId, GId) VALUES ({[cid]}, {[gid]}));
      redirect (url (compet_grps cid))
  end

and compet_caption_ cid cap =
  _ <- compet_caption cid cap;
  return {}

and compet_targets cid =
  let
    me <- currentUrl;
    template ( X.run (
      
      compet_caption_ cid "Target assignments";

      compet_pills me cid;

      i <- mkosd {};

      push_back ( nest P.id (
        ss <- push_back( tnest (
          push_back_xml
          <xml><tr>
            <th></th>
            <th>ID</th>
            <th>Name</th>
            <th>Target</th>
          </tr></xml>;

          P.ap List.rev (X.query
          (
            SELECT *
            FROM compet_sportsmen AS CS
            WHERE CS.CId = {[cid]}
            ORDER BY CS.SId
          )
          []
          (fn fs ss =>
            s <- X.source True;
            e <- X.source fs.CS.Target;

            push_back_xml
            <xml><tr>
              <td><ccheckbox source={s}/></td>
              <td>{[fs.CS.SId]}</td>
              <td>{[fs.CS.SName]}</td>
              <td><ctextbox source={e}/></td>
            </tr></xml>;

            return ((s,(fs.CS.SId,e)) :: ss)
          ))
        ));

        push_front( nest P.id (

          ntargets <- X.source "10";
          push_back_xml <xml>Number of targets <ctextbox source={ntargets}/><br/></xml>;

          letters <- X.source "ABCD";
          push_back_xml <xml>Slots per target <ctextbox source={letters}/><br/></xml>;

          push_back_xml
          <xml>
            <button value="Assign selected" onclick={fn _ => 
              cs <- checked ss;
              ntgt <- P.ap readError (get ntargets);
              slots' <- P.ap P.strlist (get letters);

              P.foldlM_ (
                fn (id, e) (tgt,slots) =>
                  case tgt > ntgt of
                    |True => (set e "" ; return (tgt,slots))
                    |False =>
                      (case slots of
                        | s :: [] =>
                          set e ((show tgt) ^ (str1 s));
                          return (tgt+1, slots')
                        | s :: ss => 
                          set e ((show tgt) ^ (str1 s));
                          return (tgt, ss)
                        | [] =>
                          set e ((show tgt));
                          return (tgt+1, [])
                      )
                ) (1,slots') cs;
              return {}
            }/>
          </xml>;

          push_back_xml
          <xml>
            <button value="Apply" onclick={fn _ => 
              vs <- P.forM ss (fn (_,(id,e)) => v <- get e; return (id,v));
              rpc(compet_targets_apply vs);
              set i (Notice "Success");
              return {}
            } />
          </xml>
        ))
      ))
    ))
  where
    fun compet_targets_apply lst =
      P.forM_ lst (fn (id,v) =>
        dml(UPDATE compet_sportsmen SET Target = {[v]} WHERE CId = {[cid]} AND SId = {[id]}))
  end

and compet_admin cid =
  let
    me <- currentUrl;
    template ( X.run (
      
      fs <- compet_caption cid "Admin";

      compet_pills me cid;

      push_back_xml
        <xml>
          <p>
            <h3>Change name</h3>
            {mkform <xml>
              {formgroup_def [#CName] fs "Name"}
              <submit action={compet_update} value="Update"/>
              </xml>}
          </p>
          <p>
            <h3>Delete (Warning!)</h3>
            <form>
            <submit action={compet_delete} value="Do it"/>
            </form>
          </p>
        </xml>;

      return {}

    ))
  where
    fun compet_update new : transaction page =
      dml(UPDATE compet SET CName = {[new.CName]} WHERE Id = {[cid]});
      redirect ( url (compet_admin cid))

    fun compet_delete _ : transaction page =
      dml(DELETE FROM compet WHERE Id = {[cid]});
      redirect (url (complist_view {}))
  end

and compet_details2 cid =
  let
    me <- currentUrl;
    template ( X.run (

      compet_caption_ cid "Scores";

      compet_pills me cid;

      push_back ( tnest (

        push_back_xml
        <xml><tr>
          <th></th>
          <th>Name</th>
          <th>Birth</th>
          <th>Bow</th>
          <th>Club</th>
          <th>Round 1</th>
          <th>Round 2</th>
          <th></th>
        </tr></xml>;

        X.query_ (
          SELECT *
          FROM compet_sportsmen AS CS,
               scores AS S0,
               scores AS S1
          WHERE
                CS.CId = {[cid]}
            AND S0.CId = {[cid]}
            AND S0.Round = 0     
            AND S0.SId = CS.SId
            AND S1.CId = {[cid]}
            AND S1.Round = 1
            AND S1.SId = CS.SId
        )
          
        (fn fs =>
          s <- X.source False;

          push_back_xml
          <xml><tr>
            <td><ccheckbox source={s}/></td>
            <td>{[fs.CS.SName]}</td>
            <td>{[fs.CS.Birth]}</td>
            <td>{[fs.CS.Bow]}</td>
            <td>{[fs.CS.Club]}</td>
            <td>{[fs.S0.Score]}</td>
            <td>{[fs.S1.Score]}</td>
            <td><a link={registered_details cid fs.CS.SId}>[Details]</a></td>
          </tr></xml>)
      ))
    ))
  where
  end

and compet_register cid =
  let
    me <- currentUrl;
    template ( X.run (

      compet_caption_ cid "Registeration";

      compet_pills me cid;

      push_back( tnest (

        push_back_xml
        <xml><tr>
          <th>Name</th>
          <th>Birth</th>
          <th>Bow</th>
          <th>Club</th>
          <th></th>
        </tr></xml>;

        X.query_ (SELECT * FROM compet_sportsmen AS CS WHERE CS.CId = {[cid]})
        (fn fs =>
          push_back_xml
          <xml><tr>
            <td>{[fs.CS.SName]}</td>
            <td>{[fs.CS.Birth]}</td>
            <td>{[fs.CS.Bow]}</td>
            <td>{[fs.CS.Club]}</td>
            <td><a link={registered_details cid fs.CS.SId}>[Details]</a></td>
          </tr></xml>
       )

      ));

      ss <- X.source "";
      ss2 <- X.source [];

      push_back_xml
      <xml><div>
        <ctextbox source={ss}/>
        <button value="Search" onclick={fn _ =>
          v <- get ss;
          ls <- rpc (sportsmen_search cid v);
          set ss2 ls
        }/>
        <dyn signal={
          l <- signal ss2;
          return (swap List.mapX l (fn x =>
            <xml>
              <br/>
              <button value="Register" onclick={fn _ =>
                rpc (compet_register_ cid x.Id);
                redirect (url (compet_register cid))
              }/>
              {[x.SName]} ({[x.Birth]})
            </xml>
            ))
         }/>
      </div></xml>
    ))

  where
  end

and complist_caption cap =
  push_back_xml <xml><h2>Competitions</h2></xml>;
  push_back_xml <xml><h3>{[cap]}</h3></xml>;
  return {}

and complist_pills (me:url) = pills me (fn pill =>
  pill (url(complist_view {})) <xml>List</xml>;
  pill (url(complist_add {})) <xml>Add</xml>;
  return {})

and complist_add {} =
  let
    me <- currentUrl;
    template ( X.run (

      complist_caption "Add new competition";

      complist_pills me;

      push_back_xml
      <xml>
        {mkrow (mkform
        <xml>
          {formgroup [#CName] "Name"}
          <submit action={compet_new} value="Create"/>
        </xml>
        )}
      </xml>
    ))
  where
    fun compet_new fs = 
      _ <- compet_new_ fs;
      redirect ( url (complist_view {}))
  end

and complist_view {} : transaction page = 
  me <- currentUrl;
  template ( X.run (

    complist_caption "List";

    complist_pills me;

    push_back( tnest (

      push_back_xml
      <xml><tr>
        <th>ID</th><th>Name</th><th>Participants</th><th/><th/>
      </tr></xml>;

      X.query_
      (SELECT * FROM compet AS T)
      (fn fs =>

        c <- X.oneRow (SELECT COUNT( * ) AS N FROM compet_sportsmen AS CS WHERE CS.CId = {[fs.T.Id]});

        push_back_xml
        <xml><tr>
          <td>{[fs.T.Id]}</td>
          <td>{[fs.T.CName]}</td>
          <td>{[c.N]}</td>
          <td> <a link={compet_grps fs.T.Id}>[Details]</a> </td>
          <td> <a link={compet_details2 fs.T.Id}>[Scores]</a> </td>
        </tr></xml>
      )
    ))
  ))

and sportsmen_list {} : transaction page =
  let
    template (

      t <- mktab (SELECT * FROM sportsmen AS T) (
          <xml>
            <tr>
              <th>ID</th>
              <th>Name</th>
              <th>Sex</th>
              <th>Birth</th>
              <th>Bow</th>
              <th>Rank</th>
              <th>Club</th>
              <th></th>
            </tr>
          </xml>)
          (fn fs =>
          <xml>
            <tr>
              <td>{[fs.T.Id]}</td>
              <td>{[fs.T.SName]}</td>
              <td>{[deserialize fs.T.Sex]}</td>
              <td>{[fs.T.Birth]}</td>
              <td>{[fs.T.Bow]}</td>
              <td>{[fs.T.Rank]}</td>
              <td>{[fs.T.Club]}</td>
              <td> <a link={sportsmen_detail fs.T.Id}>[Details]</a> </td>
            </tr>
          </xml>);

      return <xml>

          {mkrow t}

          {mkrow (mkform
          <xml>
            {formgroup [#SName] "SName"}
            {formgroup [#Sex] "Sex"}
            {formgroup [#Birth] "Birth"}
            {formgroup [#Bow] "Bow"}
            {formgroup [#Rank] "Rank"}
            {formgroup [#Club] "Club"}
            <submit action={sportsmen_new} value="Create"/>
          </xml>)}

        </xml>)

  where

    fun sportsmen_new fs : transaction page = 
      _ <- sportsmen_new_ fs;
      redirect ( url (sportsmen_list {}))

    fun sportsmen_detail uid : transaction page = 
      let
        template (return
        <xml>
          {mkform
          <xml>
            <submit action={sportsmen_delete} value="Delete sportsmen"/>
          </xml>}
        </xml>)
      where
        fun sportsmen_delete _ : transaction page =
          dml(DELETE FROM sportsmen WHERE Id = {[uid]});
          redirect ( url (sportsmen_list {}))
      end
  end

and sportsmen_search (cid:int) (s:string) : transaction (list (record sportsmen)) =
  fs <- queryL(
    SELECT * FROM sportsmen AS U,
      (SELECT U.Id AS I, COUNT(CS.CId) AS N
       FROM sportsmen AS U LEFT JOIN compet_sportsmen AS CS ON U.Id = CS.SId AND CS.CId = {[cid]}
       WHERE U.SName LIKE {["%" ^ s ^ "%"]} GROUP BY U.Id) AS SS
    WHERE U.Id = SS.I AND SS.N = 0);
  return (List.mp (fn x => x.U) fs)

and main {} : transaction page = redirect (url (complist_view {}))

and about {} : transaction page = return <xml><body>About page</body></xml>

and init {} : transaction page =
  dml(DELETE FROM compet WHERE Id>0);
  dml(DELETE FROM sportsmen WHERE Id>0);
  dml(DELETE FROM groups WHERE Id>0);
  g1 <- groups_new_ {GName="Мужчины"};
  g2 <- groups_new_ {GName="Женщины"};
  c1 <- compet_new_ {CName="Competition 1"};
  c2 <- compet_new_ {CName="Competition 2"};
  u1 <- sportsmen_new_ {SName="Иванов Пётр", Sex="М", Bow="Классика", Birth="03.04.1998", Rank="1", Club="СДЮШОР8"};
  u2 <- sportsmen_new_ {SName="Степанов Степан", Sex="М", Bow="Классика", Birth="03.04.1997", Rank="1", Club="СДЮШОР8"};
  u3 <- sportsmen_new_ {SName="Петров Иван", Sex="М", Bow="Классика", Birth="03.04.1997", Rank="1", Club="СДЮШОР8"};
  u4 <- sportsmen_new_ {SName="Дмитриев Дмитрий", Sex="М", Bow="Блок", Birth="03.04.1978", Rank="1", Club="СДЮШОР8"};
  u5 <- sportsmen_new_ {SName="Иванова Мария", Sex="Ж", Bow="Блок", Birth="03.04.1988", Rank="2", Club="СДЮШОР8"};
  compet_register_ c1 u1;
  compet_register_ c1 u2;
  compet_register_ c1 u3;
  compet_register_ c2 u1;
  compet_register_ c2 u4;
  compet_register_ c2 u5;
  redirect (url (complist_view {}))


