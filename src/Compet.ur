structure B = Bootstrap
structure X = XmlGen
structure P = Prelude
structure CSS = CSS

val swap = @@P.swap
val ap = @@P.ap
val fst = @@P.fst
val snd = @@P.snd

val cl = @@CSS.list

val nest = @@X.nest
val push = @@X.push
val lift = @@X.lift

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

fun checked [x ::: Type] (l : list (source bool * x)) : transaction (list x) =
  List.mapPartialM (fn (s,r) =>
    v <- get s;
    case v of
      |True => return (Some r)
      |False => return None) l

(** XMLGen-based *)

fun tnest [a ::: Type] (nb : X.state xtable a) : X.state xbody a =
  nest (fn x =>
    <xml>
      <table class={cl (B.bs3_table :: B.table_striped :: [])}>
        {x}
      </table>
    </xml>) nb

fun info_success (s:string) : X.state xbody (source string) =
  let
    s <- X.source s;
    push
        <xml><p>
          <dyn signal={v <- signal s;
            case v of
              |"" => return <xml><div class={invisible}>{label "OK"}</div></xml>
              |_=> return <xml>{label v}</xml>
            }/>
        </p></xml>;
      return s
  where
    fun label v = 
      <xml>
        <div class={cl (B.alert :: B.alert_success :: B.alert_dismissable :: [])} role="alert">
          <button value="" class={B.close} data={data "dismiss" "alert"} onclick={fn _ => return {}}>
            <span data={aria "hidden" "true"}>&times;</span>
            <span class={B.sr_only}>
              Close
            </span>
          </button>
          {[v]}
        </div>
      </xml>
  end

(* fun info_fail s = push <xml><div class={cl (B.alert :: B.alert_danger :: [])} role="alert">{[s]}</div></xml> *)

(* Bootstrap-based pills *)

fun pills [a ::: Type] (eu:url) (f : (url -> xbody -> X.state xbody {}) -> X.state xbody a) : X.state xbody a =
  let
    nest (fn x => <xml><ul class={cl (B.nav :: B.nav_pills :: [])}>{x}</ul></xml>) (f pill)
  where
    fun pill (u:url) (x:xbody) : X.state xbody {} =
      (case (show u) = (show eu) of
       |True => push <xml><li class={B.active}><a href={u}>{x}</a></li></xml>
       |False => push <xml><li><a href={u}>{x}</a></li></xml>);
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

con user_base = [UName = string, Bow = string , Birth = string, Club = string, Rank = string]

con user = ([Id = int] ++ user_base)

val show_user : show (record user) = mkShow (fn s => (show s.Id) ^ " " ^ (show s.UName))

table users : (user)
  PRIMARY KEY  Id

sequence usersSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet
  PRIMARY KEY Id

sequence competSeq


table compet_users : ([CId = int, UId = int] ++ user_base ++ [Target = string])
  PRIMARY KEY (CId, UId),
  CONSTRAINT CU_U FOREIGN KEY (UId) REFERENCES users (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT CU_C FOREIGN KEY (CId) REFERENCES compet (Id) ON DELETE CASCADE ON UPDATE RESTRICT

table scores : ([CId = int, UId = int, Round = int, Score = int])
  PRIMARY KEY (CId, UId, Round),
  CONSTRAINT S_U FOREIGN KEY (UId) REFERENCES users (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT S_C FOREIGN KEY (CId) REFERENCES compet (Id) ON DELETE CASCADE ON UPDATE RESTRICT

fun compet_new_ fs = 
  i <- nextval competSeq;
  dml(INSERT INTO compet(Id,CName,Hide) VALUES ({[i]}, {[fs.CName]}, {[False]}));
  return i

fun users_new_ fs =
  i <- nextval usersSeq;
  dml(INSERT INTO users (Id,UName,Bow,Birth,Rank,Club)
      VALUES ({[i]}, {[fs.UName]}, {[fs.Bow]}, {[fs.Birth]}, {[fs.Rank]}, {[fs.Club]}));
  return i

fun compet_register_ cid uid : transaction {} =
  u <- oneRow1(SELECT * FROM users AS U WHERE U.Id = {[uid]});
  dml(INSERT INTO compet_users (CId, UId, UName, Bow, Birth, Club, Rank, Target)
      VALUES ({[cid]}, {[u.Id]}, {[u.UName]}, {[u.Bow]}, {[u.Birth]}, {[u.Club]}, {[u.Rank]}, ""));
  dml(INSERT INTO scores (CId, UId, Round, Score) VALUES ({[cid]}, {[u.Id]}, 0, 0));
  dml(INSERT INTO scores (CId, UId, Round, Score) VALUES ({[cid]}, {[u.Id]}, 1, 0));
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
    myHeaders (
    JQuery.add (
    Bootstrap.add (
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

    val s = {
      Title = "Competitions",
      Main = url(compet_list {}),
      Users = url(users_list {}),
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
      fs <- oneRow1(SELECT * FROM compet_users AS U WHERE U.UId = {[uid]} AND U.CId = {[cid]});
      return <xml>
        <h3>{[fs.UName]}</h3>
        <p>
          <h4>Update</h4>
          {mkform <xml>
            {formgroup_def [#UName] fs "Name"}
            {formgroup_def [#Birth] fs "Birth"}
            {formgroup_def [#Club] fs "Club"}
            {formgroup_def [#Rank] fs "Rank"}
            {formgroup2 <xml><checkbox{#Propagate} checked={False}/></xml> "Also update users"}
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
      dml(UPDATE compet_users
          SET UName = {[frm.UName]}, Birth = {[frm.Birth]},
              Club = {[frm.Club]}, Rank = {[frm.Rank]}
          WHERE CId = {[cid]} AND UId = {[uid]});
      (case frm.Propagate of
       |False => return {}
       |True =>
         dml(UPDATE users
           SET UName = {[frm.UName]}, Birth = {[frm.Birth]},
               Club = {[frm.Club]}, Rank = {[frm.Rank]}
           WHERE Id = {[uid]}));
      redirect( url(registered_details cid uid) )

    fun registered_unregister _ =
      dml(DELETE FROM compet_users WHERE CId = {[cid]} AND UId = {[uid]});
      dml(DELETE FROM scores WHERE CId = {[cid]} AND UId = {[uid]});
      redirect( url (compet_register cid) )
  end

and compet_pills (me:url) cid = pills me (fn pill =>
  pill (url(compet_register cid)) <xml>Registration</xml>;
  pill (url(compet_targets cid)) <xml>Targets</xml>;
  pill (url(compet_details2 cid)) <xml>Scores</xml>;
  pill (url(compet_admin cid)) <xml>Admin</xml>;
  return {})

and compet_caption cid cap =
  fs <- X.oneRow1 (SELECT * FROM compet AS T WHERE T.Id = {[cid]});
  push <xml><h2>{[fs.CName]}</h2></xml>;
  push <xml><h3>{[cap]}</h3></xml>;
  return fs

and compet_caption_ cid cap =
  _ <- compet_caption cid cap;
  return {}

and compet_targets cid =
  let
    me <- currentUrl;
    template ( X.run (
      
      compet_caption_ cid "Target assignments";

      compet_pills me cid;

      i <- info_success "";

      ss <- tnest (
        push
          <xml><tr>
            <th></th>
            <th>ID</th>
            <th>Name</th>
            <th>Target</th>
          </tr></xml>;

        X.query (
            SELECT *
            FROM compet_users AS CU
            WHERE CU.CId = {[cid]}
            ORDER BY CU.UId
          )

          []

          (fn fs ss =>
            s <- X.source True;
            e <- X.source fs.CU.Target;
            push
              <xml><tr>
                <td><ccheckbox source={s}/></td>
                <td>{[fs.CU.UId]}</td>
                <td>{[fs.CU.UName]}</td>
                <td><ctextbox source={e}/></td>
              </tr></xml>;
            return ((s,(fs.CU.UId,e)) :: ss)
          )
      );

      push <xml>
        <button value="Assign selected" onclick={fn _ => 
          cs <- checked ss;
          P.forM_ cs (fn (id, e) => set e "blabla");
          return {}
          } />
        </xml>;

      push <xml>
        <button value="Apply" onclick={fn _ => 
          vs <- P.forM ss (fn (_,(id,e)) => v <- get e; return (id,v));
          rpc(compet_targets_apply vs);
          set i "Success";
          return {}
          } />
        </xml>;

      return {}
    ))
  where
    fun compet_targets_apply lst =
      P.forM_ lst (fn (id,v) =>
        dml(UPDATE compet_users SET Target = {[v]} WHERE CId = {[cid]} AND UId = {[id]}))
  end

and compet_admin cid =
  let
    me <- currentUrl;
    template ( X.run (
      
      fs <- compet_caption cid "Admin";

      compet_pills me cid;

      push
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
      redirect (url (compet_list {}))
  end

and compet_details2 cid =
  let
    me <- currentUrl;
    template ( X.run (

      compet_caption_ cid "Scores";

      compet_pills me cid;

      tnest (
        push
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
          FROM compet_users AS CU,
               scores AS S0,
               scores AS S1
          WHERE
                CU.CId = {[cid]}
            AND S0.CId = {[cid]}
            AND S0.Round = 0     
            AND S0.UId = CU.UId
            AND S1.CId = {[cid]}
            AND S1.Round = 1
            AND S1.UId = CU.UId
        )
          
        (fn fs =>
          s <- X.source False;
          push
            <xml><tr>
              <td><ccheckbox source={s}/></td>
              <td>{[fs.CU.UName]}</td>
              <td>{[fs.CU.Birth]}</td>
              <td>{[fs.CU.Bow]}</td>
              <td>{[fs.CU.Club]}</td>
              <td>{[fs.S0.Score]}</td>
              <td>{[fs.S1.Score]}</td>
              <td><a link={registered_details cid fs.CU.UId}>[Details]</a></td>
            </tr></xml>)
      )
    ))
  where
  end

and compet_register cid =
  let
    me <- currentUrl;
    template ( X.run (

      compet_caption_ cid "Registeration";

      compet_pills me cid;

      tnest (
        push 
          <xml><tr>
            <th>Name</th>
            <th>Birth</th>
            <th>Bow</th>
            <th>Club</th>
            <th></th>
          </tr></xml>;

        X.query_ (SELECT * FROM compet_users AS CU WHERE CU.CId = {[cid]})
          (fn fs =>
            push
              <xml><tr>
                <td>{[fs.CU.UName]}</td>
                <td>{[fs.CU.Birth]}</td>
                <td>{[fs.CU.Bow]}</td>
                <td>{[fs.CU.Club]}</td>
                <td><a link={registered_details cid fs.CU.UId}>[Details]</a></td>
              </tr></xml>);
         return {});

      ss <- X.source "";
      ss2 <- X.source [];

      push
        <xml><div>
          <ctextbox source={ss}/>
          <button value="Search" onclick={fn _ =>
            v <- get ss;
            ls <- rpc (users_search cid v);
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
                {[x.UName]} ({[x.Birth]})
              </xml>
              ))
           }/>
        </div></xml>;

      return {}
      ))

  where
  end

and compet_new fs = 
  _ <- compet_new_ fs;
  redirect ( url (compet_list {}))

and compet_list {} : transaction page = 
  template (
    t <- mktab (SELECT * FROM compet AS T) (
        <xml>
          <tr>
            <th>ID</th>
            <th>Name</th>
            <th></th>
            <th></th>
          </tr>
        </xml>)

        (fn fs =>
        <xml>
          <tr>
            <td>{[fs.T.Id]}</td>
            <td>{[fs.T.CName]}</td>
            <td> <a link={compet_register fs.T.Id}>[Details]</a> </td>
            <td> <a link={compet_details2 fs.T.Id}>[Scores]</a> </td>
          </tr>
        </xml>);
    return
    <xml>
      {mkrow t}
      {mkrow (mkform
      <xml>
        {formgroup [#CName] "Name"}
        <submit action={compet_new} value="Create"/>
      </xml>)}
    </xml>)

and users_list {} : transaction page =
  let
    template (

      t <- mktab (SELECT * FROM users AS T) (
          <xml>
            <tr>
              <th>ID</th>
              <th>Name</th>
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
              <td>{[fs.T.UName]}</td>
              <td>{[fs.T.Birth]}</td>
              <td>{[fs.T.Bow]}</td>
              <td>{[fs.T.Rank]}</td>
              <td>{[fs.T.Club]}</td>
              <td> <a link={users_detail fs.T.Id}>[Details]</a> </td>
            </tr>
          </xml>);

      return <xml>

          {mkrow t}

          {mkrow (mkform
          <xml>
            {formgroup [#UName] "UName"}
            {formgroup [#Birth] "Birth"}
            {formgroup [#Bow] "Bow"}
            {formgroup [#Rank] "Rank"}
            {formgroup [#Club] "Club"}
            <submit action={users_new} value="Create"/>
          </xml>)}

        </xml>)

  where

    fun users_new fs : transaction page = 
      _ <- users_new_ fs;
      redirect ( url (users_list {}))

    fun users_detail uid : transaction page = 
      let
        template (return
        <xml>
          {mkform
          <xml>
            <submit action={users_delete} value="Delete user"/>
          </xml>}
        </xml>)
      where
        fun users_delete _ : transaction page =
          dml(DELETE FROM users WHERE Id = {[uid]});
          redirect ( url (users_list {}))
      end
  end

and users_search (cid:int) (s:string) : transaction (list (record user)) =
  fs <- queryL(
    SELECT * FROM users AS U,
      (SELECT U.Id AS I, COUNT(CU.CId) AS N
       FROM users AS U LEFT JOIN compet_users AS CU ON U.Id = CU.UId AND CU.CId = {[cid]}
       WHERE U.UName LIKE {["%" ^ s ^ "%"]} GROUP BY U.Id) AS SS
    WHERE U.Id = SS.I AND SS.N = 0);
  return (List.mp (fn x => x.U) fs)

and main {} : transaction page = redirect (url (compet_list {}))

and about {} : transaction page = return <xml><body>About page</body></xml>

and init {} : transaction page =
  dml(DELETE FROM compet WHERE Id>0);
  dml(DELETE FROM users WHERE Id>0);
  c1 <- compet_new_ {CName="Competition 1"};
  c2 <- compet_new_ {CName="Competition 2"};
  u1 <- users_new_ {UName="Иванов Пётр", Bow="Классика", Birth="03.04.1998", Rank="1", Club="СДЮШОР8"};
  u2 <- users_new_ {UName="Степанов Степан", Bow="Классика", Birth="03.04.1997", Rank="1", Club="СДЮШОР8"};
  u3 <- users_new_ {UName="Петров Иван", Bow="Классика", Birth="03.04.1997", Rank="1", Club="СДЮШОР8"};
  u4 <- users_new_ {UName="Дмитриев Дмитрий", Bow="Блок", Birth="03.04.1978", Rank="1", Club="СДЮШОР8"};
  u5 <- users_new_ {UName="Петров Пётр", Bow="Блок", Birth="03.04.1988", Rank="2", Club="СДЮШОР8"};
  compet_register_ c1 u1;
  compet_register_ c1 u2;
  compet_register_ c1 u3;
  compet_register_ c2 u1;
  compet_register_ c2 u4;
  compet_register_ c2 u5;
  redirect (url (compet_list {}))


