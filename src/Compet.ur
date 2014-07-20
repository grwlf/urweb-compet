
style invisible
style visible

val cl = CSS.list

structure B = Bootstrap

fun swap a b c = a c b

(* Utils *)

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
  return <xml>
        <button dynClass={signal bt2} onclick={fn _ => 
          set bt1 visible; set bt2 invisible}>Edit</button>
        <button dynClass={signal bt1} onclick={fn _ => 
          set bt2 visible; set bt1 invisible}>Hide</button>
        <div dynClass={signal bt1}>
          {x}
        </div>
  </xml>


(* Data defineitions *)

con user_base = [UName = string, Bow = string , Birth = string, Club = string, Rank = string]

con user = ([Id = int] ++ user_base)

val show_user : show (record user) = mkShow (fn s => (show s.Id) ^ " " ^ (show s.UName))

table usersTab : (user)
  PRIMARY KEY  Id

sequence usersSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet
  PRIMARY KEY Id

sequence competSeq

table scores : ([SId = int, Score = int])

sequence scoresSeq

table compet_users : ([CId = int, UId = int] ++ user_base ++ [SId1 = int, SId2 = int])
  PRIMARY KEY (CId, UId)

sequence competUsersSeq

val st = {
  Title = "Competitions",
  Main = bless "/Compet/main",
  Users = bless "/Compet/users",
  About = bless "/Compet/about"
  }

fun registered_details (cid:int) (uid:int) : transaction page =
  let
    Templ.template st (
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
         dml(UPDATE usersTab
           SET UName = {[frm.UName]}, Birth = {[frm.Birth]},
               Club = {[frm.Club]}, Rank = {[frm.Rank]}
           WHERE Id = {[uid]}));
      redirect( url(registered_details cid uid) )

    fun registered_unregister _ =
      dml(DELETE FROM compet_users WHERE CId = {[cid]} AND UId = {[uid]});
      redirect(url (compet_details ("Unregistered " ^ (show uid)) cid))
  end

and compet_details2 cid =
  let
    Templ.template st (
      fs <- oneRow1(SELECT * FROM compet AS T WHERE T.Id = {[cid]});
      t <- mktab (SELECT * FROM compet_users AS CU WHERE CU.CId = {[cid]}) (
        <xml>
          <tr>
            <th>Name</th>
            <th>Birth</th>
            <th>Bow</th>
            <th>Club</th>
            <th></th>
          </tr>
        </xml>
        ) (fn fs =>
        <xml>
          <tr>
            <td>{[fs.CU.UName]}</td>
            <td>{[fs.CU.Birth]}</td>
            <td>{[fs.CU.Bow]}</td>
            <td>{[fs.CU.Club]}</td>
            <td><a link={registered_details cid fs.CU.UId}>[Details]</a></td>
          </tr>
        </xml>);
      return <xml>
        <h2>{[fs.CName]}</h2>

        <h3>Scores</h3>

        {mkrow t}

      </xml>)
  where
  end

and compet_details s cid =
  let
    Templ.template st (
      fs <- oneRow1(SELECT * FROM compet AS T WHERE T.Id = {[cid]});

      t <- mktab (SELECT * FROM compet_users AS CU WHERE CU.CId = {[cid]}) (
        <xml>
          <tr>
            <th>Name</th>
            <th>Birth</th>
            <th>Bow</th>
            <th>Club</th>
            <th></th>
          </tr>
        </xml>
        ) (fn fs =>
        <xml>
          <tr>
            <td>{[fs.CU.UName]}</td>
            <td>{[fs.CU.Birth]}</td>
            <td>{[fs.CU.Bow]}</td>
            <td>{[fs.CU.Club]}</td>
            <td><a link={registered_details cid fs.CU.UId}>[Details]</a></td>
          </tr>
        </xml>);

      ss <- source "";
      ss2 <- source [];

      hp <- hidingpanel <xml>
          <p>
            <h3>Change name</h3>
            {mkform <xml>
              {formgroup_def [#CName] fs "Name"}
              <submit action={compet_save} value="Update"/>
              </xml>}
          </p>
          <p>
            <h3>Delete (Warning!)</h3>
            <form>
            <submit action={compet_delete} value="Do it"/>
            </form>
          </p>
        </xml>;

      return <xml>
        {[s]}

        <br/>

        <h2>{[fs.CName]}</h2>

        {hp}

        <h3>Registered users</h3>

        {mkrow t}

        <div>
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
                {mkform <xml>
                  {[x.UName]} ({[x.Birth]})
                  <hidden{#UId} value={show x.Id}/>
                  <submit action={compet_register} value="Register"/>
                </xml>}
              </xml>
              ))
           }/>
        </div>

      </xml>
      )

  where

    fun compet_register frm : transaction page =
      sid1 <- nextval scoresSeq;
      dml(INSERT INTO scores (SId, Score) VALUES ({[sid1]}, 0));
      sid2 <- nextval scoresSeq;
      dml(INSERT INTO scores (SId, Score) VALUES ({[sid2]}, 0));
      u <- oneRow1(SELECT * FROM usersTab AS U WHERE U.Id = {[readError frm.UId]});
      dml(INSERT INTO compet_users (CId, UId, UName, Bow, Birth, Club, Rank, SId1, SId2)
          VALUES ({[cid]}, {[u.Id]}, {[u.UName]}, {[u.Bow]}, {[u.Birth]}, {[u.Club]}, {[u.Rank]}, {[sid1]},{[sid2]}));
      redirect ( url (compet_details "" cid))

    fun compet_save new : transaction page =
      dml(UPDATE compet SET CName = {[new.CName]} WHERE Id = {[cid]});
      redirect ( url (compet_details "Saved" cid))

    fun compet_delete _ : transaction page =
      dml(DELETE FROM compet WHERE Id = {[cid]});
      redirect (url (compet_list "Deleted"))

  end

and compet_list (s:string) : transaction page = 
  let
    Templ.template st (

      t <- mktab (SELECT * FROM compet AS T) (
          <xml>
            <tr>
              <th>ID</th>
              <th>Name</th>
              <th></th>
              <th></th>
            </tr>
          </xml>) (fn fs => <xml>
            <tr>
              <td>{[fs.T.Id]}</td>
              <td>{[fs.T.CName]}</td>
              <td> <a link={compet_details "" fs.T.Id}>[Details]</a> </td>
              <td> <a link={compet_details2 fs.T.Id}>[Scores]</a> </td>
            </tr>
          </xml>);

      return <xml>

        <p class={B.bg_success}>{[s]}</p>

        {mkrow t}

        {mkrow (mkform <xml>
          {formgroup [#CName] "Name"}
          <submit action={new} value="Create"/>
        </xml>)}

      </xml>)
  where
    fun new fs = 
      i <- nextval competSeq;
      dml(INSERT INTO compet(Id,CName,Hide) VALUES ({[i]}, {[fs.CName]}, {[False]}));
      redirect ( url (compet_list "Inserted"))
  end

and users_list (s:string) : transaction page =
  let
    Templ.template st (

      t <- mktab (SELECT * FROM usersTab AS T) (
              <xml><tr>
                <th>ID</th>
                <th>Name</th>
                <th>Birth</th>
                <th>Bow</th>
                <th>Rank</th>
                <th>Club</th>
                <th></th>
              </tr></xml>) (fn fs => <xml>
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

          <p class={B.bg_success}>{[s]}</p>

          {mkrow t}

          {mkrow (mkform <xml>
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
      i <- nextval usersSeq;
      dml(INSERT INTO usersTab (Id,UName,Bow,Birth,Rank,Club)
          VALUES ({[i]}, {[fs.UName]}, {[fs.Bow]}, {[fs.Birth]}, {[fs.Rank]}, {[fs.Club]}));
      redirect ( url (users_list "Inserted"))

    fun users_detail uid : transaction page = 
      let
        Templ.template st (return <xml>
          {mkform <xml>
            <submit action={users_delete} value="Delete user"/>
          </xml>}
        </xml>)
      where
        fun users_delete _ : transaction page =
          dml(DELETE FROM usersTab WHERE Id = {[uid]});
          redirect ( url (users_list "Deleted"))
      end
  end

and users_search (cid:int) (s:string) : transaction (list (record user)) =
  fs <- queryL(
    SELECT * FROM usersTab AS U,
      (SELECT U.Id AS I, COUNT(CU.CId) AS N
       FROM usersTab AS U LEFT JOIN compet_users AS CU ON U.Id = CU.UId AND CU.CId = {[cid]}
       WHERE U.UName LIKE {["%" ^ s ^ "%"]} GROUP BY U.Id) AS SS
    WHERE U.Id = SS.I AND SS.N = 0);
  return (List.mp (fn x => x.U) fs)

and users {} : transaction page = users_list ""

and main {} : transaction page = compet_list ""

and about {} : transaction page = return <xml><body>About page</body></xml>

