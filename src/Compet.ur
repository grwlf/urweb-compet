
style invisible
style visible

fun swap a b c = a c b

con user_base = [UName = string, Bow = string , Birth = string, Club = string]

con user = ([Id = int] ++ user_base)

val show_user : show (record user) = mkShow (fn s => (show s.Id) ^ " " ^ (show s.UName))

table usersTab : (user)
  PRIMARY KEY  Id

sequence usersSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet
  PRIMARY KEY Id

sequence competSeq

table compet_users : ([CId = int, UId = int] ++ user_base)
  PRIMARY KEY (CId, UId)

sequence competUsersSeq

val st = {
  Title = "Competitions",
  Main = bless "/Compet/main",
  Users = bless "/Compet/users",
  About = bless "/Compet/about"
  }

fun compet_register (cid:int) frm : transaction page =
  u <- oneRow1(SELECT * FROM usersTab AS U WHERE U.Id = {[readError frm.UId]});
  dml(INSERT INTO compet_users (CId, UId, UName, Bow, Birth, Club)
      VALUES ({[cid]}, {[u.Id]}, {[u.UName]}, {[u.Bow]}, {[u.Birth]}, {[u.Club]}));
  redirect ( url (compet_details "" cid))

and registered_details (cid:int) (uid:int) : transaction page =
  let
    Templ.template st (
      fs <- oneRow1(SELECT * FROM compet_users AS U WHERE U.UId = {[uid]} AND U.CId = {[cid]});
      return <xml>
        <h3>{[fs.UName]}</h3>
        <p>
          <h4>Update</h4>
          <form>
            <li> Name : <textbox{#UName} value={fs.UName}/> </li>
            <li> Birth : <textbox{#Birth} value={fs.Birth}/> </li>
            <li> Club : <textbox{#Club} value={fs.Club}/> </li>
            <li> <checkbox{#Propagate} checked={False}/> </li>
            <submit action={registered_update} value="Update"/>
          </form>
        </p>
        <p>
          <h4>Unregister</h4>
          <form>
            <submit action={registered_unregister} value="Unregister"/>
          </form>
        </p>
      </xml>
    )
  where
    fun registered_update frm =
      dml(UPDATE compet_users
          SET UName = {[frm.UName]}, Birth = {[frm.Birth]}, Club = {[frm.Club]}
          WHERE CId = {[cid]} AND UId = {[uid]});
      (case frm.Propagate of
       |False => return {}
       |True =>
         dml(UPDATE usersTab
           SET UName = {[frm.UName]}, Birth = {[frm.Birth]}, Club = {[frm.Club]}
           WHERE Id = {[uid]}));
      redirect( url(registered_details cid uid) )

    fun registered_unregister _ =
      dml(DELETE FROM compet_users WHERE CId = {[cid]} AND UId = {[uid]});
      redirect(url (compet_details ("Unregistered " ^ (show uid)) cid))
  end

and compet_details s cid =
  let
    Templ.template st (
      fs <- oneRow(SELECT * FROM compet AS T WHERE T.Id = {[cid]});

      cu <- queryX (SELECT * FROM compet_users AS CU WHERE CU.CId = {[cid]}) (fn fs =>
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
      bt1 <- source invisible;
      bt2 <- source visible;

      return <xml>
        {[s]}

        <br/>

        <h2>{[fs.T.CName]}</h2>

        <button dynClass={signal bt2} onclick={fn _ => 
          set bt1 visible; set bt2 invisible}>Edit</button>
        <button dynClass={signal bt1} onclick={fn _ => 
          set bt2 visible; set bt1 invisible}>Hide</button>
        <div dynClass={signal bt1}>
          <p>
            <h3>Change name</h3>
            <form>
            <li>
            <textbox{#Txt} value={show fs.T.CName}/>
            </li>
            <submit action={save} value="Update"/>
            </form>
          </p>
          <p>
            <h3>Delete (Warning!)</h3>
            <form>
            <submit action={delete} value="Do it"/>
            </form>
          </p>
        </div>

        <h3>Registered users</h3>

        <table border={1}>
          <tr>
            <th>Name</th>
            <th>Birth</th>
            <th>Bow</th>
            <th>Club</th>
            <th></th>
          </tr>
          {cu}
        </table>

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
                <form>
                  {[x.UName]} ({[x.Birth]})
                  <hidden{#UId} value={show x.Id}/>
                  <submit action={compet_register cid} value="Register"/>
                </form>
              </xml>
              ))
           }/>
        </div>

      </xml>
      )

  where

    fun save new : transaction page =
      dml(UPDATE compet SET CName = {[new.Txt]} WHERE Id = {[cid]});
      redirect ( url (compet_details "Saved" cid))

    fun delete _ : transaction page =
      dml(DELETE FROM compet WHERE Id = {[cid]});
      redirect (url (compet_list "Deleted"))

  end

and compet_list (s:string) : transaction page = 
  let
    Templ.template st (
      rows <- queryX (SELECT * FROM compet AS T) (fn fs => <xml>
          <tr>
            <td>{[fs.T.Id]}</td>
            <td>{[fs.T.CName]}</td>
            <td> <a link={compet_details "" fs.T.Id}>[Details]</a> </td>
          </tr>
        </xml>);
      return <xml>
        {[s]}
        <table border={1}>
          <tr>
            <th>ID</th>
            <th>Name</th>
          </tr>
          {rows}
        </table>

        <form>
          <li>
            Name : <textbox{#CName}/>
          </li>
          <submit action={new} value="Create"/>
        </form>
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
        rows <- queryX (SELECT * FROM usersTab AS T) (fn fs => <xml>
            <tr>
              <td>{[fs.T.Id]}</td>
              <td>{[fs.T.UName]}</td>
              <td>{[fs.T.Birth]}</td>
              <td>{[fs.T.Bow]}</td>
              <td>{[fs.T.Club]}</td>
              <td> <a link={users_detail fs.T.Id}>[Details]</a> </td>
            </tr>
          </xml>);

        return <xml>
          {[s]}
          <table border={1}>
            <tr>
              <th>ID</th>
              <th>Name</th>
            </tr>
            {rows}
          </table>

          <form>
            <li> Name : <textbox{#UName}/> </li>
            <li> Birth : <textbox{#Birth}/> </li>
            <li> Bow : <textbox{#Bow}/> </li>
            <li> Club : <textbox{#Club}/> </li>
            <submit action={users_new} value="Create"/>
          </form>
        </xml>
    )
  where

    fun users_new fs : transaction page = 
      i <- nextval usersSeq;
      dml(INSERT INTO usersTab (Id,UName,Bow,Birth,Club)
          VALUES ({[i]}, {[fs.UName]}, {[fs.Bow]}, {[fs.Birth]}, {[fs.Club]}));
      redirect ( url (users_list "Inserted"))

    fun users_detail uid : transaction page = 
      let
        Templ.template st (return <xml>
        <form>
          <submit action={users_delete} value="Delete"/>
        </form>
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

