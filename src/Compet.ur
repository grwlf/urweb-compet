

con user = [UName = string, Bow = string , Birth = string]

table usersTab : ([Id = int] ++ user)

sequence usersSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet

sequence competSeq

table compet_users : ([CId = int, UId = int])

sequence competUsersSeq


val st = {
  Title = "Competitions",
  Main = bless "/Compet/main",
  Users = bless "/Compet/users",
  About = bless "/Compet/about"
  }

fun compet_register cid frm =
  dml(INSERT INTO compet_users (CId, UId) VALUES ({[cid]},{[readError frm.UId]}));
  compet_details ("Registered " ^ (show frm.UId)) cid

and compet_details s cid =
  let
    Templ.template st (
      fs <- oneRow(SELECT * FROM compet AS T WHERE T.Id = {[cid]});

      cu <- queryX (SELECT * FROM compet_users AS T, usersTab AS U WHERE T.UId = U.Id AND T.CId = {[cid]}) (fn fs =>
        <xml>
          <tr>
            <td>{[fs.U.UName]}</td>
            <td>{[fs.U.Birth]}</td>
            <td>{[fs.U.Bow]}</td>
          </tr>
        </xml>);

      ss <- source "";

      return <xml>
        {[s]}

        <br/>

        Details: {[fs.T.CName]}

        <h2>Registered users</h2>

        <table border={1}>
          <tr>
            <th>Name</th>
            <th>Birth</th>
            <th>Bow</th>
          </tr>
          {cu}
        </table>

        <div>
          <ctextbox source={ss}/>
          <button value="Search" onclick={fn _ => v <- get ss;
                                          id <- rpc (users_search v);
                                          alert ( "Now inserting #" ^ (show id) )
                                          }/>
          
        </div>

        <form>
          <textbox{#UId}/>
          <submit action={compet_register cid} value="Register"/>
        </form>

        <h2>Competition parameters</h2>

        <form>
          <li>
            <textbox{#Txt} value={show fs.T.CName}/>
          </li>
          <submit action={save} value="Update"/>
        </form>

        <form>
          Delete whole competition
          <submit action={delete} value="Do it"/>
        </form>

      </xml>
      )

  where

    fun save new : transaction page =
      dml(UPDATE compet SET CName = {[new.Txt]} WHERE Id = {[cid]});
      compet_details "Saved" cid

    fun delete _ : transaction page =
      dml(DELETE FROM compet WHERE Id = {[cid]});
      compet_list "Deleted"

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
      compet_list "Inserted"
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
            <submit action={users_new} value="Create"/>
          </form>
        </xml>
    )
  where

    fun users_new fs : transaction page = 
      i <- nextval usersSeq;
      dml(INSERT INTO usersTab (Id,UName,Bow,Birth) VALUES ({[i]}, {[fs.UName]}, {[fs.Bow]}, {[fs.Birth]}));
      users_list "Inserted"

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
          users_list "Deleted"
      end
  end

and users_search (s:string) : transaction int =
  fs <- oneRow(SELECT * FROM usersTab AS U WHERE U.UName LIKE {["%" ^ s ^ "%"]});
  return fs.U.Id

and users {} : transaction page = users_list ""

and main {} : transaction page = compet_list ""

and about {} : transaction page = return <xml><body>About page</body></xml>

