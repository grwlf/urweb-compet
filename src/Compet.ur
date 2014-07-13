

con user = [UName = string, Bow = string , Birth = string]

table users : ([Id = int] ++ user)

sequence usersSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet

sequence competSeq

table compet_users : ([CId = int, Uid = int] ++ user)

fun bla {} : transaction page = 
  return <xml><body>asdasd</body></xml>

val statics = {
  Main = bless "/Compet/main",
  Users = bless "/Compet/users",
  About = bless "/Compet/about"
  }

structure U = Crud.Make (
  struct
     val tab = users

     val seq = usersSeq
               
     val cols = {UName = Crud.string "Name", Bow = Crud.string "BowType", Birth = Crud.string "Birth date"}

     val details = fn c => return <xml>Details: {[c.UName]}</xml>

     val st = statics ++ {Title = "Users"}
  end)

structure C = Crud.Make(
  struct
     val tab = compet

     val seq = competSeq
               
     val cols = {CName = Crud.string "Name", Hide = Crud.bool "Hide?"}

     val details = fn c => return <xml>Details: {[c.CName]}</xml>

     val st = statics ++ {Title = "Competitions"}
  end)

fun users {} : transaction page = U.main ()

fun main {} : transaction page = C.main ()

fun about {} : transaction page = return <xml><body>About page</body></xml>

