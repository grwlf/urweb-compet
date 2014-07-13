

con user = [UName = string, Bow = string , Birth = string]

table users : ([Id = int] ++ user)

sequence userSeq

con compet = [Id = int, CName = string, Hide = bool]

table compet : compet

sequence competSeq

table compet_users : ([CId = int, Uid = int] ++ user)

fun bla {} : transaction page = 
  return <xml><body>asdasd</body></xml>

val statics : Templ.static = { Title = "Competitions", Main = url (bla {}), About = url (bla {}) }

structure C = Crud.Make(
  struct
     val tab = compet

     val seq = competSeq
               
     val cols = {CName = Crud.string "Name", Hide = Crud.bool "Hide?"}

     val st = statics
  end)

fun main {} : transaction page = C.main ()

fun about {} : transaction page = return <xml><body>About page</body></xml>

