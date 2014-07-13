

type static = { Main : url, About : url, Title : string, Users : url }

val template : static -> transaction xbody -> transaction page

