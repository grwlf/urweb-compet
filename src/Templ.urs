

type static = { Main : url, About : url, Title : string }

val template : static -> transaction xbody -> transaction page

