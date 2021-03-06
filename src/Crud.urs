con colMeta = fn (db :: Type, widget :: Type) =>
                 {Nam : string,
                  Show : db -> xbody,
                  Widget : nm :: Name -> xml form [] [nm = widget],
                  WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
                  Parse : widget -> db,
                  Inject : sql_injectable db}

con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

val int : string -> colMeta (int, string)
val float : string -> colMeta (float, string)
val string : string -> colMeta (string, string)
val bool : string -> colMeta (bool, bool)

functor Make(M :
  sig
    con cols :: {(Type * Type)}
    constraint [Id] ~ cols
    val fl : folder cols

    val st : Templ.static

    table tab : ([Id = int] ++ map fst cols)

    sequence seq

    val details : $(map fst cols) -> transaction xbody

    val cols : colsMeta cols
  end) :
sig
    val main : {} -> transaction page
end
