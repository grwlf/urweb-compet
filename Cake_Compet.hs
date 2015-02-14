
module Cake_Compet where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import qualified Cake_Prelude as Prelude
import qualified Cake_Bootstrap as Bootstrap
import qualified Cake_MonadPack as MonadPack
import Cake_Compet_P

app = do
  let pn = file "Compet.urp"
  uwapp "-dbms postgres" pn $ do
    allow mime "text/javascript"
    allow mime "text/css"
    allow mime "image/jpeg"
    allow mime "image/png"
    allow mime "image/gif"
    allow mime "application/octet-stream"
    allow url "/Compet/*"
    allow url "http://github.com*"
    allow url "http://impredicative.com*"
    allow url "http://hit.msk.ru*"
    database ("dbname="++(takeBaseName pn))
    safeGet "Compet/main"
    safeGet "Compet/init"
    sql (pn.="sql")
    library Prelude.lib
    library Bootstrap.lib
    library MonadPack.lib
    ur (sys "list")
    ur (sys "string")
    ur (sys "option")
    ur (file "src/XmlGen.ur")
    embed (file "src/Compet.css")
    ur (file "src/StyleSoup.ur")
    ur (file "src/DragTable.ur")
    ur (file "src/Compet.ur", file "src/Compet.urs")

main = writeDefaultMakefiles $ do

  a <- app

  db <- rule $ do
    let sql = urpSql (toUrp a)
    let dbn = takeBaseName sql
    shell [cmd|dropdb --if-exists $(string dbn)|]
    shell [cmd|createdb $(string dbn)|]
    shell [cmd|psql -f $(sql) $(string dbn)|]
    shell [cmd|touch @(sql.="db")|]

  rule $ do
    phony "dropdb"
    depend db

  rule $ do
    phony "tc"
    shell [cmd|urweb -tc $(string "Compet")|]

  rule $ do
    phony "all"
    depend app
    



