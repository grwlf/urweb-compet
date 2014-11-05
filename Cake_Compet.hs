
module Cake_Compet where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import qualified Cake_Prelude as Prelude hiding(main)
import Cake_Compet_P

theapp prelude bootstrap monadpack = do
  let pn = file "Compet.urp"
  uwapp "-dbms postgres" pn $ do
    allow mime "text/javascript";
    allow mime "text/css";
    allow mime "image/jpeg";
    allow mime "image/png";
    allow mime "image/gif";
    allow mime "application/octet-stream";
    allow url "/Compet/*"
    allow url "http://github.com*"
    allow url "http://impredicative.com*"
    allow url "http://hit.msk.ru*"
    database ("dbname="++(takeBaseName pn))
    safeGet (file "Compet.ur") "main"
    safeGet (file "Compet.ur") "init"
    sql (pn.="sql")
    bootstrap
    monadpack
    prelude
    ur (sys "list")
    ur (sys "string")
    ur (sys "option")
    -- ur (single (file "src/Prelude.ur"))
    ur (single (file "src/XmlGen.ur"))
    bin (file "src/Compet.css") [NoScan]
    ur (single (file "src/StyleSoup.ur"))
    ur (single (file "src/DragTable.ur"))
    ur (pair (file "src/Compet.ur"))


main = writeMake (file "Makefile") $ do
  p <- Prelude.thelib
  a <- theapp
    (library p)
    (library' (externalMakeTarget (file "lib/uru3/Bootstrap/lib.urp") "lib"))
    (library' (externalMakeTarget (file "lib/urweb-monad-pack/lib.urp") "lib"))
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
    depend a
    



