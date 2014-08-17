
module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

instance IsString File where fromString = file

main = writeMake (file "Makefile") $ do
    
  let pn = "Compet.urp"

  a <- uwapp "-dbms postgres" pn $ do
    allow mime "text/javascript";
    allow mime "text/css";
    allow mime "image/jpeg";
    allow mime "image/png";
    allow mime "image/gif";
    allow mime "application/octet-stream";
    allow url "/Compet/*"
    database ("dbname="++(takeBaseName pn))
    safeGet "Compet.ur" "main"
    safeGet "Compet.ur" "init"
    sql (pn.="sql")
    library' (externalMakeTarget "lib/uru3/Bootstrap/lib.urp" "lib")
    library' (externalMakeTarget "lib/urweb-monad-pack/lib.urp" "lib")
    ur (sys "list")
    ur (sys "string")
    ur (single "src/Prelude.ur")
    ur (single "src/XmlGen.ur")
    bin ("src/Compet.css") [NoScan]
    ur (pair "src/Compet.ur")

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


