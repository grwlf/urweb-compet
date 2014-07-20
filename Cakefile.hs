
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
    allow url "/Compet/main"
    allow url "/Compet/users"
    allow url "/Compet/about"
    database ("dbname="++(takeBaseName pn))
    safeGet "Compet.ur" "main"
    sql (pn.="sql")
    library' (externalMake "lib/uru3/Bootstrap/lib.urp")
    ur (sys "list")
    ur (sys "string")
    bin ("src/Compet.css") [NoScan]
    ur (pair "src/Templ.ur")
    ur (pair "src/Crud.ur")
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
    shell [cmd|urweb -tc -dumpTypes $(string "Compet")|]


  rule $ do
    phony "all"
    depend a


