
module Cake_Compet where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import qualified Cake_Prelude as Prelude
import qualified Cake_Bootstrap as Bootstrap
import qualified Cake_MonadPack as MonadPack
import Cake_Compet_P

(app,db) = do
  let pn = file "Compet.urp"
  uwapp_postgres pn $ do
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
    safeGet "Compet/main"
    safeGet "Compet/init"
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

  rule $ do
    phony "dropdb"
    depend db

  rule $ do
    phony "tc"
    shell [cmd|urweb -tc $(string "Compet")|]

  rule $ do
    phony "all"
    depend app
    



