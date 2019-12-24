module Latte.Frontend.Error where

import Latte.Frontend.AST

emph :: String -> String
emph s = "\x1b[1m" <> s <> "\x1b[0m"

typeMatchError :: Type -> Type -> String
typeMatchError t1 t2 =
  "Opsie Whoopsie x_x I kant metch typz!! me wnted " <> emph (pp t1) <> " but @daddy@ giv " <> emph (pp t2) <> " but thx anyway Xoxox"

classMatchError :: Id -> Id -> String
classMatchError Id{iName = c1} Id{iName = c2} =
  "i rly wish " <> emph c1 <> " be @daddy@ of " <> emph c2 <> " but itz not ;( ;((("

undefinedVar :: Id -> String
undefinedVar i =
  "wats thz little boi " <> emph (iName i) <> " uwu"

noReturn :: String
noReturn =
  "som sneaky [B]oi can escape hier ;--; Mommy plz " <> emph ("return") <> " here  I lov u"

undefinedClass :: Id -> String
undefinedClass Id{iName = c} =
  "XDDD WHAT THE HECK THIS " <> emph c <> " NOT EVEN A CLASS BRO LOLZ ROTFL"
