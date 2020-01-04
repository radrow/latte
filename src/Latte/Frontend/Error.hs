module Latte.Frontend.Error where

import Latte.Frontend.AST

emph :: String -> String
emph s = "\x1b[1m" <> s <> "\x1b[0m"

mainType :: String
mainType = "plz man cant u into c? ? ? " <> emph "main" <> " muzzt B " <> emph "int main()"

noMain :: String
noMain = emph "main" <> " where r u"

typeMatchError :: Type -> Type -> String
typeMatchError t1 t2 =
  "Opsie Whoopsie x_x I kant metch typz!! me wnted " <> emph (pp t1) <> " but @daddy@ giv " <> emph (pp t2) <> " but thx anyway Xoxox"

argNumMismatch :: Int -> Int -> String
argNumMismatch want giv = "boiiii cant you count. I wnt " <> emph (show want) <> " argz and u gib me " <> emph (show giv)

duplicateVar :: Id -> String
duplicateVar i = "why r u doin " <> emph (pp i) <> " too much"

duplicateFun :: Id -> String
duplicateFun i = "i know " <> emph (pp i) <> ". gimme some other boi lol"

duplicateClass :: Id -> String
duplicateClass i = "ok man. i get u looooooooov xoxoxox " <> emph (pp i) <> " but 1 is just enough"

duplicateField :: Id -> String
duplicateField i = "field field bald field, u know like in thiz song. too much " <> emph (pp i) <> " btw"

duplicateMethod :: Id -> String
duplicateMethod i = "MAN WHAT THE F XDXDXDDDD WHY SECOND " <> emph (pp i) <> " WTF WHY X'DD"

duplicateConstructor :: Maybe Id -> String
duplicateConstructor (Just i) = "Your constructor " <> emph (pp i) <> " seems to be redefined sir."
duplicateConstructor Nothing = "Your unnamed constructor seems to be redefined sir."

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

notAClass :: Type -> String
notAClass t = "cant hit that babe " <> emph (pp t) <> " bcoz itz not classy :c"

noSuchField :: Id -> Id -> String
noSuchField c i = "u cant hev evryfffing bro, for exampl " <> emph (pp c) <> " cant hev " <> emph (pp i)


noSuchMethod :: Id -> Id -> String
noSuchMethod c i = "meffod nott ffund :( " <> emph (pp c) <> " misses " <> emph (pp i) <> " rip"


noSuchConstructor :: Id -> Maybe Id -> String
noSuchConstructor c (Just i) =
  emph (pp i) <> " is not the wey. to get " <> emph (pp c) <> " u need smth els"
noSuchConstructor c Nothing =
  "no unnamed bois allowd in " <> emph (pp c)


notInClass :: String
notInClass = emph "this" ++ " isnt a claz xC xC xC"
