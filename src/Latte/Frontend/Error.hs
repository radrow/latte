{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Latte.Frontend.Error where

import Latte.Frontend.AST
import Latte.Pretty
import Prelude hiding ((<>))

data Error
  = MainType
  | NoMain
  | TypeMatch Type Type
  | OperatorTypeMatch AnyOp [(Type, Type)] (Type, Type)
  | ArgNumFun FunId Int Int
  | ArgNumMethod ClassId MethodId Int Int
  | ArgNumConstructor ClassId (Maybe ConstructorId) Int Int
  | DuplicateVar VarId
  | DuplicateFun FunId
  | DuplicateClass ClassId
  | DuplicateField ClassId FieldId
  | DuplicateMethod ClassId MethodId
  | DuplicateConstructor ClassId (Maybe ConstructorId)
  | ClassMatch ClassId ClassId
  | UndefinedVar VarId
  | UndefinedFun FunId
  | UndefinedClass ClassId
  | UndefinedField ClassId FieldId
  | UndefinedMethod ClassId MethodId
  | UndefinedConstructor ClassId (Maybe ConstructorId)
  | NoReturn
  | NotAClass Type
  | NotInClass

instance Pretty Error where
  pPrint = \case
    MainType ->
      "plz man cant u into c? ? ? " <> emph "main" <> " muzzt B " <> emph "int main()"
    NoMain ->
      emph "main" <> " where r u"
    TypeMatch t1 t2 ->
      "Opsie Whoopsie x_x I kant metch typz!! me wnted " <>
      emph (pPrint t1) <> " but @daddy@ gived " <> emph (pPrint t2) <> " but thx anyway Xoxox"

    OperatorTypeMatch o goods (badL, badR) ->
      "Dud, " <> emph (pPrint o) <> " hav soooo MANY winky-chicky suits ;3 look at them *.* " <>
      emph (vcat [pPrint l <+> pPrint o <+> pPrint r | (l, r) <- goods]) $+$
      " and u giv " <>
      emph (pPrint badL <+> pPrint o <+> pPrint badR) <+> " -,-"

    ArgNumFun f want giv ->
      "boiiii cant you count. " <> emph (pPrint f) <> " wnt " <> emph (int want) <>
      " argz and u gib me " <> emph (int giv)

    ArgNumMethod cl m want giv ->
      emph (pPrint m) <> " 'o " <> emph (pPrint cl) <> " cant stand " <> emph (int giv) <> " argz uwu " <>
      "butt only " <> emph (int want) <> " :c"

    ArgNumConstructor cl cr want giv ->
      "cant build " <> emph (pPrint cl) <> " wiv " <> emph (maybe "noname constructor" pPrint cr) <>
      " and its sweeeeet bff team ^.^ of " <> emph (int giv) <> " argz. U need " <> emph (int want) <> " boiz&gurlz."

    DuplicateVar i ->
      "why r u doin " <> emph (pPrint i) <> " too much"

    DuplicateFun i ->
      "i know " <> emph (pPrint i) <> ". gimme some other boi lol"

    DuplicateClass i ->
      "ok man. i get u looooooooov xoxoxox kawaii " <> emph (pPrint i) <> " but 1 is just enough"

    DuplicateField c i ->
      "OMG YOU GIV A FRIEND TO " <> emph (pPrint i) <> " OwO <3 <3 (Y) how sw33t ;D ;D" <>
      " related claz " <> emph (pPrint c) <> " sooo proud X_X but the lAtTi-cOmPi is a duplicatephobo :c :'c"

    DuplicateMethod c i ->
      "MAN WHAT THE F XDXDXDDDD WHY SECOND " <> emph (pPrint i) <> " WTF WHY X'DD " <>
      emph (pPrint c) <> " is ful of it man plz"

    DuplicateConstructor cl (Just i) ->
      "Your constructor " <> emph (pPrint i) <> " of class " <> emph (pPrint cl) <> " seems to be redefined sir."
    DuplicateConstructor cl Nothing ->
      "ErRORrrr iN CLAS " <> emph (pPrint cl) <> ". . . . " <> emph ("noname constructor") <> " iS DUPliCatED"

    ClassMatch c1 c2 ->
      "i rly wish " <> emph (pPrint c1) <> " be @daddy@ of " <> emph (pPrint c2) <> " but itz not ;( ;((("

    UndefinedVar i ->
      "wats thz little boi " <> emph (pPrint i) <> " uwu"

    UndefinedFun i ->
      "Ich kenne keine " <> emph (pPrint i) <> " Funktion. Können Sie es implementieren?"

    NoReturn ->
      "som sneaky [B]oi can escape hier ;--; Mommy plz " <> emph ("return") <> " here  I lov u"

    UndefinedClass c ->
      "XDDD WHAT THE HECK THIS " <> emph (pPrint c) <> " NOT EVEN A CLASS BRO LOLZ ROTFL"

    NotAClass t ->
      "cant hit that babe " <> emph (pPrint t) <> " bcoz itz not classy :c"

    UndefinedField c i ->
      "u cant hev evryfffing bro, for exampl " <> emph (pPrint c) <> " cant hev " <> emph (pPrint i)

    UndefinedMethod c i ->
      "meffod nott ffund :( " <> emph (pPrint c) <> " misses " <> emph (pPrint i) <> " rip"

    UndefinedConstructor c (Just i) ->
      emph (pPrint i) <> " is not de wey. to get " <> emph (pPrint c) <> " u need smth els"
    UndefinedConstructor c Nothing ->
      "no unnamed bois allowd in " <> emph (pPrint c)

    NotInClass ->
      emph "this" <> " isnt a claz xC xC xC"
