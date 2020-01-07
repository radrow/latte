{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Latte.Frontend.Error where

import Control.Monad.Except
import Latte.Frontend.AST
import Latte.Pretty
import Prelude hiding ((<>))

data Error
  = MainType
  | NoMain
  | TypeMatch Type Type
  | ArgNum Id Int Int
  | DuplicateVar Id
  | DuplicateFun Id
  | DuplicateClass Id
  | DuplicateField Id
  | DuplicateMethod Id
  | DuplicateConstructor (Maybe Id)
  | ClassMatch Id Id
  | UndefinedVar Id
  | UndefinedFun Id
  | UndefinedClass Id
  | UndefinedField Id Id
  | UndefinedMethod Id Id
  | UndefinedConstructor Id (Maybe Id)
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

    ArgNum f want giv ->
      "boiiii cant you count. " <> emph (pPrint f) <> " wnt " <> emph (int want) <>
      " argz and u gib me " <> emph (int giv)

    DuplicateVar i ->
      "why r u doin " <> emph (pPrint i) <> " too much"

    DuplicateFun i ->
      "i know " <> emph (pPrint i) <> ". gimme some other boi lol"

    DuplicateClass i ->
      "ok man. i get u looooooooov xoxoxox " <> emph (pPrint i) <> " but 1 is just enough"

    DuplicateField i ->
      "field field bald field, u know like in thiz song. too much " <> emph (pPrint i) <> " btw"

    DuplicateMethod i ->
      "MAN WHAT THE F XDXDXDDDD WHY SECOND " <> emph (pPrint i) <> " WTF WHY X'DD"

    DuplicateConstructor (Just i) ->
      "Your constructor " <> emph (pPrint i) <> " seems to be redefined sir."
    DuplicateConstructor Nothing ->
      "Your unnamed constructor seems to be redefined sir."

    ClassMatch c1 c2 ->
      "i rly wish " <> emph (pPrint c1) <> " be @daddy@ of " <> emph (pPrint c2) <> " but itz not ;( ;((("

    UndefinedVar i ->
      "wats thz little boi " <> emph (pPrint i) <> " uwu"

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
      emph (pPrint i) <> " is not the wey. to get " <> emph (pPrint c) <> " u need smth els"
    UndefinedConstructor c Nothing ->
      "no unnamed bois allowd in " <> emph (pPrint c)

    NotInClass ->
      emph "this" <> " isnt a claz xC xC xC"
