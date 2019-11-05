module Latte.Error where

import Latte.Types.Latte

emph :: String -> String
emph s = "\x1b[1m" <> s <> "\x1b[0m"

typeMatchError :: Type -> Type -> String
typeMatchError t1 t2 =
  "Opsie Whoopsie x_x I kant metch typz!! me wnted " <> emph (show t1) <> " but @daddy@ giv " <> emph (show t2) <> " but thx anyway Xoxox"

undefinedVar :: Id -> String
undefinedVar i =
  "wats thz little boi " <> emph (iName i) <> " uwu"
