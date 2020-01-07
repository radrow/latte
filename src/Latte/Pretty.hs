module Latte.Pretty
  ( module PP
  , pp
  , emph
  ) where

import Text.PrettyPrint.HughesPJClass as PP
import Prelude hiding ((<>))

emph :: Doc -> Doc
emph s = text "\x1b[1m" <> s <> text "\x1b[0m"

pp :: Pretty p => p -> String
pp = render . pPrint
