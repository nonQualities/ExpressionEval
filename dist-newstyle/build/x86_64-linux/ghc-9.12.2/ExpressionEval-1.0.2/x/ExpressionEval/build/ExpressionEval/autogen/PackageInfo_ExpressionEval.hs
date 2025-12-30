{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ExpressionEval (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ExpressionEval"
version :: Version
version = Version [1,0,2] []

synopsis :: String
synopsis = "Mini-interpreter in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
