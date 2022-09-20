module Ficha4 where

import Data.Char
import Data. 

digitAlpha :: String -> (String,String)
digitAlpha (x:xs) | 65<=ord x<=90 || 97<=ord x<=122 = (x,"") 