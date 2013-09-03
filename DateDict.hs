module DateDict
       where

import AUG
import Data.Dynamic

last :: Dict
last = [ toDyn (xsucc :: EnumBox -> EnumBox) ]
