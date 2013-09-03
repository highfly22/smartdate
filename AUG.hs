{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module AUG
       ( Dict
       , deduce
       , EnumBox
       , XEnum(..)
       ) where

import Data.Dynamic
import Data.Maybe

class XEnum a where
  xsucc :: a -> a
  xpred :: a -> a
  xenumFrom :: a -> [a]

instance XEnum Int where
  xsucc = succ
  xpred = pred
  xenumFrom = enumFrom

instance XEnum Char where
  xsucc = succ
  xpred = pred
  xenumFrom = enumFrom

data EnumBox = forall s. (XEnum s, Show s) => EB s
               deriving Typeable

instance Show EnumBox where
  show (EB s) = "EB " ++ show s

instance XEnum EnumBox where
  xsucc (EB a) = EB . xsucc $ a
  xpred (EB a) = EB . xpred $ a
  xenumFrom (EB a) = foldr (\a b -> EB a : b) [] (xenumFrom a)

type Dict = [Dynamic]

deduce :: Dict -> Dict -> Dict
deduce a b = [z|x <- a, y <- b, let (Just z) = dynApply x y] ++
             [z|x <- a, y <- b, let (Just z) = dynApply y x]


-- fromDyn (dynApp (toDyn (xsucc :: EnumBox -> EnumBox) ) (toDyn (EB 'a'))) (EB (1 ::Int))
-- (dynApply (toDyn (xsucc :: EnumBox -> EnumBox) ) (toDyn 'a'))
