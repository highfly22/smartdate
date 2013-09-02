{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
import Data.Typeable;
import Data.Dynamic;
import Data.Maybe;

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

-- fromDyn (dynApp (toDyn (xsucc :: EnumBox -> EnumBox) ) (toDyn (EB 'a'))) (EB (1 ::Int))
