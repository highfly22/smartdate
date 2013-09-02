{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
import Data.Typeable;
import Data.Maybe;

class Typeable a => XTerm a where
  xapply :: (XTerm b, Typeable b, XTerm r, Typeable r) => r -> a -> b -> Maybe r

data FunBox = forall a b. (XTerm a, XTerm b) => FB (a -> b)
            deriving Typeable

i2c :: Int -> Char
i2c = (toEnum :: Int -> Char)

instance XTerm FunBox where
  xapply r (FB a) b = Nothing

instance XTerm Int where
  xapply r a b = Nothing

instance XTerm Char where
  xapply r a b = Nothing

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

data EnumBox = forall s. (XTerm s, XEnum s, Show s) => EB s
               deriving Typeable

instance Show EnumBox where
  show (EB s) = "EB " ++ show s

instance XEnum EnumBox where
  xsucc (EB a) = EB . xsucc $ a
  xpred (EB a) = EB . xpred $ a
  xenumFrom (EB a) = foldr (\a b -> EB a : b) [] (xenumFrom a)

instance XTerm EnumBox where
  xapply r a b = Nothing 

