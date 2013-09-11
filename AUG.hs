{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module AUG
       ( Dict
       , deduce
       , EnumBox
       , XEnum(..)
       , Gloss(..)
       , toGloss
       ) where

import Data.Dynamic
import Data.Typeable.Internal
import Unsafe.Coerce
import Data.Maybe
import GHC.Base

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

type Context = [Dynamic]

data Gloss =  Gloss TypeRep (Context -> Any)

instance Show Gloss where
  show (Gloss t f) = "(Gloss " ++ show t ++ ")"

toGloss :: Typeable a => (Context -> a) -> Gloss
toGloss v = Gloss (typeOf v) (unsafeCoerce v)

t :: Context -> Int
t c = 1

always1 :: Int -> Int
always1 a = 2

f :: Context -> Int -> Int
f c = always1

deduceGloss :: Gloss -> Gloss -> Maybe Gloss
deduceGloss (Gloss funcT func) (Gloss argT arg) =  case funResultTy (realtype funcT) (realtype argT) of
  Just t -> Just $ Gloss t (\c ->  ((unsafeCoerce func) c) ((unsafeCoerce arg) c))
  Nothing -> Nothing
  where realtype = ((!! 1) . snd . splitTyConApp)

-- fromDyn (dynApp (toDyn (xsucc :: EnumBox -> EnumBox) ) (toDyn (EB 'a'))) (EB (1 ::Int))
-- (dynApply (toDyn (xsucc :: EnumBox -> EnumBox) ) (toDyn 'a'))
