{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
import Data.Typeable;

data EnumBox = forall s. (Enum s, Show s) => EB s
               deriving Typeable

instance Show EnumBox where
  show (EB s) = "EB " ++ show s

next :: EnumBox -> EnumBox
next (EB a) = succ a

