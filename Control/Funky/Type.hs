{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Funky.Type where

import           Data.Tensor.TypeLevel
import qualified Data.Vector as V

import           Control.Funky.Instruction

{-| Tuple application.
    'Tap' @t@ states that @t@ is a data constructor like a homogeneous tuple,
    that supports uncurried function application.
 -}
class Tap t where
  -- | 'TFun t a b' is the type of the curried version of 
  --  the function of type @t a -> b@ .
  type TFun t a b :: *        
  -- | Apply 'TFun t a' to 't a' .
  tap :: TFun t a b -> t a -> b

instance Tap Vec where
  type TFun Vec a b = b
  tap f _ = f

instance Tap [] where
  type TFun [] a b = [a] -> b
  tap f = f 

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a b = a -> TFun v a b
  tap f (vx :~ x) = f x `tap` vx

data Thunk a where
  Thunk :: (Tap t, Functor t) => (TFun t a a) -> t Int -> Thunk a

newtype Machine a = Machine
  { unMachine :: V.Vector a }


-- | Source code of a machine, that can be modified, written to/from a file.
type Program = Machine Instruction

-- | Machine that is ready for computing values of type @a@ .
type Executable a = Machine (Thunk a)

-- | Machine that is fully evaluated.
type Executed a = Machine a


-- | Utility function for creating machine.
fromList :: [a] -> Machine a
fromList = Machine . V.fromList

-- | Utility function for converting a machine to a list.
toList :: Machine a -> [a] 
toList =  V.toList . unMachine


