{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Funky.Machine where

import           Data.Default
import           Data.Maybe
import           Data.Tensor.TypeLevel
import qualified Data.Vector as V

import           Funky.Instruction

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

data Step a where
  Step :: (Tap t, Functor t) => (TFun t a a) -> t Int -> Step a

newtype Machine a = Machine
  { instructions :: V.Vector a }

fromList :: [Step a] -> Executable a
fromList = Machine . V.fromList

-- | Source code of a machine, that can be modified, written to/from a file.
type Program = Machine Instruction

-- | Machine that is ready for computing values of type @a@ .
type Executable a = Machine (Step a)



{-|

evaluate the executable machine into vector of results.

-}

eval :: forall a. Default a => Machine (Step a) -> V.Vector a
eval (Machine insts) = ret
  where
    ret :: V.Vector a
    ret = V.imap compute insts

    compute :: Int -> Step a -> a
    compute idx inst = case inst of
      Step f idxs -> f `tap` fmap (get . (idx-)) idxs

    get :: Int -> a
    get addr = maybe def id (ret V.!? addr)
