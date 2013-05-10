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

{-| Tuple apply.
    'Tap' @t@ states that @t@ is a data constructor like a homogeneous tuple,
    that supports uncurried function application.
 -}
class Tap t where
  -- | The type of the curried function     
  --   that can be applied to @t a@.
  type TFun t a :: *        
  -- | Apply 'TFun t a' to 't a' .
  tap :: TFun t a -> t a -> a

instance Tap Vec where
  type TFun Vec a = a
  tap f _ = f

instance Tap [] where
  type TFun [] a = [a] -> a
  tap f xs = f xs


instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap f (vx :~ x) = f x `tap` vx

data Step a where
  Step :: (Tap t, Functor t) => (TFun t a) -> t Int -> Step a

fromList :: [Step a] -> Executable a
fromList = Machine . V.fromList

type Executable a = Machine (Step a)

newtype Machine a = Machine
  { instructions :: V.Vector a }

{-|

'eval' the executable machine into vector of results.

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


imm :: a -> Step a
imm x = Step x Vec

una :: (a -> a) -> Int -> Step a
una f x = Step f $ vec1 x 

bin :: (a -> a -> a) -> Int -> Int -> Step a
bin f x y = Step f $ vec2 y x
