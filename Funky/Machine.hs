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

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap f (vx :~ x) = f x `tap` vx


data Inst a where
  Nop :: Inst a
  Inst :: (Tap t, Functor t) => (TFun t a) -> t Int -> Inst a

fromList :: [Inst a] -> Machine a
fromList = Machine . V.fromList

data Machine a = Machine
  { instructions :: V.Vector (Inst a) }

{-|

'eval' turns the list of instructions into vector of results.

-}

eval :: forall a. Default a => Machine a -> V.Vector a
eval (Machine insts) = ret
  where
    ret :: V.Vector a
    ret = V.imap compute insts

    compute :: Int -> Inst a -> a
    compute idx inst = case inst of
      Nop -> def
      Inst f idxs -> f `tap` fmap (get . (idx-)) idxs

    get :: Int -> a
    get addr = maybe def id (ret V.!? addr)


imm :: a -> Inst a
imm x = Inst x Vec

una :: (a -> a) -> Int -> Inst a
una f x = Inst f $ vec1 x 

bin :: (a -> a -> a) -> Int -> Int -> Inst a
bin f x y = Inst f $ vec2 y x
