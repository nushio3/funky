{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Funky.Machine where

import           Data.Default
import           Data.Maybe
import           Data.Tensor.TypeLevel
import qualified Data.Vector as V

{-| Class of type constructor @t@ such that @t a@ is a homogeneous tuple -}
class Tap t where
  type TFun t a :: *        
  tap :: t a -> TFun t a -> a

instance Tap Vec where
  type TFun Vec a = a
  tap _ f = f

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap (vx :~ x) f = vx `tap` f x




data Inst a
  = Nop
  | Imm a
  | Unary (a -> a) Int
  | Binary (a -> a -> a) Int Int

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
      Imm f -> f
      Unary f a0 -> f $ get (idx-a0)
      Binary f a0 a1 -> f (get $ idx-a0) (get $ idx-a1)

    get :: Int -> a
    get addr = maybe def id (ret V.!? addr)
