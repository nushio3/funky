{-# LANGUAGE ScopedTypeVariables #-}

module Funky.Machine where

import           Data.Default
import           Data.Maybe
import qualified Data.Vector as V

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
