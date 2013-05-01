{-# LANGUAGE ScopedTypeVariables #-}

module Funky.Machine where

import           Data.Default
import qualified Data.Vector as V

data Inst a
  = Nop
  | Unary (a -> a) Int
  | Binary (a -> a -> a) Int Int


data Machine a = Machine
  { instructions :: V.Vector (Inst a) }

eval :: forall a. Default a => Machine a -> V.Vector a
eval Machine insts = ret
  where
    ret :: V.Vector a
    ret = V.map compute insts

    compute :: Inst a
    compute inst = case inst of
      Nop -> def
      Unary f a0 -> f $ get a0
      Binary f a0 a1 -> f (get a0) (get a1)
