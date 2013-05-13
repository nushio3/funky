{-# LANGUAGE TupleSections #-}
module Control.Funky.Compiler.Instances where

import qualified Data.Default as Default

import           Control.Funky.Compiler
import           Control.Funky.Instruction
import           Control.Funky.Type
import           Data.Tensor.TypeLevel
import           Prelude hiding (read)
import qualified Safe

nop :: Default.Default a => PartialCompiler a
nop = ("Nop", go) where
  go Nop = Just $ Thunk Default.def Vec
  go _   = Nothing

def :: Default.Default a => PartialCompiler a
def = ("Default",go) where
  go _ = Just $ Thunk Default.def Vec

ord :: (Ord a, Num a) => PartialCompiler a
ord = ("Ord", go) where
  go (Select xs) = Just $ Thunk (\x y z -> if x<0 then y else z) xs
  go (Theta xs)  = Just $ Thunk (\x -> if x >0 then 1 else 0) xs
  go _           = Nothing

imm :: PartialCompiler a
imm = ("Imm", go) where
  go (Imm x) = Just $ Thunk x Vec
  go _       = Nothing

num :: Num a => PartialCompiler a
num = ("Num", go) where
  go (Add xs)    = Just $ Thunk (+) xs
  go (Sub xs)    = Just $ Thunk (flip (-)) xs
  go (Mul xs)    = Just $ Thunk (*) xs
  go (Negate xs) = Just $ Thunk negate xs
  go _           = Nothing