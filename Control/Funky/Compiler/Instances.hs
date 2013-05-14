{-# LANGUAGE TupleSections #-}
module Control.Funky.Compiler.Instances where

import qualified Data.Default as Default

import           Control.Funky.Compiler
import           Control.Funky.Instruction
import           Control.Funky.Type
import           Data.Tensor.TypeLevel
import           Prelude hiding (read)

nop :: Default.Default a => PartialCompiler a
nop = ("Nop", go) where
  go Nop = Just $ Thunk Default.def Vec
  go _   = Nothing

def :: Default.Default a => PartialCompiler a
def = ("Default",go) where
  go _ = Just $ Thunk Default.def Vec

imm :: PartialCompiler a
imm = ("Imm", go) where
  go (Imm x) = Just $ Thunk x Vec
  go _       = Nothing

ord :: (Ord a, Num a) => PartialCompiler a
ord = ("Ord", go) where
  go (Select xs) = Just $ Thunk (\x y z -> if x<0 then y else z) xs
  go (Theta xs)  = Just $ Thunk (\x -> if x >0 then 1 else 0) xs
  go _           = Nothing

num :: Num a => PartialCompiler a
num = ("Num", go) where
  go (Add xs)    = Just $ Thunk (+) xs
  go (Sub xs)    = Just $ Thunk (flip (-)) xs
  go (Mul xs)    = Just $ Thunk (*) xs
  go (Negate xs) = Just $ Thunk negate xs
  go _           = Nothing

fractional :: Fractional a => PartialCompiler a
fractional = ("Fractional", go) where
  go (Divide xs) = Just $ Thunk (/) xs
  go (Recip xs)  = Just $ Thunk recip xs
  go _          = Nothing

floating :: Floating a => PartialCompiler a
floating = ("Floating", go) where
  go (Pi xs)    = Just $ Thunk pi xs
  go (Exp xs)   = Just $ Thunk exp xs
  go (Sqrt xs)  = Just $ Thunk sqrt xs
  go (Log xs)   = Just $ Thunk log xs
  go (Sin xs)   = Just $ Thunk sin xs
  go (Cos xs)   = Just $ Thunk cos xs
  go (Tan xs)   = Just $ Thunk tan xs
  go (Asin xs)  = Just $ Thunk asin xs
  go (Acos xs)  = Just $ Thunk acos xs
  go (Atan xs)  = Just $ Thunk atan xs
  go (Sinh xs)  = Just $ Thunk sinh xs
  go (Cosh xs)  = Just $ Thunk cosh xs
  go (Tanh xs)  = Just $ Thunk tanh xs
  go (Asinh xs) = Just $ Thunk asinh xs
  go (Acosh xs) = Just $ Thunk acosh xs
  go (Atanh xs) = Just $ Thunk atanh xs
  go _          = Nothing
