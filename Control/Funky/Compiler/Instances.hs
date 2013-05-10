module Control.Funky.Compiler.Instances where

import qualified Data.Default as Default

import           Control.Funky.Compiler
import           Control.Funky.Instruction
import           Control.Funky.Type
import           Data.Tensor.TypeLevel
import           Prelude hiding (read)
import qualified Safe

nop :: Default.Default a => PartialCompiler a
nop Nop = Just $ Thunk Default.def Vec
nop _   = Nothing

def :: Default.Default a => PartialCompiler a
def _ = Just $ Thunk Default.def Vec

ord :: (Ord a, Num a) => PartialCompiler a
ord (Select xs) = Just $ Thunk (\x y z -> if x<0 then y else z) xs
ord (Theta xs)  = Just $ Thunk (\x -> if x >0 then 1 else 0) xs
ord _           = Nothing

read :: Read a => PartialCompiler a
read (Imm str) = do
  x <- Safe.readMay str
  return $ Thunk x Vec
read _         = Nothing

num :: Num a => PartialCompiler a
num (Add xs)    = Just $ Thunk (+) xs
num (Sub xs)    = Just $ Thunk (flip (-)) xs
num (Mul xs)    = Just $ Thunk (*) xs
num (Negate xs) = Just $ Thunk negate xs
num _           = Nothing