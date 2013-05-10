module Control.Funky.Compiler.Instances where

import qualified Data.Default as Def

import           Control.Funky.Compiler
import           Control.Funky.Instruction
import           Control.Funky.Type
import           Data.Tensor.TypeLevel
import           Prelude hiding (read)
import qualified Prelude


def :: Def.Default a => PartialCompiler a
def _ = Nothing

read :: Read a => PartialCompiler a
read (Imm str) = Just $ Thunk (Prelude.read str) Vec
read _         = Nothing

num :: Num a => PartialCompiler a
num (Add xs)    = Just $ Thunk (+) xs
num (Sub xs)    = Just $ Thunk (flip (-)) xs
num (Mul xs)    = Just $ Thunk (*) xs
num (Negate xs) = Just $ Thunk negate xs
num _           = Nothing