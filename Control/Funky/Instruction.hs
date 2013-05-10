module Control.Funky.Instruction where

import           Data.Tensor.TypeLevel

type Addr = Int

data Instruction 
  = Nop
  | Imm String
  | Select (Vec3 Addr)
  | Theta (Vec1 Addr)
  | Add (Vec2 Addr)
  | Sub (Vec2 Addr)
  | Mul (Vec2 Addr)
  | Negate (Vec1 Addr)
  | Abs (Vec1 Addr)
  | Sum [Addr]
  | Prod [Addr]
  deriving (Eq, Show)