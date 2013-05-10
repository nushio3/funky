module Funky.Instruction where

import           Data.Tensor.TypeLevel

type Addr = Int

Instruction 
  = Nop
  | Theta (Vec3 Addr)
  | Add (Vec2 Addr)
  | Sub (Vec2 Addr)
  | Mul (Vec2 Addr)
  | Negate (Vec1 Addr)
  | Abs (Vec1 Addr)
  | Sum [Addr]
  | Prod [Addr]