module Control.Funky.Instruction where

import           Control.Applicative ((<$>))
import           Data.Tensor.TypeLevel
import qualified Test.QuickCheck.Arbitrary as QC
import qualified Test.QuickCheck.Gen as QC

type Addr = Int

-- | values can most possibly be serialized, but functions
-- cannot. Therefore, we represent the functions by corresponding
-- symbols rather than function itself.

data Instruction a
  = Nop
  | Imm a

  -- functions for ord
  | Select (Vec3 Addr)
  | Theta (Vec1 Addr)

  -- functions for Num
  | Add (Vec2 Addr)
  | Sub (Vec2 Addr)
  | Mul (Vec2 Addr)
  | Negate (Vec1 Addr)
  | Abs (Vec1 Addr)
  | Sum [Addr]
  | Prod [Addr]

  -- functions for Fractional
  | Divide (Vec2 Addr)
  | Recip  (Vec1 Addr)

  -- functions for Floating
  | Pi (Vec Addr)
  | Exp (Vec1 Addr)
  | Sqrt (Vec1 Addr)
  | Log (Vec1 Addr)
  | Sin (Vec1 Addr)
  | Cos (Vec1 Addr)
  | Tan (Vec1 Addr)
  | Asin (Vec1 Addr)
  | Acos (Vec1 Addr)
  | Atan (Vec1 Addr)
  | Sinh (Vec1 Addr)
  | Cosh (Vec1 Addr)
  | Tanh (Vec1 Addr)
  | Asinh (Vec1 Addr)
  | Acosh (Vec1 Addr)
  | Atanh (Vec1 Addr)

  
  deriving (Eq, Show)

instance QC.Arbitrary a => QC.Arbitrary (Instruction a) where
  arbitrary = QC.oneof
    [ return Nop
    , Imm <$> QC.arbitrary ]