module Control.Funky.Instruction where

import           Control.Applicative ((<$>))
import           Data.Tensor.TypeLevel
import qualified Test.QuickCheck.Arbitrary as QC
import qualified Test.QuickCheck.Gen as QC

type Addr = Int

data Instruction a
  = Nop
  | Imm a -- values can most possibly be serialized, but functions cannot.
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

instance QC.Arbitrary a => QC.Arbitrary (Instruction a) where
  arbitrary = QC.oneof
    [ return Nop
    , Imm <$> QC.arbitrary ]