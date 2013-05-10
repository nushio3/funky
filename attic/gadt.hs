{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Default
import Data.Tensor.TypeLevel

{-| Tuple apply.
    'Tap' is a data constructor which is similar to tuple,
    that supports uncurried function application.
 -}

class Tap2 t where
  -- | The type of the curried function
  --   that can be applied to @t a@.
  type TFun2 (t a->b) :: *
  -- | Apply 'TFun t a' to 't a' .
  tap :: TFun t a -> t a -> a




class Tap t where
  -- | The type of the curried function
  --   that can be applied to @t a@.
  type TFun t a :: *
  -- | Apply 'TFun t a' to 't a' .
  tap :: TFun t a -> t a -> a

instance Tap Vec where
  type TFun Vec a = a
  tap f _ = f

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap f (vx :~ x) = f x `tap` vx

-- google homogeneous tuple in haskell.

data Inst a where
  Inst :: (Tap t, Functor t) => TFun t a -> t Integer -> Inst a


data Op
  = Nop
  | Negate (Vec1 Integer)
  | Add (Vec2 Integer)

translate :: (Default a, Num a) => Op -> Inst a
translate Nop = Inst def Vec
translate (Negate x) = Inst negate x
translate (Add x) = Inst (+) x

tmul :: Num a => Vec2 a -> a
tmul = tap (*)

eval :: (Default a, Num a) => Inst a -> a
eval (Inst f v) = tap f (fmap fromInteger v)



main :: IO ()
main = do
  print $ eval $ (translate Nop :: Inst Double)
  print $ eval $ (translate $ Add (vec2 6 7) :: Inst Double)
