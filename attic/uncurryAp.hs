{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Tensor.TypeLevel

class Tap t where
  type TFun t a :: *        
  tap :: t a -> TFun t a -> a

instance Tap Vec where
  type TFun Vec a = a
  tap _ f = f

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap (vx :~ x) f = vx `tap` f x



main :: IO ()
main = do
  print $ Vec `tap` 4
  print $ vec1 4 `tap` negate
  print $ vec2 6 7 `tap` (*)
