{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Tensor.TypeLevel

class Tap t where
  type TFun t a :: *        
  tap :: TFun t a -> t a -> a

instance Tap Vec where
  type TFun Vec a = a
  tap f _ = f

instance Tap v => Tap ((:~) v) where
  type TFun ((:~) v) a = a -> TFun v a
  tap f (vx :~ x) = f x `tap` vx

tmul :: Num a => Vec2 a -> a
tmul = tap (*)

main :: IO ()
main = do
  print $ 4 `tap` Vec 
  print $ negate `tap` vec1 4  
  print $ (*) `tap` vec2 6 7  
  print $ tmul $ vec2 6 7 
