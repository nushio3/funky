{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
type Apply a m = m a
type Binary f a = f a (f a a)

type family Listo a
type instance Listo Double = [Double]

xs :: Apply Int []
xs = [6,7]

mul :: Binary (->) Int
mul = (*)

mulu :: Binary (,) Int
mulu = (6,(7,42))



main = do
  print $ mul 6 7
