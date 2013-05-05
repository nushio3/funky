{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

class RealFunc real f where
  type UncurriedInput real f :: *
  uncurry' :: f -> (UncurriedInput real f -> real)

instance RealFunc a a where
  type UncurriedInput a a = ()
  uncurry' x = const x

instance (RealFunc a f) => RealFunc a (a->f) where
  type UncurriedInput a (a->f) = (a,UncurriedInput a f)
  uncurry' fun (x,xs) = uncurry' (fun x) xs

--main = do
--  print $ uncurry ((*) :: Double -> Double -> Double) (6,(7,()))


