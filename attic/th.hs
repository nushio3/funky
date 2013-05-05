{-# LANGUAGE TemplateHaskell #-}

import TypeLevel.Number.Nat

n1 :: $(natT 1)
n1 = undefined

main = print $ addN n1 n1
