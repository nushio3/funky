module Funky.Evaluation where

import           Data.Default

import           Funky.Machine

{-|

evaluate the executable machine into vector of results.

-}

eval :: Default a => Machine (Thunk a) -> Machine a
eval (Machine insts) = Machine ret
  where
    ret :: V.Vector a
    ret = V.imap compute insts

    compute :: Int -> Thunk a -> a
    compute idx inst = case inst of
      Thunk f idxs -> f `tap` fmap get idxs

    get :: Int -> a
    get addr = maybe def id (ret V.!? addr)
