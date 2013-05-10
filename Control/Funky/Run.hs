{-# LANGUAGE ScopedTypeVariables #-}

module Control.Funky.Run where

import           Data.Default (Default, def)
import qualified Data.Vector as V

import           Control.Funky.Type


{-|

evaluate the executable machine into vector of results.

-}

run :: forall a. Default a => Machine (Thunk a) -> Machine a
run (Machine insts) = Machine ret
  where
    ret :: V.Vector a
    ret = V.imap compute insts

    compute :: Int -> Thunk a -> a
    compute idx inst = case inst of
      Thunk f idxs -> f `tap` fmap get idxs

    get :: Int -> a
    get addr = maybe def id (ret V.!? addr)
