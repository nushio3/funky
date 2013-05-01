module Funky.Machine where

import Data.Vector

data Machine a = Machine
  { instructions :: Vector a }
