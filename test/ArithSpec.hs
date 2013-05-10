module ArithSpec (spec) where


import qualified Data.Vector as V
import           Data.Vector ((!))
import           Test.Hspec
import           Funky.Machine

import           Data.Tensor.TypeLevel hiding ((!))

imm :: a -> Step a
imm x = Step x Vec

una :: (a -> a) -> Int -> Step a
una f x = Step f $ vec1 x 

bin :: (a -> a -> a) -> Int -> Int -> Step a
bin f x y = Step f $ vec2 y x


machine1 :: Executable Double
machine1 = fromList 
  [ imm 6
  , imm 7 
  , bin (*) 0 1]

machine2 :: Executable Int
machine2 = fromList 
  [ imm 6
  , imm 7 
  , bin (-) (-1) 1
  , bin (+) 0 500
  , bin (*) 2 3
  , una negate 4]


spec :: Spec
spec = do
  describe "Funky Machine" $ do
    it "gives answer to everything" $ do
      let ret = eval machine1 
      (ret ! 2) `shouldBe` 42
    it "gives the default where out of index" $ do
      eval machine2 `shouldBe` V.fromList [6,7,-7,6,-42,42]