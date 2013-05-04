module ArithSpec (spec) where


import qualified Data.Vector as V
import           Data.Vector ((!))
import           Test.Hspec
import           Funky.Machine

machine1 :: Machine Double
machine1 = fromList 
  [ Imm 6
  , Imm 7 
  , Binary (*) 1 2]

machine2 :: Machine Int
machine2 = fromList 
  [ Imm 6
  , Imm 7 
  , Binary (-) 3 1
  , Binary (+) 3 (-5)
  , Binary (*) 2 1
  , Unary negate 1]


spec :: Spec
spec = do
  describe "Funky Machine" $ do
    it "gives answer to everything" $ do
      let ret = eval machine1 
      (ret ! 2) `shouldBe` 42
    it "gives the default where out of index" $ do
      eval machine2 `shouldBe` V.fromList [6,7,-7,6,-42,42]