module ArithSpec (spec) where


import qualified Data.Vector as V
import           Data.Vector ((!))
import           Test.Hspec
import           Funky.Machine

machine1 :: Executable Double
machine1 = fromList 
  [ imm 6
  , imm 7 
  , bin (*) 1 2]

machine2 :: Executable Int
machine2 = fromList 
  [ imm 6
  , imm 7 
  , bin (-) 3 1
  , bin (+) 3 (-5)
  , bin (*) 2 1
  , una negate 1]


spec :: Spec
spec = do
  describe "Funky Machine" $ do
    it "gives answer to everything" $ do
      let ret = eval machine1 
      (ret ! 2) `shouldBe` 42
    it "gives the default where out of index" $ do
      eval machine2 `shouldBe` V.fromList [6,7,-7,6,-42,42]