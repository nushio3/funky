{-# LANGUAGE ScopedTypeVariables #-}

module Control.Funky.Compiler where

import           Text.Printf(printf)

import           Control.Funky.Instruction
import           Control.Funky.Type

-- | The name and the function of the compiler
type PartialCompiler a = (String, Instruction -> Maybe (Thunk a))

runCompilers :: forall a.
             [PartialCompiler a] -> Program -> Either String (Executable a)

runCompilers pcs prog =
  case retE of
    Left inst ->
      Left $ printf "Instruction %s accepted by none of compiler %s"
                    (show inst)
                    (show $ map fst pcs)
    Right ts  -> Right $ fromList ts

  where
    retE :: Either
      Instruction {- this instruction remained unprocessed -}
      [Thunk a]   {- all the instructions successfully processed -}
    retE = sequence $ map (revolute . runPCs) $ toList prog

    revolute :: Either x y -> Either y x
    revolute (Left x) = Right x
    revolute (Right y) = Left y

    runPCs :: Instruction -> Either (Thunk a) Instruction
    runPCs inst = foldl (>>=) (return inst) (map runPC pcs)

    runPC :: PartialCompiler a
          -> Instruction -> Either (Thunk a) Instruction
    runPC (_,pcBody) inst = case pcBody inst of
       -- here we use 'Left' to denote the state that requires no
       -- further processing.
       Just th -> Left th
       Nothing -> Right inst