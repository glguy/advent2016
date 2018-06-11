module Main where

import           Common
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char

main :: IO ()
main =
  do solution <- followInstructions . parseLines parseInstr <$> readInputFile 10

     print [ who | (who,Two 17 61) <- Map.toList solution]

     print $ product [ v | i <- [0..2]
                         , let One v = solution Map.! Output i ]

-- Types ---------------------------------------------------------------

data Instr = Value !Int !Target | Gives !Target !Target !Target
  deriving Show

data Target = Bot !Int | Output !Int
  deriving (Eq, Ord, Show)

data Holding = One !Int | Two !Int !Int
  deriving Show

-- Parsing -------------------------------------------------------------

parseInstr :: Parser Instr
parseInstr =
         Value <$  string "value " <*> number <* string " goes to " <*> target
     <|> Gives <$> target <* string " gives low to "
               <*> target <* string " and high to "
               <*> target

target :: Parser Target
target = Bot    <$ string "bot "    <*> number <|>
         Output <$ string "output " <*> number

-- Solving -------------------------------------------------------------

followInstructions :: [Instr] -> Map Target Holding
followInstructions xs = result
  where
    result = Map.fromListWithKey combine (concatMap aux xs)

    aux (Value val tgt)   = [ (tgt, One val) ]
    aux (Gives src lo hi) = [ (lo , One l), (hi, One h) ]
      where Two l h = result Map.! src


combine :: Target -> Holding -> Holding -> Holding
combine _ (One x) (One y) = Two (min x y) (max x y)
combine tgt _ _ = error ("Bad combination for " ++ show tgt)
