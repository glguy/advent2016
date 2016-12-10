module Main where

import           Common
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main =
  do txt <- readInputFile 10

     let solution = followInstructions
                  $ map parseInstr
                  $ lines txt

     print [ who | (who,what) <- Map.toList solution
                 , sort what  == [ 17, 61 ]
                 ]

     print $ product $ do i <- [0..2]
                          solution Map.! Output i

data Instr = Value Int Target | Gives Int Target Target
  deriving Show

data Target = Bot Int | Output Int
  deriving (Eq, Ord, Show)

parseInstr :: String -> Instr
parseInstr str =
  case words str of

    ["value",n,"goes","to",tgt,m] ->
      Value (read n) (parseTarget tgt m)

    ["bot",a,"gives","low","to",tgt1,n,"and","high","to",tgt2,m] ->
      Gives (read a) (parseTarget tgt1 n) (parseTarget tgt2 m)

    _ -> error ("Bad instruction: " ++ str)
  where
    parseTarget "bot"    n = Bot    (read n)
    parseTarget "output" n = Output (read n)
    parseTarget other    _ = error ("Bad target: " ++ other)


followInstructions :: [Instr] -> Map Target [Int]
followInstructions xs = result
  where
    result = Map.fromListWith (++) (concatMap aux xs)

    aux (Value val tgt)   = [ (tgt,[val]) ]
    aux (Gives src lo hi) = [ (lo ,[l]), (hi,[h]) ]
      where [l,h] = sort (result Map.! Bot src)
