module Main where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main =
  do txt <- readFile "inputs/input10.txt"

     let solution = toSolution
                  $ toRoutes
                  $ map parseInstr
                  $ lines txt

     print [ who | (who,what) <- Map.toList solution
                 , sort what  == [ 17, 61 ]
                 ]

     print $ do [a] <- Map.lookup (ToOutput 0) solution
                [b] <- Map.lookup (ToOutput 1) solution
                [c] <- Map.lookup (ToOutput 2) solution
                return (a * b * c)

data Instr
  = Value Int Target
  | Gives Int Target Target
  deriving Show

data Target = ToBot Int | ToOutput Int
  deriving (Eq, Ord, Show)

data Source = FromBotHi Int | FromBotLo Int | Constant Int


parseInstr :: String -> Instr
parseInstr str =
  case words str of

    ["value",n,"goes","to",tgt,m] ->
      Value (read n) (parseTarget tgt m)

    ["bot",a,"gives","low","to",tgt1,n,"and","high","to",tgt2,m] ->
      Gives (read a) (parseTarget tgt1 n) (parseTarget tgt2 m)

    _ -> error ("Bad instruction: " ++ str)
  where
    parseTarget "bot"    n = ToBot    (read n)
    parseTarget "output" n = ToOutput (read n)
    parseTarget other    _ = error ("Bad target: " ++ other)


toRoutes :: [Instr] -> Map Target [Source]
toRoutes xs = Map.fromListWith (++) (concatMap aux xs)
  where
    aux (Value val tgt)   = [ (tgt, [Constant  val]) ]
    aux (Gives src lo hi) = [ (lo , [FromBotLo src])
                            , (hi , [FromBotHi src]) ]

toSolution :: Map Target [Source] -> Map Target [Int]
toSolution routes = result
  where
    result = fmap (map fromSource) routes
    fromSource (FromBotLo n) = minimum (result Map.! ToBot n)
    fromSource (FromBotHi n) = maximum (result Map.! ToBot n)
    fromSource (Constant  n) = n
