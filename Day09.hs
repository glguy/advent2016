module Main where

main =
  do xs <- lines <$> readFile "inputs/input9.txt"
     print (sum (map decode1 xs))
     print (sum (map decode2 xs))


reads1 :: Read a => String -> (a,String)
reads1 str =
  case reads str of
    [x] -> x
    _   -> error ("reads1: " ++ str)


decode1 :: String -> Int
decode1 ('(':xs) = m*n + decode1 (drop n xs2)
  where
    (n,'x':xs1) = reads1 xs
    (m,')':xs2) = reads1 xs1
decode1 (_:xs) = 1 + decode1 xs
decode1 []     = 0


decode2 :: String -> Int
decode2 ('(':xs) = m * decode2 sub + decode2 xs3
  where
    (n  ,'x':xs1) = reads1 xs
    (m  ,')':xs2) = reads1 xs1
    (sub,    xs3) = splitAt n xs2
decode2 (_:xs) = 1 + decode2 xs
decode2 [] = 0
