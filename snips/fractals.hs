-- Snipped from:
--
-- https://janmasrovira.gitlab.io/ascetic-slug/post/ascii-fractals/

type Pattern
  = [[Char]]

main :: IO ()
main =
  putStrLn (unlines (fractalize p 3))
 where
  p =
    [ " # "
    , "###"
    , " # "
    ]

fractalize :: Pattern -> Int -> Pattern
fractalize [] _ = []
fractalize pat k = [ [ charAt k i j | j <- [0..m^k - 1] ] | i <- [0..n^k - 1] ]
  where
    n = length pat
    m = length (head pat)
    charAt k i j
      | k <= 1 = pat!!i!!j
      | ' ' == charAt (k - 1) (i`div`n) (j`div`m) = ' '
      | otherwise = pat!!(i`mod`n)!!(j`mod`m)

render :: Pattern -> String
render = unlines
