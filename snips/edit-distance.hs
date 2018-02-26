-- Snipped from:
--
-- https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/docs/Options-Applicative-Help-Levenshtein.html

import System.Environment
import System.Random

main :: IO ()
main = do
  [n] <- map read <$> getArgs
  let (xs, ys) = splitAt n (take (2*n) (randoms (mkStdGen 1)))
  print (editDistance (xs :: [Char]) ys)

editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b = last $
  case () of
    _ | lab a b == 0
     -> mainDiag a b
      | lab a b > 0
     -> lowers a b !! (lab a b - 1)
      | otherwise
     -> uppers a b !! (-1 - lab a b)

mainDiag a b = oneDiag a b (head (uppers a b)) (-1 : head (lowers a b))

uppers a b = eachDiag a b (mainDiag a b : uppers a b) -- upper diagonals

lowers a b = eachDiag b a (mainDiag a b : lowers a b) -- lower diagonals

eachDiag _ [] _ = []
eachDiag _ _ [] = []
eachDiag a' (_:bs) (lastDiag:diags) =
  oneDiag a' bs (nextDiag diags) lastDiag : eachDiag a' bs diags

nextDiag diags = head (tail diags)

oneDiag a' b' diagAbove diagBelow = thisdiag a' b' diagAbove diagBelow

doDiag [] _ _ _ _ = []
doDiag _ [] _ _ _ = []
-- Check for a transposition
-- We don't add anything to nw here, the next character
-- will be different however and the transposition
-- will have an edit distance of 1.
doDiag (ach:ach':as) (bch:bch':bs) nw n w
  | ach' == bch && ach == bch'
  = nw : (doDiag (ach' : as) (bch' : bs) nw (tail n) (tail w))
-- Standard case
doDiag (ach:as) (bch:bs) nw n w =
  me : (doDiag as bs me (tail n) (tail w))
  where
    me =
      if ach == bch
        then nw
        else 1 + min3 (head w) nw (head n)

firstelt diagBelow = 1 + head diagBelow

thisdiag a' b' diagAbove diagBelow =
  firstelt diagBelow
    : doDiag a' b' (firstelt diagBelow) diagAbove (tail diagBelow)

lab a b = length a - length b

min3 x y z =
  if x < y
    then x
    else min y z
