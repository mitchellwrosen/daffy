-- Snipped from:
--
-- https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/docs/Options-Applicative-Help-Levenshtein.html

import System.Environment
import System.Random

main :: IO ()
main = do
  [n] <- getArgs
  let (xs, ys) = splitAt n (take (2*n) (randoms (mkStdGen 1)))
  print (editDistance (xs :: [Char]) ys)

editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b = last $
  case () of
    _ | lab == 0
     -> mainDiag
      | lab > 0
     -> lowers !! (lab - 1)
      | otherwise
     -> uppers !! (-1 - lab)
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals

    eachDiag _ [] _ = []
    eachDiag _ _ [] = []
    eachDiag a' (_:bs) (lastDiag:diags) =
      oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
      where
        nextDiag = head (tail diags)

    oneDiag a' b' diagAbove diagBelow = thisdiag
      where
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
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a' b' firstelt diagAbove (tail diagBelow)

    lab = length a - length b

    min3 x y z =
      if x < y
        then x
        else min y z
