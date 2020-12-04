module Main where

import Control.Monad (forM_, guard, when)
import qualified Data.IntSet as IS
import Data.List (lines)
import System.IO (hGetContents, stdin)

-- Read input on stdin as a set of integers
input :: IO IS.IntSet
input = IS.fromList . map read . lines <$> hGetContents stdin

solve :: IS.IntSet -> [IS.Key]
solve is = do             -- list monad
  a <- IS.toAscList is    -- take a from the sorted set of inputs
  b <- IS.toAscList is    -- take b from the sorted set of inputs
  let c = 2020 - (a + b)  -- Instead of picking c and testing
                          -- a + b + c == 2020, we calculate what
                          -- c is needed to make the test work.
  guard $ a < b && b < c  -- To prevent results which are simply
                          -- permutations of a, b, c we constrain
                          -- the ordering so only one is found.
  guard $ IS.member c is  -- Test if c is present in our input.
                          -- This is faster than linearly stepping
                          -- through the input set and testing
                          -- the summed values are 2020.
  return $ a * b * c

main :: IO ()
main =
  input >>= return . solve >>= showSolution
  where
    showSolution []  = putStrLn "No solution found."
    showSolution [x] = print x
    showSolution xs  = do
      putStrLn "Multiple solutions found:"
      mapM_ print xs