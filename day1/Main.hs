module Main where

import Control.Monad (forM_, guard, when)
import qualified Data.IntSet as IS
import Data.List (lines)
import System.IO (hGetContents, stdin)

input :: IO IS.IntSet
input = IS.fromList . map read . lines <$> hGetContents stdin

solve :: IS.IntSet -> [IS.Key]
solve is = do
  a <- IS.toAscList is
  b <- IS.toAscList is
  guard $ a < b
  let c = 2020 - (a + b)
  guard $ b < c
  guard $ IS.member c is
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