module Main where

import Control.Monad (forM_, guard, when)
import qualified Data.IntSet as IS
import Data.List (lines)
import System.IO (hGetContents, stdin)

input :: IO IS.IntSet
input = IS.fromList . map read . lines <$> hGetContents stdin

main :: IO ()
main = do
  is <- input
  let r = do
        a <- IS.toAscList is
        b <- IS.toAscList is
        guard $ a < b
        let c = 2020 - (a + b)
        guard $ b < c
        guard $ IS.member c is
        return $ a * b * c
  print r
