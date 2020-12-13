{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Massiv.Array (Array, B, Ix2 (..), Sz (..), Sz2 (..))
import qualified Data.Massiv.Array as A
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-
    Input is rows of '.' (empty) or '#' (tree).
    Not specified in puzzle but we will assume each row is the same width.
-}

-- Using the `massiv` package for the first time to see how it works, instead of
-- using [[Square]]. Using the Storable rather than Boxed type.
data Map = Map
  { mapArray :: Array B Ix2 Square,
    mapSize :: Sz2
  }
  deriving (Show)

-- Our squares are either open or contain a tree
data Square = Open | Tree
  deriving (Show, Eq, Ord, Enum)

-- read the square at the given coordinate, taking right wrapping into account
index :: Map -> (Int, Int) -> Maybe Square
index m (x, y) =
  let (Sz2 _ width) = mapSize m
   in A.index (mapArray m) (y :. x `mod` width)

{-
    We use a small parser to read each line of input
-}

type Parser = M.Parsec Void String

-- space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

-- parse something followed by whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse an input line (terminated by eol or eof)
line :: Parser [Square]
line = M.some square <* (void C.eol <|> M.eof)

-- parse an single square
square :: Parser Square
square =
  M.choice
    [ Open <$ C.char '.',
      Tree <$ C.char '#'
    ]

-- parse map
map' :: Parser Map
map' = do
  ls <- M.many line <* M.eof
  let a = A.fromLists' A.Seq ls
  return Map {mapArray = a, mapSize = A.size a}

-- parse entire input from stdin, raising exception on failure
input :: IO Map
input = do
  i <- getContents
  case M.parse map' "stdin" i of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right m -> return m

slopeEncounters :: (Int, Int) -> Map -> Int
slopeEncounters (sx, sy) map = go (0, 0)
  where
    go pos@(x, y) =
      let next = go (x + sx, y + sy)
       in case index map pos of
            Just Tree -> 1 + next
            Just Open -> 0 + next
            Nothing -> 0

part1 :: Map -> Int
part1 = slopeEncounters (3, 1)

part2 :: Map -> Int
part2 map =
  slopeEncounters (1, 1) map
    * slopeEncounters (3, 1) map
    * slopeEncounters (5, 1) map
    * slopeEncounters (7, 1) map
    * slopeEncounters (1, 2) map

main :: IO ()
main = do
  map <- input
  print $ part1 map
  print $ part2 map