{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Massiv.Array (Array, S, Ix2(..), Sz(..), Sz2(..))
import qualified Data.Massiv.Array as A
import Data.Void (Void)
import Foreign.Ptr
import Foreign.Storable
import System.IO (hGetContents, stdin)
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
  { mapArray :: Array S Ix2 Square
  , mapSize :: Sz2
  }
  deriving (Show)

-- Our squares are either open or contain a tree
data Square = Open | Tree
  deriving (Show, Eq, Ord, Enum)

-- Storable instance using the enum Int value
instance Storable Square where
  sizeOf _ = sizeOf @Int 0
  alignment _ = alignment @Int 0
  peek ptr = toEnum <$> peek (castPtr ptr)
  poke ptr a = poke (castPtr ptr) (fromEnum a)

-- read the square at the given coordinate, taking right wrapping into account
index :: Map -> (Int, Int) -> Square
index m (x, y) =
  let (Sz2 _ width) = mapSize m
  in A.index' (mapArray m) (y :. x `mod` width)

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
square = M.choice
  [ Open <$ C.char '.'
  , Tree <$ C.char '#'
  ]

map' :: Parser Map
map' = do
    ls <- M.many line <* M.eof
    let a = A.fromLists' A.Seq ls
    return Map { mapArray = a , mapSize = A.size a }

-- parse entire input from stdin, raising exception on failure
input :: IO Map
input = do
  i <- hGetContents stdin
  case M.parse map' "stdin" i of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right m -> return m

solve :: [[Square]] -> Int
solve = undefined

main :: IO ()
main = do
  m <- input
  print $ index m (35, 0)
