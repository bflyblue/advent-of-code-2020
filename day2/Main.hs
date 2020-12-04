{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (empty)
import qualified Data.List as List
import Control.Monad (void, forM_, guard, when)
import Data.List (lines)
import Data.Void (Void)
import System.IO (hGetContents, stdin)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-
    Each line of the input has a password policy, followed by a password:
    1-3 a: abcde
    Policy: look at the letter in position 1 and 3 for the letter 'a'
    Password: abcde
-}

-- Line in input
data Line = Line {lPolicy :: Policy, lPassword :: String}
  deriving (Show)

-- Policy
data Policy = Policy {polFst :: Int, polSnd :: Int, polLetter :: Char}
  deriving (Show)

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

-- match a symbol followed by whitespace
symbol :: M.Tokens String -> Parser (M.Tokens String)
symbol = L.symbol sc

-- parse positive integers
int :: Parser Int
int = lexeme L.decimal

-- parse a policy
policy :: Parser Policy
policy = do
  polFst <- int <?> "policy first position"
  _ <- symbol "-"
  polSnd <- int <?> "policy second position"
  polLetter <- lexeme C.letterChar <?> "policy letter"
  return Policy {..}

-- parse an input line (terminated by eol or eof)
line :: Parser Line
line = do
  lPolicy <- policy <?> "policy"
  _ <- symbol ":"
  lPassword <- M.some C.letterChar <?> "password"
  _ <- void C.eol <|> M.eof
  return Line {..}

-- parse entire input from stdin, raising exception on failure
input :: IO [Line]
input = do
  i <- hGetContents stdin
  case M.parse lines "stdin" i of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right ls -> return ls
  where
    lines = M.many line <* M.eof

-- check if a line's password is valid according to it's policy
valid :: Line -> Bool
valid (Line lPolicy lPassword) =
  let l1 = (lPassword !? polFst lPolicy) == Just (polLetter lPolicy)
      l2 = (lPassword !? polSnd lPolicy) == Just (polLetter lPolicy)
  in l1 `xor` l2
  where xor a b = (a || b) && not (a && b)

-- safe index
(!?) :: [a] -> Int -> Maybe a
lst !? idx = if List.length lst < idx then Nothing else Just $ lst !! (idx - 1)

solve :: [Line] -> Int
solve = List.length . filter valid

main :: IO ()
main = input >>= return . solve >>= print