{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-
    Input is
-}

data Passport = Passport
  { byr :: Maybe String,
    iyr :: Maybe String,
    eyr :: Maybe String,
    hgt :: Maybe String,
    hcl :: Maybe String,
    ecl :: Maybe String,
    pid :: Maybe String,
    cid :: Maybe String
  }
  deriving (Show, Eq)

instance Semigroup Passport where
  a <> b =
    Passport
      { byr = byr a <> byr b,
        iyr = iyr a <> iyr b,
        eyr = eyr a <> eyr b,
        hgt = hgt a <> hgt b,
        hcl = hcl a <> hcl b,
        ecl = ecl a <> ecl b,
        pid = pid a <> pid b,
        cid = cid a <> cid b
      }

instance Monoid Passport where
  mempty =
    Passport
      { byr = Nothing,
        iyr = Nothing,
        eyr = Nothing,
        hgt = Nothing,
        hcl = Nothing,
        ecl = Nothing,
        pid = Nothing,
        cid = Nothing
      }

type Parser = M.Parsec Void String

-- space consumer
sc :: Parser ()
sc = do
  void $ M.takeWhileP (Just "space") (== ' ')
  void $
    M.optional $ do
      void C.eol
      void $ M.takeWhileP (Just "space") (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

val :: Parser String
val = lexeme (M.takeWhile1P (Just "field value") (not . isSpace))

-- parse an single field (represented as a passport with a single field present)
field :: Parser Passport
field =
  M.choice
    [ f "byr" (\v -> mempty {byr = Just v}),
      f "iyr" (\v -> mempty {iyr = Just v}),
      f "eyr" (\v -> mempty {eyr = Just v}),
      f "hgt" (\v -> mempty {hgt = Just v}),
      f "hcl" (\v -> mempty {hcl = Just v}),
      f "ecl" (\v -> mempty {ecl = Just v}),
      f "pid" (\v -> mempty {pid = Just v}),
      f "cid" (\v -> mempty {cid = Just v})
    ]
  where
    f name setter = do
      void $ C.string name
      void $ C.char ':'
      setter <$> val

-- parse an single passport
passport :: Parser Passport
passport = mconcat <$> M.some field

-- parse all passports
passports :: Parser [Passport]
passports = passport `M.sepBy` C.eol <* M.eof

-- parse entire input from stdin, raising exception on failure
input :: IO [Passport]
input = do
  i <- getContents
  case M.parse passports "stdin" i of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right m -> return m

valid :: Passport -> Bool
valid p =
  and
    [ isJust (byr p),
      isJust (iyr p),
      isJust (eyr p),
      isJust (hgt p),
      isJust (hcl p),
      isJust (ecl p),
      isJust (pid p)
      -- isJust (cid p)
    ]

part1 :: [Passport] -> Int
part1 = length . filter valid

part2 :: a
part2 = undefined

main :: IO ()
main = do
  ps <- input
  print $ part1 ps
-- print $ part2 _