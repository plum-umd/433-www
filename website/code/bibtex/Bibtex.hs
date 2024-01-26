{-# LANGUAGE InstanceSigs, RankNTypes, TemplateHaskell #-}
module Bibtex where

import Prelude hiding ((<>))
import Control.Applicative hiding (some, many)
import Control.Monad 

import Control.Lens hiding (noneOf)

import qualified Data.Char as Char

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error

import Text.PrettyPrint (Doc, (<+>), (<>), ($$))
import qualified Text.PrettyPrint as PP

import Data.Void (Void)

-- | Data Modelling

-- | A Single Bib Entry
data Bib = Bib
  { _name  :: String
  , _title :: String
  , _procs :: String
  , _short :: String
  , _authors :: String
  , _year :: String
  , _url  :: String
  , _keys :: [String]
  , _rest :: [(String, String)]
  } deriving (Eq, Show)

data TempBib = TempBib
  { _tempName :: String
  , _entries  :: [(String, String)]
  } deriving (Eq, Show)

makeLenses ''Bib
makeLenses ''TempBib

-- | Parsec <err> <input>
type Parser = Parsec Void String

{-
@inproceedings{DeeperShallowEmbeddings,
  author    = {Jacob Prinz and Alex Kavvos and Leonidas Lampropoulos},
  title     = {Deeper Shallow Embeddings},
  booktitle = {13th International Conference on Interactive Theorem Proving (ITP)},
  shortbooktitle = {ITP},
  year      = 2022,
  series    = {Lecture Notes in Computer Science},
}
-}

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2 <* space

bibOpen :: Parser String
bibOpen = (string "@inproceedings" <|> string "@INPROCEEDINGS")
  *> sb (char '{') (char ',') (many alphaNumChar)

entryValue :: Parser String
entryValue =
  sb (char '{') (char '}') (takeWhileP Nothing (/= '}'))
  <|> many alphaNumChar

entry :: Parser (String, String)
entry = (,) <$> (many alphaNumChar)
            <*> sb (char '=') (char ',') entryValue

bibEntries :: Parser [(String, String)]
bibEntries = many entry

tempBib :: Parser TempBib
tempBib = do
  n <- bibOpen
  es <- bibEntries
  char '}'
  space
  return $ TempBib n es

bib :: Parser Bib
bib = do
  TempBib n es <- tempBib
  let find key =
        case lookup key es of
          Just t  -> t
          Nothing -> error $ "No " ++ key ++ " found in bib: " ++ n 
  return $ Bib { _name  = n
               , _title = find "title"
               , _procs = find "booktitle"
               , _short = find "shortbooktitle"
               , _authors = find "author"
               , _year  = find "year"
               , _url = "https://lemonidas.github.io/pdf/" ++ n ++ ".pdf"
               , _keys = []
               , _rest = []
               }

class PP a where
  pp :: a -> Doc

instance PP String where
  pp s = PP.text s

instance PP Bib where
  pp b =
    (pp "@inproceedings{" <> pp (b ^. name) <> PP.char ',') $$
    PP.nest 2 (PP.vcat [ pp "title={" <> pp (b ^. title) <> pp "},"
                       , pp "booktitle={" <> pp (b ^. procs) <> pp "},"
                       , pp "shortbooktitle={" <> pp (b ^. short) <> pp "},"
                       , pp "author={" <> pp (b ^. authors) <> pp "},"
                       , pp "year={" <> pp (b ^. year) <> pp "},"
                       , pp "url={" <> pp (b ^. url) <> pp "},"
                       ]) $$
    PP.char '}'
    

--dummy :: Parser String
dummy = bibOpen *> bibEntries 

main :: IO ()
main = do
  inp <- getContents
  case runParser (many bib) "" inp of
    Right res -> forM_ res $ \bib -> putStrLn $ PP.render (pp bib)
    Left  err -> putStrLn $ show err

-- parse, print
forall bib, parse (print bib) == bib
