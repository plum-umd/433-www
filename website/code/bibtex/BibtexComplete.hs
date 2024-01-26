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

import Text.PrettyPrint (Doc, (<+>), (<>))
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

inProc :: Parser String
inProc = (string "inproceedings" <|> string "INPROCEEDINGS") <* char '{'

inPHD :: Parser String
inPHD = (string "phdthesis") <* char '{'

inBook :: Parser String
inBook = (string "book") <* char '{'

bibOpen :: Parser String
bibOpen = char '@' *> (inProc <|> inPHD <|> inBook)

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2 <* space

bibOpener :: Parser String
bibOpener = sb bibOpen (char ',') (some alphaNumChar)

entryContent :: Parser String
entryContent = sb (char '{') (char '}') (takeWhileP Nothing (/= '}'))
  <|> takeWhileP Nothing (/= ',')

bibEntry :: Parser (String, String)
bibEntry = (,) <$> many alphaNumChar <*> sb (char '=') (char ',') entryContent

bibEntries :: Parser [(String, String)]
bibEntries = many bibEntry
                                         
tempBibEntry :: Parser TempBib
tempBibEntry = TempBib <$> bibOpener <*> (bibEntries <* char '}' <* space)

--debuger :: Parser (String, String)
debuger = do
  open <- bibOpener
  ea <- many bibEntry
  return (open, ea)

toBib :: TempBib -> Bib
toBib t@(TempBib tn es) =
  let finder key =
        case lookup key es of
          Just s -> s
          Nothing -> error $ "No " ++ key ++ " in: " ++ show t 
      url  = maybe ("https://lemonidas.github.io/pdf/" ++ tn) id (lookup "url" es)
      keys = maybe [] return (lookup "keys" es)
  in Bib { _name  = tn
         , _title = finder "title"
         , _procs = finder "booktitle"
         , _short = finder "shortbooktitle"
         , _authors = finder "author"
         , _url  = url
         , _keys = keys
         , _rest = [] -- TODO: FIX
         }

pp :: Bib -> Doc
pp bib =
    (PP.text "@inproceedings{" <> PP.text (bib ^. name)  <> PP.char ',') PP.$$
    PP.nest 2 (PP.vcat [ PP.text "author = {" <> PP.text (bib ^. authors) <> PP.text "},"
                       , PP.text "title = {" <> PP.text (bib ^. title) <> PP.text "},"
                       , PP.text "booktitle = {" <> PP.text (bib ^. procs) <> PP.text "},"
                       , PP.text "shortbooktitle = {" <> PP.text (bib ^. short) <> PP.text "},"
                       , PP.text "url = {" <> PP.text (bib ^. url) <> PP.text "}," ]) PP.$$
    PP.char '}'

main :: IO ()
main = do
  inp <- getContents
  case runParser (many tempBibEntry) "" inp of
    Right temps -> let bibs = map toBib temps in
                   forM_ bibs $ putStrLn . PP.render . pp
    Left err -> putStrLn $ show err

