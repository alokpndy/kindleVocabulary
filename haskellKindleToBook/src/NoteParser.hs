-- # extesnions
{-# LANGUAGE OverloadedStrings #-}

-- #  module
module NoteParser where

-- # imports
import Control.Applicative
import Data.Attoparsec.Text      
import Data.Text (Text)
import qualified Data.Text as TX
import Types
import Data.Char
import Data.Text.IO as T

import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Text       as A



stringChoices :: [Text] -> Parser Text
stringChoices = AC.choice . map A.asciiCI


-- | Book Parser
parseBook :: Parser Text
parseBook = do
  A.takeWhile1 (\c -> c /= '(') 
  
-- | Author Parser
parseAuthor :: Parser Text
parseAuthor = do
  A.skipWhile (\c -> c /= '(')
  char '('
  value <- A.takeTill ( == ')')
  return value 

-- parseOnly
manyAuthors :: Parser [Text]
manyAuthors = do
    (many $  parseAuthor <* A.char ')')

-- | Format Parser
formatParser :: Parser ClipFormat
formatParser  =   note <|> highlight <|> bookmark 
      where note =  stringChoices ["Note "] *> pure Note
            highlight = stringChoices ["Highlight "] *> pure Highlight
            bookmark = stringChoices ["Bookmark "] *> pure Bookmark

parseAllClippings :: Text -> Either String [Clipping]
parseAllClippings xs  = do
   clips  <- A.parseOnly ( many1 parseAClipping) xs 
   return clips

 
parseAClipping :: Parser Clipping
parseAClipping = do
  book <- parseBook
  author <- A.takeTill  A.isEndOfLine
  comment
  ctype <-  stringChoices [ "- Your "] *>  formatParser
  tag <-  A.skipWhile (/= '|')  *> dateParser
  body <-   comment   *> sectionName
  return $  Clipping (last (nameExist author)) book  ctype tag  body 


-- fromJust can be used here
nameExist xs = case ((A.parseOnly (many parseAuthor)) xs) of
  Right x ->  x
  Left y -> ["(Failed Author)"]


comment :: Parser ()
comment = do
        skipWhile (/= '\n')
        char '\n'
        return ()

sectionName :: Parser Text
sectionName = do
        comment
        name <- takeTill (== '=')
        "=========="
        return $ name 





-- Added on Tuesday, May 19, 2015 3:43:03 PM
-- 02  05    19   2015  03    43    03    02
-- day month day  year  hour  min   sec   noon


-- | Format Parser
dayParser :: Parser WeekDay
dayParser  =   mon <|> tue <|> wed <|> thu <|> fri <|> sat <|> sun  
      where mon = stringChoices ["Monday"] *> pure "01"
            tue = stringChoices ["Tuesday"] *> pure "02"
            wed = stringChoices ["Wednesday"] *> pure "03"
            thu = stringChoices ["Thursday"] *> pure "04"
            fri = stringChoices ["Friday"] *> pure "05"
            sat = stringChoices ["Saturday"] *> pure "06"
            sun = stringChoices ["Sunday"] *> pure "07"

monthParser :: Parser Month
monthParser  = jan <|> feb <|> mar <|> apr <|> may <|> june <|> july <|> aug <|> sep <|> oct <|> nov <|> dec   
      where jan = stringChoices ["January"] *> pure "01"
            feb = stringChoices ["February"] *> pure "02"
            mar = stringChoices ["March"] *> pure "03"
            apr = stringChoices ["April"] *> pure "04"
            may = stringChoices ["May"] *> pure "05"
            june = stringChoices ["June"] *> pure "06"
            july = stringChoices ["July"] *> pure "07"
            aug = stringChoices ["August"] *> pure "08"
            sep = stringChoices ["September"] *> pure "09"
            oct = stringChoices ["October"] *> pure "10"
            nov = stringChoices ["November"] *> pure "11"
            dec = stringChoices ["December"] *> pure "12"

dayTimeParser = do
  am <|> pm
  where
    am = stringChoices ["AM"] *> pure "01"
    pm = stringChoices ["PM"] *> pure "02"

dateParser = do
  stringChoices ["| Added on "]
  ww <- dayParser
  A.char ',' <* A.skipSpace
  mm <- monthParser <* A.skipSpace
  dd <- A.takeWhile1 isDigit <* A.char ',' 
  yyyy <- A.skipSpace *> A.takeWhile1 isDigit <* A.skipSpace
  hh <- A.takeWhile1 isDigit <* A.char ':'
  mm <- A.takeWhile1 isDigit <* A.char ':'
  ss <- A.takeWhile1 isDigit <* A.skipSpace
  dt <- dayTimeParser
  return $ ww <> mm <> (checkHour dd) <> yyyy  <> (checkHour hh) <> mm <> ss <> dt


checkHour :: Text -> Text
checkHour xs = if ((TX.length xs) == 1) then ("0" <> xs) else xs
  
