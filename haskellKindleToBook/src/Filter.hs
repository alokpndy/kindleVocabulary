{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Filter where

import NoteParser
import Types

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import qualified Data.Text.IO   as T
import qualified Data.Text  as TI

onlHighlight xs = do
   case (parseAllClippings xs) of
     Right al ->
       filter (\x -> (clippingType x) == Highlight) al
     Left _ -> []
   
onlyHighlightBody xs  = fmap (\x -> clippingBody x ) (onlHighlight xs)




makeBook file  = do
  file <-  pure $ onlHighlight file
  renderFile "clipping.tex" $ execLaTeXM (accents file)


accents :: [Clipping] -> LaTeXM ()
accents xs  = thePreamble >> document (multipleBody xs)

thePreamble :: LaTeXM ()
thePreamble = do
  documentclass [] book
  usepackage [utf8] inputenc
  title "Kindle Highlights"

theBody :: Clipping ->  LaTeXM ()
theBody xs = do
  maketitle <>  large3 (bodyLine xs) <> tagWord xs <> "\r\n\r\n" <> center "-----------------" <> "\r\n\r\n"
  where
    tagWord xs = flushright  ( (fromString (TI.unpack("by " <> clippingAuthor xs <> ", " <> clippingBook xs))) )
    bodyLine xs = (fromString (TI.unpack( clippingBody xs)))


multipleBody xs = mconcat $  map theBody xs 
