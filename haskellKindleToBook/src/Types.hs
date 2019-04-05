{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- # module
module Types where

-- # imports
import Data.Text (Text, pack) 
import GHC.Generics
import Control.Applicative
--i
import Data.Aeson

-- | Clipping Type
data Clipping = Clipping
  { clippingAuthor :: Text,
    clippingBook   :: Text,
    clippingType   :: ClipFormat,
    clippingTag    :: Text, -- date and time in integer
    clippingBody   :: Text
  } deriving (Show, Eq, Generic)


-- | Clipping formatt
data ClipFormat = Note | Highlight | Bookmark
  deriving (Show, Eq, Generic )

type WeekDay =  Text
type Month = Text

instance ToJSON Clipping
instance FromJSON Clipping

instance ToJSON ClipFormat where
    toJSON  = String . pack . show
    
instance FromJSON ClipFormat where
    parseJSON = withText "ClipFormat" f
      where
        f "Note" = return Note
        f "Highlight" = return Highlight
        f "Bookmark" = return Bookmark
      
{--  Example of raw data

4: Harry Potter and the Goblet of Fire (J.K. Rowling)
- Your Highlight on Location 8866-8867 | Added on Monday, February 2, 2015 9:42:48 AM

If you want to know what a man’s like, take a good look at how he treats his inferiors, not his equals.”
==========

-}
 
