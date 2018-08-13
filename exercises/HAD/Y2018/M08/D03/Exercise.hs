{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Y2018.M08.D03.Exercise where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Maybe                     ( fromJust )

{--
Another 'discovering the structure of JSON' Haskell exercise today.

You have the follow JSON file:
--}

exDir, newsJSON :: FilePath
exDir = "exercises/HAD/Y2018/M08/D03/"
newsJSON = "news.json"

-- 1. read in the JSON (unprettified) and write it out as pretty JSON

prettify :: FilePath -> FilePath -> IO ()
prettify unprettyIn prettyOut =
    (encodePretty . decode @[Article] <$> BL.readFile unprettyIn)
        >>= BL.writeFile prettyOut

-- this function may be helpful for solving prettify ...

listVals :: ByteString -> [Value]
listVals = fromJust . decode

-- 2. what structures can you see from this enprettified set?

-- ... or ... that's a tough question, let's take this approach, instead:

data Article = Art {
    author           :: String
    , image          :: FilePath
    , url            :: FilePath
    -- , published, updated :: Date
    , article        :: String
    , idx            :: Integer
    , summary, title :: String }
   deriving (Eq, Show)

instance FromJSON Article where
    parseJSON = withObject "Article" $ \v -> do
        authorO <- v .: "author_meta"
        author <- authorO .: "display_name"
        imageO <- v .: "image"
        image <- imageO .: "url"
        url <- v .: "link"
        article <- (v .: "content") >>= (.: "rendered") -- instead of using articleO
        idx <- v .: "id"
        summary <- v .: "lede"
        title <- v .: "title"
        pure Art {author,url, image, article, summary, idx, title}

instance ToJSON Article where
    toJSON Art {author,image,url,article,idx,summary,title} = object
        [
            "author" .= author
            , "image" .= image
            , "url" .= url
            , "article" .= article
            , "idx" .= idx
            , "summary" .= summary
            , "title" .= title
        ]
{--
The mapping from the Haskell values to the JSON is as follows:

author       ==> author_meta.display_name
image        ==> image.url
url          ==> link
article      ==> content.rendered
published    ==> date
updated      ==> modified
idx          ==> id
summary      ==> lede
title        ==> title

map the JSON to the above structure.
--}

readArticles :: FilePath -> IO [Article]
readArticles json = fromJust . decode <$> BL.readFile json

-- will listVals help here?

-- a. how many articles are there?
-- 2    length <$> readArticles (exDir ++ newsJSON)
-- b. what was the max id of the article set?
-- maximum . map idx <$> readArticles (exDir ++ newsJSON)
-- 134471
