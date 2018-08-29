{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Y2018.M07.D24.Exercise where

{--
There are a couple of things to do with today's Haskell exercise.

There's a JSON file associated with this exercise. I'd show you the first
few lines, but I can't because the file:

$ wc Y2018/M07/D24/disambiguation_full_25.json
       0  178958 1403669 Y2018/M07/D24/disambiguation_full_25.json

apparently has '0' lines (one line, no line-feed).

But I will show you the first few characters so you can get an idea of the
structure.
--}

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>) )
-- or, better yet, you do that.

exDir, dict :: FilePath
exDir = "Y2018/M07/D24/"
dict = "disambiguation_full_25.json"

-- Write a function that gives you the first n bytes of a file:

firstNBytes :: Int -> FilePath -> IO ByteString
firstNBytes n file = BSL.take (fromIntegral n) <$> BSL.readFile file

{--
What are the first 250 bytes of this file?

>>> firstNBytes 250 (exDir ++ dict)
"{\"Freshwater mangrove\": {}, \"Mango-pine\": {}, \"Heather Smith\": {\"Heath
er Smith (author)\": \"Heather Smith  author   Australian author\", \"Heather
Smith (curler)\": \"Heather Smith  curler    born 1972  Canadian Curler\", \"H
eather Smith (public servant)\": "

Okay, from the first few bytes of this file, we see we have entries with no
values... that's ... "useful."

But, also, I found that not every element is a map. Separate this dictionary
into ones that have mappings and ones that don't. What is the structure of the
elements that are not mappings?
--}

type Dictionary = Map String Value

loadDictionary :: FilePath -> IO Dictionary
loadDictionary file = do
    json <- decode <$> BSL.readFile file
    case json of
        Just r  -> pure r
        Nothing -> error "This is a partial function and I don't like it"

partitionDictionary :: Dictionary -> (Dictionary, Dictionary)
partitionDictionary = Map.partition isObject
  where
    isObject :: Value -> Bool
    isObject (Object _) = True
    isObject _          = False
{--
>>> dic <- loadDictionary (exDir ++ dict)
>>> length dic
4614

>>> (dics,nondics) = partitionDictionary dic
>>> length nondics
343

That's quite a few entries!

What are these nondics?

>>> take 10 (Map.elems nondics)
[Null,Null,Null,Null,Null,Null,Null,Null,Null,Null]

Huh. Are there any that are not null?
--}

nonNulls :: Dictionary -> Dictionary
nonNulls = Map.filter
    (\case
        Null -> False
        _    -> True
    )

{--
>>> length (nonNulls nondics)
0

Nope, all the nondics are Nulls. So we can get rid of them, and for the dics
we can get rid of the entries that have empty maps. Let's do that.
--}

pruneDictionary :: Dictionary -> Dictionary
pruneDictionary = Map.filter notEmpty
  where
    notEmpty (fromJSON @(Map String String) -> Success ms) =
        not . Map.null $ ms
    notEmpty _ = error "I hate these"

-- Of the original 4614 entries you loaded in, how many entries have data?
sizeOf =
    length . pruneDictionary . fst . partitionDictionary <$> loadDictionary
        (exDir <> dict)
