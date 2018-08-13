{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D10.Exercise where
{--
So, yesterday we explored JSON structure

(EVERYTHING IS A MAP! ... EXCEPT WHAT ISN'T, BUT OKAY!)

Today we're going to explore TWO (much smaller) JSON structures and transform
one to another.

Because, like XML, JSON is all about transformation, baybee!
--}

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy          as BL
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Data.Monoid                    ( (<>) )
import           Data.Scientific                ( toRealFloat )
import           Data.Vector                    ( toList )
-- the input JSON (being output from an analysis tool)

exDir, input :: FilePath
exDir = "exercises/HAD/Y2018/M07/D10/"
input = "output.json"

-- yeah, the input is the output of the analysis tool. Deal.

{--
The input is in the following format:

Map EntityName { wiki_info:  Wiki, scores: [related articles], queryEnt: Int }

where:
--}

data Wiki = Wikt { wname, wtitle, wurl :: String,
                   wtext, wsum, wimg   :: Maybe String }
   deriving Show

instance FromJSON Wiki where
   parseJSON (Object o) = Wikt
    <$> o .: "Entity"
    <*> o .: "Page_title"
    <*> o .: "WikiURL"
    <*> o .: "Full_text"
    <*> o .: "WikiSummary"
    <*> o .: "WikiImg"

type EntityName = String

type Input = Map EntityName Analysis

-- and out Analysis is a composition of the wiki info, scores, and query

data Analysis = Ysis { wikt :: Wiki, scores :: [Value], query :: Double }
   deriving Show

{--
Now, you would think this would Just Work(tm). And it would, if this were
well-structured JSON.

But it's not well-structured JSON. Check out this embedded entry:

    "big brother": {
        "scores": "",
        "query_entity_score": ""
    },

wut. So much for well-structured JSON. How do we deal with this? I don't know.

I think what we have to do is to stage the parsing into ProtoAnalysis then
convert ProtoAnalysis to Analysis iff it has wiki_info. Let's try that.
--}

data ProtoAnalysis = PA { paWik :: Maybe Wiki, paScores, paQuery :: Value }
   deriving Show

instance FromJSON ProtoAnalysis where
   parseJSON (Object o) = PA <$> o .:? "wiki_info" <*> o .: "scores" <*> o .: "query_entity_score"

readProto :: FilePath -> IO (Map EntityName ProtoAnalysis) -- we should decode with Maybe so we don't throw exceptions
readProto file = fromJust . decode <$> BL.readFile file

-- That will work. Now we convert a Proto to Analysis
proto2analysis :: ProtoAnalysis -> Maybe Analysis
proto2analysis PA { paWik = Nothing } = Nothing
proto2analysis PA { paWik = Just wiki, paScores = Array arr, paQuery = Number q }
    = Just $ Ysis wiki (toList arr) (toRealFloat q)

-- then we sequence the result to get our Input value from the JSON

readInputJSON :: FilePath -> IO Input
readInputJSON file = do
    map <- readProto file
    pure $ M.mapMaybe proto2analysis map

-- What is your result? How many entries does your Input Map have?


-- goofing around:
permute :: String -> [String]
permute s = go s ""
  where
    go ""       ret = [reverse ret]
    go (x : xs) ret = if x == '?'
        then go xs ('1' : ret) <> go xs ('0' : ret)
        else go xs (x : ret)
