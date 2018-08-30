{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Y2018.M08.D15.Exercise where

{--
Today we have a list of the top 5000 English language words (by frequency) from
https://www.wordfrequency.info. We are going to parse this file and answer
a question.
--}

import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Prelude                 hiding ( Word )
-- below modules available via 1HaskellADay git repository

import           Control.Scan.CSV

exDir, tsvFile :: FilePath
exDir = "Y2018/M08/D15/"
tsvFile = "words_counts.tsv"

-- The TSV file has a header and footer, so watch out. Parse the file into
-- the following structure:

type Word = String
type PartOfSpeech = Char

data WordFreq = WF { word :: Word, partOfSpeech :: PartOfSpeech,
                     freq :: Int, dispersion :: Float }
    deriving Show

-- the parts of speech are catalogued here: ... um ... okay, bad link.
-- deal with it: guess away.

-- So, our word frequencies:

readWordFreqs :: FilePath -> IO (Vector WordFreq)
readWordFreqs file = do
    f <- readFile file
    let words = lines f
    let wfs   = fmap read words :: [WordFreq]
    pure $ V.fromList wfs

-- taken from soln.
convert2WF :: [String] -> [(WordFreq, String)]
convert2WF [w, p, f, d] = reads f >>= \(freq, _) ->
    reads d >>= \(disp, _) -> return (WF w (head p) freq disp, "")
convert2WF _ = []

instance Read WordFreq where
    readsPrec _ = convert2WF . splitWF

splitWF :: String -> [String]
splitWF = filter (not . null) . rendBy (== ' ')

-- What is the part-of-speech that has the most words? second most? etc?
partsOfSpeech :: Vector WordFreq -> Map PartOfSpeech [Word]
partsOfSpeech wordfreqs = undefined
    {-V.foldr
    (\(WF word partOfSpeech _ _) m -> M.insert partOfSpeech word)
    M.empty
    wordfreqs-}

-- you can use a multi-map to construct the above result if you'd like

-- What are the words of length 5 in this list? Or, more generally, what are
-- the words of length n?

type Length = Int

nwords :: Vector WordFreq -> Map Length [Word]
nwords wordfreqs = undefined

-- Again, use a multi-map if you'd like
