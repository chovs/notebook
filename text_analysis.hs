--- analyzer
--- 1.count the number of sentences from each paragraph
---     1.1 index the different paragraphes
--- 2.break down the numbers of words in each sentence
---     2.1 index 'word' the different words with 'word_count' (word as a sequence)
---     2.2 take sum on 'word_count' put in 'total_word_count'

--- results (total_word_count, paragraphes) as a result

-- The goal of this code is to help a writer or learner better understand the structure of their text.

-- To use this in your writing flow, analyze your draft with this tool after each revision. It provides counts of sentences per paragraph and tracks your word usage, helping you see patterns, inconsistencies, or improvements over time.

import Data.List (group, sort, intercalate)
import Data.Char (toLower, isAlpha, isSpace)

-- ── Types ────────────────────────────────────────────────────────────────────

type Word'      = String
type WordCount  = (Word', Int)          -- (word, count in sentence)

data SentenceAnalysis = SentenceAnalysis
  { sentenceIndex    :: Int
  , sentenceText     :: String
  , wordCounts       :: [WordCount]     -- 2.1: each word with its count
  , totalWordCount   :: Int             -- 2.2: sum of word_count
  } deriving (Show)

data ParagraphAnalysis = ParagraphAnalysis
  { paragraphIndex   :: Int             -- 1.1: paragraph index
  , sentences        :: [SentenceAnalysis]
  , sentenceCount    :: Int             -- 1: number of sentences
  } deriving (Show)

data TextAnalysis = TextAnalysis
  { paragraphs       :: [ParagraphAnalysis]
  , grandTotalWords  :: Int             -- total across all paragraphs
  } deriving (Show)

-- ── Splitting helpers ────────────────────────────────────────────────────────

-- Split text into paragraphs on blank lines
splitParagraphs :: String -> [String]
splitParagraphs = filter (not . null) . splitOn '\n'
  where
    splitOn _ [] = [""]
    splitOn c (x:xs)
      | x == c    = "" : splitOn c xs
      | otherwise = let (h:t) = splitOn c xs in (x:h) : t

-- Split a paragraph into sentences on '.', '!', '?'
splitSentences :: String -> [String]
splitSentences = filter (not . all isSpace) . go []
  where
    go acc []                          = [reverse acc]
    go acc (c:cs)
      | c `elem` ".!?"                 = (reverse (c:acc)) : go [] cs
      | otherwise                      = go (c:acc) cs

-- Tokenise a sentence into lowercase alphabetic words
tokenize :: String -> [Word']
tokenize = words . map (\c -> if isAlpha c then toLower c else ' ')

-- ── Analysis ─────────────────────────────────────────────────────────────────

countWords :: [Word'] -> [WordCount]
countWords = map (\ws -> (head ws, length ws)) . group . sort

analyzeSentence :: Int -> String -> SentenceAnalysis
analyzeSentence idx sent =
  let ws    = tokenize sent
      wc    = countWords ws
      total = length ws
  in  SentenceAnalysis idx sent wc total

analyzeParagraph :: Int -> String -> ParagraphAnalysis
analyzeParagraph idx para =
  let sents    = splitSentences para
      analyzed = zipWith analyzeSentence [1..] sents
  in  ParagraphAnalysis idx analyzed (length analyzed)

analyzeText :: String -> TextAnalysis
analyzeText text =
  let paras    = splitParagraphs text
      analyzed = zipWith analyzeParagraph [1..] paras
      grand    = sum [ totalWordCount s
                     | p <- analyzed, s <- sentences p ]
  in  TextAnalysis analyzed grand

-- ── Pretty printing ───────────────────────────────────────────────────────────

printWordCounts :: [WordCount] -> String
printWordCounts = intercalate ", " . map (\(w,n) -> w ++ ":" ++ show n)

printSentence :: SentenceAnalysis -> String
printSentence s = unlines
  [ "    Sentence " ++ show (sentenceIndex s) ++ ":"
  , "      Text       : " ++ take 60 (sentenceText s) ++ "..."
  , "      Word counts: " ++ printWordCounts (wordCounts s)
  , "      Total words: " ++ show (totalWordCount s)
  ]

printParagraph :: ParagraphAnalysis -> String
printParagraph p = unlines $
  [ "  Paragraph " ++ show (paragraphIndex p)
  , "  Sentences: "  ++ show (sentenceCount p)
  ] ++ map printSentence (sentences p)

printAnalysis :: TextAnalysis -> String
printAnalysis ta = unlines $
  [ "=== Text Analysis ==="
  , "Total paragraphs : " ++ show (length (paragraphs ta))
  , "Grand total words: " ++ show (grandTotalWords ta)
  , ""
  ] ++ map printParagraph (paragraphs ta)

-- ── Entry point ───────────────────────────────────────────────────────────────

main :: IO ()
main = do
  text <- readFile "example.txt"
  let result = analyzeText text
  putStr (printAnalysis result)