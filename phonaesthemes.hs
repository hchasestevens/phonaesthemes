import Data.List (isSuffixOf, isInfixOf, isPrefixOf, nub, foldl', stripPrefix, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Function (on)
import System.Directory (getDirectoryContents, makeRelativeToCurrentDirectory)
import Control.Monad (forever)
import qualified Data.Map.Strict as M


data Edge = Edge String String String 
	deriving (Show)

rel (Edge r _ _) = r
start (Edge _ s _) = s
end (Edge _ _ e) = e

toEdge :: [String] -> Edge
toEdge (_:r:s:e:_) = Edge r s e

lineToEdge :: String -> Edge
lineToEdge = toEdge . splitOn "\t"

positiveRel :: Edge -> Bool
positiveRel edge = not $ any (flip isInfixOf $ rel edge) terms
	where
		terms = ["Not", "Antonym", "TranslationOf", "AtLocation"]

simpleStart :: Edge -> Bool
simpleStart e = foldr1 (&&) conds
	where
		s = start e
		conds = ["/c/en" `isPrefixOf` s,
			     "/c/en" `isPrefixOf` (end e),
			     (length . (splitOn "/") $ s) == 4,
			     (not . (isInfixOf "_")) s
			     ]

word :: Edge -> String
word edge = last . splitOn "/" $ start edge

minify :: Edge -> Edge
minify edge = Edge [] (removePrefix $ start edge) (removePrefix $ end edge)
	where
		removePrefix = fromJust . stripPrefix "/c/en/"


(prefixMin, prefixMax) = (2, 4)

prefixes :: String -> [String]
prefixes str = map (flip (++) "-") $ map (flip take $ str) [prefixMin..min prefixMax $ length str `div` 2]

suffixes :: String -> [String]
suffixes str = map reverse $ prefixes $ reverse str

ngrams :: Edge -> [String]
ngrams edge = prefixes w ++ suffixes w
	where
		w = word edge

ngramPairs :: Edge -> [(String, String)]
ngramPairs edge = [(ngram, end edge) | ngram <- ngrams $! edge]


updateCount :: (Ord a) => M.Map a Int -> a -> M.Map a Int
updateCount dict k = M.insertWith (+) k 1 dict

type Counter a = M.Map a Int

populateCounts :: (Counter (String, String), Counter String, Counter String) -> (String, String) -> (Counter (String, String), Counter String, Counter String)
populateCounts (assoc_dict, ngram_dict, meaning_dict) pair@(ngram, meaning) = (assoc_dict', ngram_dict', meaning_dict')
	where
		ngram_dict' = updateCount ngram_dict ngram
		assoc_dict' = updateCount assoc_dict pair
		meaning_dict' = updateCount meaning_dict meaning


main = do
	conts <- getDirectoryContents "assertions"
	let csv_fnames = map ((++) "assertions\\") $ filter (isSuffixOf "csv") conts
	csvs <- mapM readFile csv_fnames
	let edges = [lineToEdge line | csv_lines <- map lines csvs, line <- csv_lines]
	let posedges = filter positiveRel edges
	--print $ head posedges
	let simedges = filter simpleStart posedges
	--print $ head simedges
	let minedges = map minify simedges
	--print $ head minedges
	let pairs = concat $ filter (not . null) $ map ngramPairs minedges
	let (assoc_counts, ngram_totals, meaning_totals) = foldl' populateCounts (M.empty, M.empty, M.empty) pairs
	putStrLn "Ready."
	forever $ do
		term <- getLine
		let keys = filter (((==) term) . fst) $ M.keys assoc_counts
		let results = map (\key -> (snd key, fromJust $ M.lookup key assoc_counts)) keys
		let sorted_results = sortBy (compare `on` snd) results
		mapM_ (putStrLn . show) sorted_results
