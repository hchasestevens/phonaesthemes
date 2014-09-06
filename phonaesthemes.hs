import Data.List (isSuffixOf, isInfixOf, isPrefixOf, nub, foldl', stripPrefix, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Function (on)
import System.Directory (getDirectoryContents, makeRelativeToCurrentDirectory)
import Control.Monad (forever)
import qualified Data.Map.Strict as M


data Edge = Edge {
	rel :: String,
	start :: String,
	end :: String
} deriving (Show)

toEdge :: [String] -> Edge
toEdge (_:r:s:e:_) = Edge {rel=r, start=s, end=e}

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
minify edge = Edge {
		rel="",
		start=removePrefix $ start edge,
		end=removePrefix $ end edge
	}
	where
		removePrefix = fromJust . stripPrefix "/c/en/"


(prefixMin, prefixMax) = (2, 4)

prefixes :: String -> [String]
prefixes str = map ((:) '_') $ map (flip take $ str) [prefixMin..min prefixMax $ length str `div` 2]

suffixes :: String -> [String]
suffixes str = map reverse $ prefixes $ reverse str

ngrams :: Edge -> [String]
ngrams edge = prefixes w ++ suffixes w
	where
		w = word edge

ngramPairs :: Edge -> [(String, String)]
ngramPairs edge = [(ngram, end edge) | ngram <- ngrams edge]


updateCount :: (Ord a) => M.Map a Int -> a -> M.Map a Int
updateCount dict k = M.insertWith (+) k 1 dict

populateCounts :: (M.Map (String, String) Int, M.Map String Int) -> (String, String) -> (M.Map (String, String) Int, M.Map String Int)
populateCounts (assoc_dict, count_dict) pair@(ngram, _) = (assoc_dict', count_dict')
	where
		count_dict' = updateCount count_dict ngram
		assoc_dict' = updateCount assoc_dict pair


main = do
	conts <- getDirectoryContents "assertions"
	let csv_fnames = map ((++) "assertions\\") $ filter (isSuffixOf "csv") conts
	csvs <- mapM readFile csv_fnames
	let edges = [lineToEdge line | csv_lines <- map lines csvs, line <- csv_lines]
	let posedges = filter positiveRel edges
	print $ head posedges
	let simedges = filter simpleStart posedges
	print $ head simedges
	let minedges = map minify simedges
	print $ head minedges
	print $ length minedges
	let pairs = concat $ filter (not . null) $ map ngramPairs minedges
	let (assoc_counts, totals) = foldl' populateCounts (M.empty, M.empty) pairs
	--let cr_keys = filter (((==) "_gl") . fst) $ M.keys assoc_counts
	--let cr_results = map (\key -> (snd key, fromJust $ M.lookup key assoc_counts)) cr_keys
	--let sorted_cr_results = sortBy (compare `on` snd) cr_results
	--mapM_ (putStrLn . show) sorted_cr_results
	forever $ do
		term <- getLine
		let keys = filter (((==) term) . fst) $ M.keys assoc_counts
		let results = map (\key -> (snd key, fromJust $ M.lookup key assoc_counts)) keys
		let sorted_results = sortBy (compare `on` snd) results
		mapM_ (putStrLn . show) sorted_results
