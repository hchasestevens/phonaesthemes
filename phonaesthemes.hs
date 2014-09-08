import qualified Data.Map.Strict as M
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, nub, foldl', stripPrefix, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Function (on)

import System.Directory (getDirectoryContents, makeRelativeToCurrentDirectory, removeFile)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn)

import Control.Monad (forever, when)


data Edge = Edge String String String 
	deriving (Show, Read)  --TODO: minified instancing of show/read

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
		removePrefix str 
			| "/c/en/" `isPrefixOf` str = (fromJust . stripPrefix "/c/en/") str
			| otherwise = str


(prefixMin, prefixMax) = (2, 4)

prefixes :: String -> [String]
prefixes str = map (flip (++) "-") $ map (flip take $ str) [prefixMin..min prefixMax $ length str `div` 2]

suffixes :: String -> [String]
suffixes str = map reverse $ prefixes $ reverse str

ngrams :: Edge -> [String]
ngrams edge = prefixes w ++ suffixes w
	where
		w = word edge

matches :: String -> String -> Bool
matches term prefix
	| ('-':xs) <- prefix = xs `isSuffixOf` term'
	| otherwise = (init prefix) `isPrefixOf` term'
	where
		term' = filter (not . (==) '-') $ (head . splitOn "/") term

ngramPairs :: Edge -> [(String, String)]
ngramPairs edge = [(ngram, end edge) | ngram <- ngrams edge]


type Counter a = M.Map a Int

updateCount :: (Ord a) => a -> Counter a -> Counter a
updateCount k dict = M.insertWith (+) k 1 dict

populateCounts :: (Counter (String, String), Counter String, Counter String) -> (String, String) -> (Counter (String, String), Counter String, Counter String)
populateCounts (assoc_dict, ngram_dict, meaning_dict) pair@(ngram, meaning) = (assoc_dict', ngram_dict', meaning_dict')
	where
		ngram_dict' = updateCount ngram $! ngram_dict
		assoc_dict' = updateCount pair $! assoc_dict
		meaning_dict' = updateCount meaning $! meaning_dict


simedge_cache = ".simedge_cache"
cache = ".cache"

preprocess :: String -> IO ()
preprocess assertion_fname = do
	putStrLn $ "Preprocessing " ++ assertion_fname
	file <- readFile assertion_fname
	let edges = map lineToEdge $ lines file
	let posedges = filter positiveRel edges
	putStrLn "    Writing to simedge cache."
	withFile simedge_cache AppendMode $ \handle -> do
		let simedges = filter simpleStart posedges
		let min_simedges = map minify simedges
		mapM_ (hPutStrLn handle . show) min_simedges
	putStrLn "    Writing to posedge cache."
	withFile cache AppendMode $ \handle -> do
		let min_posedges = map minify posedges
		mapM_ (hPutStrLn handle . show) min_posedges
	return ()

main = do
	ls <- getDirectoryContents "."
	conts <- getDirectoryContents "assertions"
	let csv_fnames = map ((++) "assertions\\") $ filter (isSuffixOf "csv") conts
	-- TODO: discard caches if assertions have been added/removed
	when (any (not . (flip elem) ls) [simedge_cache, cache]) (do
		when (simedge_cache `elem` ls) (removeFile simedge_cache)
		when (cache `elem` ls) (removeFile cache)
		mapM_ preprocess csv_fnames)
	simedges <- readFile simedge_cache
	-- TODO: ngramPairs optionally returns phonemes (SPHINX?)
	let pairs = concat $ filter (not . null) $ map (ngramPairs . read) $ lines simedges
	let (assoc_counts, ngram_totals, meaning_totals) = foldl' populateCounts (M.empty, M.empty, M.empty) pairs
	-- TODO: Further populate with depth-2 concepts (loaded in from .cache)
	-- TODO: Filter by not "matches"
	-- TODO: Lower bound of wilson score on concepts
	putStrLn "Ready."
	-- Mode 1: Interactive, user supplies prefixes or suffixes
	forever $ do
		term <- getLine
		let keys = filter (((==) term) . fst) $ M.keys assoc_counts
		let results = map (\key -> (snd key, fromJust $ M.lookup key assoc_counts)) keys
		let sorted_results = sortBy (compare `on` snd) results
		mapM_ (putStrLn . show) sorted_results
	-- TODO: Mode 2: 
	-- TODO: Sort all pairings by lower bound of wilson score per-prefix/suffix
	-- TODO: Printout top N pairings
	return ()
