-- Bogi Napoleon Wennerstrøm (bwenne16@student.aau.dk)
module Huffman
( 
    compress,
    decompress,
    PrefixEntry,
    PrefixTable,
    HuffmanCoding
) where
        
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Word

type PrefixEntry t = (t, [Word8])
type PrefixTable t = [PrefixEntry t]
type HuffmanCoding t = ([Word8], PrefixTable t)

type TreeValue a = (a, Integer)
data Tree a = Leaf | Node (TreeValue a) (Tree a) (Tree a) deriving Show

-- | The encoding for going left in a tree
left :: [Word8]
left = [1 :: Word8]
-- | The encoding for going right in a tree
right :: [Word8]
right = [0 :: Word8]

-- | Takes a node and returns its value
nodeVal :: Tree a -> (TreeValue a)
nodeVal (Node v _ _) = v

-- | Determines if a tree is a leaf node
isLeaf :: Tree a -> Bool
isLeaf Leaf = True
isLeaf (Node _ _ _) = False

-- | From a prefix creates a predicate function which determines if
-- | a given prefix entry's prefix starts with the first prefix
startsWith :: [Word8] -> PrefixEntry t -> Bool
startsWith prefix = (\entry -> prefix `isPrefixOf` (snd entry))

-- | From a list, this function counts the occurences of each unique
-- | item in said list and returns a list of pairs of the occurence
-- | how many times it appeared 
frequencyList :: (Num a, Ord k) => [k] -> [(k, a)]
frequencyList list = map (\a -> (head a, fromIntegral (length a))) (group (sort list))

-- | From a list of nodes this function merges the first two nodes 
-- | together creating a new node where the leafs of the new node 
-- | is the two nodes that are being merged.
mergeTwoFirst :: [Tree [a]] -> [Tree [a]]
mergeTwoFirst (n1:n2:rest) =
    let
        v1 = nodeVal n1
        v2 = nodeVal n2
        sum = (snd v1) + (snd v2)
        bits = (fst v1) ++ (fst v2)
    in
        (Node (bits, sum) n1 n2) : rest

-- | From a list of nodes merges the two nodes with the lowest integer
-- | values, and creates a new node where the leafs of the new node is the
-- | two nodes that are being merged.
mergeTwoLowest :: [Tree [a]] -> [Tree [a]]
mergeTwoLowest flt = mergeTwoFirst (sortBy comparator flt)
    where comparator = \(Node (_, a) _ _) (Node (_, b) _ _) -> compare a b

-- | From a list of trees, this function merges all the nodes together
-- | to a single node, always merging the two nodes with the lowest
-- | value first
buildTree :: [Tree [a]] -> Tree [a]
buildTree (x:[]) = x
buildTree flt = buildTree (mergeTwoLowest flt)

-- | From a tree, this function collapses the tree into a list of
-- | prefix entries. The function encodes the path needed to find
-- | each node, and then the value of the node in a pair.
collapseTree :: Tree [a] -> PrefixTable a
collapseTree tree =
    let 
        visit Leaf p = []
        visit (Node v Leaf Leaf) p = [((head (fst v)), p)]
        visit (Node _ a b) p = 
            let 
                n1 = visit a (p ++ left)
                n2 = visit b (p ++ right)
            in
                n1 ++ n2
    in
        visit tree []

-- | Compress a sequence of ordinals using Huffman's compression
-- | technique. The returning pair consists of the compressed payload
-- | and then the prefix table
compress :: Ord t => [t] -> HuffmanCoding t
compress payload = 
    let 
        frequencies = frequencyList payload
        treeNodes = map (\(a, b) -> Node ([a], b) Leaf Leaf) frequencies
        tree = buildTree treeNodes
        prefixes = collapseTree tree
        prefixMap = Map.fromList prefixes
        replaceWithPrefixes [] res = res
        replaceWithPrefixes (s:sx) res = replaceWithPrefixes sx (res ++ prefix)
            where prefix = fromJust (Map.lookup s prefixMap)
    in
        (replaceWithPrefixes payload [], prefixes)

-- | Decompress a Huffman compressed payload
decompress :: HuffmanCoding t -> [t]
decompress (payload, prefixes) =
    let
        replacePrefixes ([], (p:[])) _ = [fst p]
        replacePrefixes ([], _) _ = []
        replacePrefixes (payload, (p:[])) _ = (fst p) : (replacePrefixes (payload, prefixes) [])
        replacePrefixes ((s:sx), prefixes) word = 
            let word' = word ++ [s]
            in replacePrefixes (sx, (filter (startsWith word') prefixes)) word'
    in
        replacePrefixes (payload, prefixes) []
