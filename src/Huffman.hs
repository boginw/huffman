
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
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
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

-- | From a list, this function counts the occurences of each unique
-- | item in said list and returns a list of pairs of the occurence
-- | how many times it appeared 
frequencyList :: (Num a, Ord k) => [k] -> [(k, a)]
frequencyList list = 
    Map.toList (aux list Map.empty)
    where
        aux [] m = m 
        aux (s:sx) m =
            let
                f = Map.lookup s m
            in
                if isNothing f then
                    aux sx (Map.insert s 1 m)
                else
                    aux sx (Map.insert s (fromJust f + 1) m)

-- | From a list of nodes merges the two nodes with the lowest integer
-- | values, and creates a new node where the leafs of the new node is the
-- | two nodes that are being merged.
mergeTwoLowest :: [Tree [a]] -> [Tree [a]]
mergeTwoLowest flt = 
    let
        srt = sortBy (\(Node (_, a) _ _) (Node (_, b) _ _) -> compare a b) flt
        node1 = head srt
        node2 = head (tail srt)
        node1Val = nodeVal node1
        node2Val = nodeVal node2
        nodeSum = (snd node1Val) + (snd node2Val)
        nodeBits = (fst node1Val) ++ (fst node2Val)
        rest = (tail (tail srt))
    in
        (Node (nodeBits, nodeSum) node1 node2) : rest

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
        aux Leaf p = []
        aux (Node v Leaf Leaf) p = [((head (fst v)), p)]
        aux (Node _ a b) p = 
            let 
                n1 = aux a (p ++ left)
                n2 = aux b (p ++ right)
            in
                n1 ++ n2
    in
        aux tree []

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
        aux l res =
            if (length l == 0) then res
            else aux t (res Seq.>< found)
            where 
                h = l `Seq.index` 0
                t = Seq.drop 1 l
                found = Seq.fromList (fromJust (Map.lookup h prefixMap))
    in
        (
            --(aux (Seq.fromList (reverse s)) Seq.empty), 
            --Seq.fromList (map (\(a, p) -> (a, Seq.fromList p)) prefixes)
            Fold.toList (aux (Seq.fromList payload) Seq.empty), 
            prefixes
        )

-- | Decompress a Huffman compressed payload
decompress :: HuffmanCoding t -> [t]
decompress (b, d) =
    let
        inv = Map.fromList(map (\(a, b) -> (b, a)) d)
        aux [] [] res = res
        aux [] i res = 
            res ++ if (isJust found) then [fromJust found] else []
            where 
                found = Map.lookup i inv
        aux (s:sx) i res = 
            let
                r = Map.lookup i inv
            in
                if (isJust r) then aux (s:sx) [] (res ++ [(fromJust r)])
                                else aux sx (i++[s]) res
    in
        aux b [] []


