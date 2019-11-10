-- Bogi Napoleon Wennerstrøm (bwenne16@student.aau.dk)
module HuffmanSerialization
( 
    toBytes,
    fromBytes
) where
        
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Huffman
import Data.Word
import Data.Bits

-- |-------------------------------------------|
-- | Huffman File Format:                      |
-- |-------------------------------------------|
-- | 1. Magic number                           |
-- |     * 7 bytes "huffman"                   |
-- | 2. Prefix table:                          |
-- |     1. Length of the prefix table length  |
-- |     2. prefix table length in bytes       |
-- |     3. prefix table, for each entry:      |
-- |         1. key: 1 byte                    |
-- |         2. length of value: 1 byte        |
-- |         3. value: "length of value" bits  |
-- | 3. Body                                   |
-- |     1. length of file size                |
-- |     2. file size                          |
-- |     3. body                               |
-- |-------------------------------------------|


-- | The 'magic' bytes for the file format, spells out "huffman"
magic :: [Word8]
magic = [104,117,102,102,109,97,110] :: [Word8]

-- | Ensures that the first several bytes of a byte list is the
-- | magic bytes
readMagic :: [Word8] -> [Word8]
readMagic bytes =
    if magic == (take magicLength bytes) then
        drop magicLength bytes
    else
        error "Could not find magic bytes"
    where
        magicLength = length magic

-- | Writes the 'magic' bytes, can be considered redundant due to
-- | the already existing 'magic' declaration
writeMagic :: [Word8]
writeMagic = magic

-- | Takes a list of bytes and shifts appropriately and adds them
-- | to produce an integer
bytesToInt :: [Word8] -> Integer
bytesToInt bytes = 
    let
        ints = map toInteger bytes
    in
        foldl (\a b -> (a `shift` 8) + b) 0 ints

-- | Takes a number and converts it to binary in list form
-- | Example 129 -> [1,0,0,0,0,0,0,1]
toBin :: (Integral inp, Num out) => inp -> [out]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

-- | Takes a byte and converts it to 'ones-and-zeros' in list form
-- | TODO: look at possibility to merge with 'toBin'
toBits :: (FiniteBits byte, Num out) => byte -> [out]
toBits x = [if testBit x i then 1 else 0 | i <- [0.. finiteBitSize x - 1]]

-- | Takes a list of bits and converts them it into a single number, by
-- | shifting them appropriately and adding them together.
fromBits :: (Num p, Bits p) => [p] -> p
fromBits [] = 0
fromBits (bit:bits) = (bit `shift` (length bits)) + fromBits bits

-- | Groups a list of bits into groups of 8, and then converts each group
-- | to a byte by using 'fromBits'
bitsToBytes :: (Num p, Bits p) => [p] -> [p]
bitsToBytes [] = []
bitsToBytes bits = 
    if len > 8 then 
        (fromBits (take 8 bits)) : (bitsToBytes (drop 8 bits))
    else [fromBits bits]
    where
        len = length bits

-- | Splits a list of bytes into a list of bits
bytesToBits :: (FiniteBits byte, Num out) => [byte] -> [out]
bytesToBits [] = []
bytesToBits (s:sx) = (reverse (toBits s)) ++ (bytesToBits sx)

-- | Reads an integer from a list of bytes and returns that integer
-- | and the remaining bytes in a tuple.
readInt :: [Word8] -> (Integer, [Word8])
readInt (lengthOfSizeAsByte:bytes) = 
    let
        lengthOfSize = fromIntegral lengthOfSizeAsByte
        size = take lengthOfSize bytes
        rest = drop lengthOfSize bytes
    in
        (bytesToInt size, rest)

-- | Writes an integer to a list of bytes. The compliment of 'readInt'
writeInt :: Integral inp => inp -> [Word8]
writeInt int =
    let
        bin = toBin int
        len = length bin
        bytes = fromIntegral (ceiling ((fromIntegral len) / 8)) :: Word8
        bits = 8 * (fromIntegral bytes)
        rem = bits - len
    in
        bytes : (bitsToBytes (replicate rem 0 ++ bin))

-- | Writes a prefix table entry to a list of bytes
writePrefixTableEntry :: PrefixEntry Word8 -> [Word8]
writePrefixTableEntry entry =
    key : len : (bitsToBytes bits)
    where
        key = fst entry
        bits = snd entry
        len = fromIntegral (length bits)

-- | Writes a prefix table to a list of bytes
writePrefixTable :: PrefixTable Word8 -> [Word8]
writePrefixTable table =
    let
        entries [] = []
        entries (s:sx) = (writePrefixTableEntry s) ++ (entries sx)
        entriesFromTable = entries table
        len = (length entriesFromTable)
    in
        (writeInt len ++ entriesFromTable)

-- | Reads a prefix table entry from a list of bytes and returns
-- | the prefix table entry and the rest of the bytes after the
-- | entry as a pair of (prefix table entry, rest of the bytes)
readPrefixTableEntry :: [Word8] -> (PrefixEntry Word8, [Word8])
readPrefixTableEntry (key:len:bytes) = 
    ((key, reverse (prefix length bytes [])), rest)
    where
        length = fromIntegral len
        bytesUsed = ceiling ((fromIntegral length) / 8)
        rest = drop bytesUsed bytes
        prefix 0 _ result = result
        prefix length (first:bytes) result = 
            if length >= 8 then
                prefix (length - 8) bytes result ++ bits
            else
                result ++ (take length bits)
            where
                bits :: [Word8]
                bits = toBits first

-- | Reads a prefix table from a list of bytes and returns the
-- | prefix table and the rest of the bytes after the table as
-- | a pair of (prefix table, rest of the bytes)
readPrefixTable :: [Word8] -> (PrefixTable Word8, [Word8])
readPrefixTable bytesWithSize =
    let
        readSize = readInt bytesWithSize
        size = fromIntegral (fst readSize)
        bytes = snd readSize
        rest = drop size bytes
        entries 0 bytes result = result
        entries size bytes result = 
            let
                readEntry = readPrefixTableEntry bytes
                entry = fst readEntry
                restBytes = snd readEntry
                restSize = (size - ((length bytes) - (length restBytes)))
            in
                entry : (entries restSize restBytes result)
    in
        ((entries size bytes []), rest)

-- | Reads the body from a list of bytes and ignores everything after.
readBody :: (Num out) => [Word8] -> [out]
readBody bytes =
    let
        readSize = readInt bytes
        size = fromIntegral (fst readSize)
        rest = snd readSize
    in
        (bytesToBits (take size rest))

-- | Writes the body to a list of bytes
writeBody :: [Word8] -> [Word8]
writeBody body = 
    let 
        bytes = bitsToBytes body
        len = writeInt (length bytes)
    in 
        len ++ bytes

-- | Takes a huffman coding and converts it into a list of bytes
toBytes :: HuffmanCoding Word8 -> [Word8]
toBytes s = writeMagic ++ writePrefixTable (snd s) ++ writeBody (fst s)

-- | Takes a serialized huffman coding and deserializes it into 
-- | a hufffman coding
fromBytes :: [Word8] -> HuffmanCoding Word8
fromBytes bytes = 
    let
        afterMagic = readMagic bytes
        readPrefix = readPrefixTable afterMagic
        prefixTable = fst readPrefix
        afterPrefixTable = snd readPrefix
        body = readBody afterPrefixTable
    in
        (body, prefixTable)