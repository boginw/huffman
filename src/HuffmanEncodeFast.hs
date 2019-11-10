module HuffmanEncodeFast
( 
    toBytes,
    fromBytes
) where
        
import Data.Char
import Data.Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Data.Word
import Data.Bits
import Debug.Trace

magic = Seq.fromList ([104,117,102,102,109,97,110] :: [Word8])

readMagic :: Seq.Seq Word8 -> Seq.Seq Word8
readMagic bytes =
    if magic == (Seq.take 7 bytes) then
        Seq.drop 7 bytes
    else
        error "wtf?"

writeMagic :: Seq.Seq Word8
writeMagic = magic

bytesToInt :: Seq.Seq Word8 -> Integer
bytesToInt bytes = 
    let
        ints = Seq.mapWithIndex (\a b -> toInteger b) bytes
    in
        Fold.foldl (\a b -> (a `shift` 8) + b) 0 ints

readInt :: Seq.Seq Word8 -> (Integer, Seq.Seq Word8)
readInt bytes = 
    let
        lengthOfSizeAsByte = bytes `Seq.index` 0
        tail = Seq.drop 1 bytes
        lengthOfSize = fromIntegral lengthOfSizeAsByte
        size = Seq.take lengthOfSize tail
        rest = Seq.drop lengthOfSize tail
    in
        (bytesToInt size, rest)

toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

writeInt int =
    let
        bin = Seq.fromList (toBin int)
        len = length bin
        bytes = fromIntegral (ceiling ((fromIntegral len) / 8)) :: Word8
        bits = 8 * (fromIntegral bytes)
        rem = bits - len
    in
        bytes Seq.<| (bitsToBytes (Seq.replicate rem 0 Seq.>< bin))

toBits :: (FiniteBits byte, Num out) => byte -> Seq.Seq out
toBits x = Seq.fromList [if testBit x i then 1 else 0 | i <- [0.. finiteBitSize x - 1]]

--fromBits :: (Num p, Bits p) => [p] -> p
--fromBits [] = 0
--fromBits (bit:bits) = (bit `shift` (length bits)) + fromBits bits

fromBits :: (Num p, Bits p) => Seq.Seq p -> p
fromBits bits = 
    if len == 0 then 0
    else (bit `shift` (length tail)) + fromBits tail
    where 
        len = length bits
        bit = bits `Seq.index` 0
        tail = Seq.drop 1 bits

bitsToBytes :: (Num p, Bits p) => Seq.Seq p -> Seq.Seq p
bitsToBytes bits = 
    if len == 0 then Seq.empty
    else if len > 8 then 
        ((fromBits (Seq.take 8 bits)) Seq.<| (bitsToBytes (Seq.drop 8 bits)))
    else Seq.singleton (fromBits bits)
    where
        len = length bits

--bytesToBits :: (FiniteBits byte, Num out) => Seq.Seq byte -> Seq.Seq out
bytesToBits bytes = 
    if (length bytes == 0) then Seq.empty
    else (Seq.reverse (toBits s)) Seq.>< (bytesToBits sx)
        where
            s = bytes `Seq.index` 0
            sx = Seq.drop 1 bytes

--writePrefixTableEntry :: (Bits byte, Num byte) => (byte, [byte]) -> [byte]
writePrefixTableEntry entry =
    key Seq.<| (len Seq.<| (bitsToBytes bits))
    where
        key = fst entry
        bits = snd entry
        len = fromIntegral (length bits)

--writePrefixTable :: [(Word8, [Word8])] -> [Word8]
writePrefixTable table =
    let
        entries tbl = 
            if length tbl == 0 then Seq.empty
            else
                (writePrefixTableEntry s) Seq.>< (entries sx)
                where
                    s = tbl `Seq.index` 0
                    sx = Seq.drop 1 tbl
        entriesFromTable = entries table
        len = (length entriesFromTable)
    in
        ((writeInt len) Seq.>< entriesFromTable)

--readPrefixTableEntry :: [Word8] -> ((Word8, [Word8]), [Word8])
readPrefixTableEntry input = 
    ((key, Seq.reverse (prefix length bytes Seq.empty)), rest)
    where
        key = input `Seq.index` 0
        len = input `Seq.index` 1
        bytes = Seq.drop 2 input
        length = fromIntegral len
        bytesUsed = ceiling ((fromIntegral length) / 8)
        rest = Seq.drop bytesUsed bytes
        prefix 0 _ result = result
        prefix length input result = 
            if length >= 8 then
                let 
                    res = prefix (length - 8) rest result Seq.>< bits
                in
                    if (length `mod` 1000 == 0) then
                        trace (show length) res
                    else 
                        res
            else
                result Seq.>< (Seq.take length bits)
            where
                rest = Seq.drop 1 input
                bits :: Seq.Seq Word8
                bits = toBits (input `Seq.index` 0)

--readPrefixTable :: [Word8] -> ([(Word8, [Word8])], [Word8])
readPrefixTable bytesWithSize =
    let
        readSize = readInt bytesWithSize
        size = fromIntegral (fst readSize)
        bytes = snd readSize
        rest = Seq.drop size bytes
        entries 0 bytes result = result
        entries size bytes result = 
            let
                readEntry = readPrefixTableEntry bytes
                entry = fst readEntry
                rest = snd readEntry
            in
                entries (size - ((length bytes) - (length rest))) rest (result Seq.|> entry)
    in
        (Seq.reverse (entries size bytes Seq.empty), rest)

--readBody :: (Num out) => Seq.Seq Word8 -> Seq.Seq out
readBody bytes =
    let
        readSize = readInt bytes
        size = fromIntegral (fst readSize)
        rest = snd readSize
    in
        (bytesToBits (Seq.take size rest))

writeBody :: Seq.Seq Word8 -> Seq.Seq Word8
writeBody body = 
    let 
        bytes = bitsToBytes body
        len = writeInt (length bytes)
    in 
        len Seq.>< bytes

toBytes s = Fold.toList (writeMagic Seq.>< writePrefixTable (snd s) Seq.>< writeBody (fst s))

fromBytes input = 
    let
        bytes = Seq.fromList input
        afterMagic = readMagic bytes
        readPrefix = readPrefixTable afterMagic
        prefixTable = fst readPrefix
        afterPrefixTable = snd readPrefix
        body = readBody afterPrefixTable
    in
        (Fold.toList body, map (\(a, b) -> (a, Fold.toList b)) (Fold.toList prefixTable))