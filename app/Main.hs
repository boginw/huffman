module Main where

import System.Environment
import qualified Huffman
import qualified HuffmanSerialization
import qualified Data.ByteString as ByteString
import Data.List
import Data.Word

-- | The usage information to be printed whenever the
-- | user does something that is considered illegal
usage :: String
usage = "Usage: huffman input"

-- | Either compresses or decompresses the bytes provided
-- | using the Huffman method depending on the ending of 
-- | the file name
getBytes :: String -> [Word8] -> [Word8]
getBytes path fileBytes = 
    if ".huf" `isSuffixOf` path then
        Huffman.decompress (HuffmanSerialization.fromBytes fileBytes)
    else
        HuffmanSerialization.toBytes (Huffman.compress fileBytes)

-- | Generates the output path depending on if we are
-- | compressing or decompressing
outputPath :: String -> String
outputPath path =
    if ".huf" `isSuffixOf` path then
        (take (length path - 4) path) ++ ".orig"
    else
        path ++ ".huf"

main :: IO ()
main = do
    args <- getArgs

    if length args /= 1 then
        error usage
    else
        putStrLn "Reading file"

    let fileName = (head args)
    fileBytesString <- ByteString.readFile fileName
    let fileBytes = ByteString.unpack fileBytesString
    let bytes = ByteString.pack (getBytes fileName fileBytes)
    let output = outputPath fileName
    ByteString.writeFile output bytes
    putStrLn "Done"
