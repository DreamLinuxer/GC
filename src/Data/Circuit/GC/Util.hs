module Data.Circuit.GC.Util where

import Data.Bits as B
import Data.Serialize
import Data.Array
import Data.Int
import Data.ByteString as BS
import qualified Data.ByteArray as BA
import Crypto.Hash
import Data.List as L

type Table = [((Bool, Bool), Bool)]
type GTable = Array Int32 ByteString

-- a label is 16 bytes
labelSize :: Int
labelSize = 32

randByteString :: ByteString -> Int -> ByteString
randByteString seed n = BA.convert $ hashWith SHA256 $ append (encode n) seed

permute :: Bool -> Bool -> Bool -> Table -> Table
permute p1 p2 p3 = sortOn fst . fmap (\((x,y),z) -> ((x `xor` p1, y `xor` p2), z `xor` p3))

encrypt :: ByteString -> ByteString -> ByteString -> ByteString -> Table -> GTable
encrypt delta label1 label2 label3 =
  listArray (0,3) . 
  fmap (\((x,y),z) -> hashWith SHA256 (BA.append (aux x label1) (aux y label2))
                      `BA.xor`
                      aux' z label3)
  where
    aux b l = if b then l `BA.xor` delta else l
    aux' b bs = let bs' = aux b bs in
                  BS.cons (set b (BS.head bs')) (BS.tail bs')
    set b w = if b then w `setBit` 0
                   else w `clearBit` 0

decrypt :: ByteString -> ByteString -> ByteString -> Bool
decrypt label1 label2 msg = getBit $ hashWith SHA256 (BS.append label1 label2) `BA.xor` msg

getBit :: ByteString -> Bool
getBit bs = B.testBit (BS.head bs) 0

clear :: ByteString -> ByteString
clear bs = BS.cons (BS.head bs `clearBit` 0) (BS.tail bs)

indexB :: Bool -> Bool -> Int
indexB b1 b2 = (aux b1) * 2 + (aux b2)
  where
    aux b = if b then 1 else 0

and_table, or_table :: Table
and_table = [((False, False), False),
             ((False,  True), False),
             (( True, False), False),
             (( True,  True),  True)]
or_table  = [((False, False), False),
             ((False,  True),  True),
             (( True, False),  True),
             (( True,  True),  True)]
