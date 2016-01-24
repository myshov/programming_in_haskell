import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2: int2bin(n `div` 2)

createParity :: [Bit] -> Bit
createParity bits = (sum [1 | x <- bits, x == 1]) `mod` 2

checkParity :: [Bit] -> [Bit]
checkParity bits | p == lastBit = take 8 bits
                 | otherwise = error "Error during transmition"
                 where p = createParity (take 8 bits)
                       lastBit = last bits

makePart :: [Bit] -> [Bit]
makePart bits = take 8 (bits ++ repeat 0) ++ [createParity bits]

encode :: String -> [Bit]
encode = concat.map(makePart.int2bin.ord)

chopPart :: [Bit] -> [[Bit]]
chopPart []= []
chopPart bits = take 9 bits: chopPart (drop 9 bits)

decode :: [Bit] -> String
decode = map(chr.bin2int.checkParity).chopPart

transmit :: String -> String
transmit = decode.channel.encode

channel :: [Bit] -> [Bit]
channel = id

transmitWithErrors :: String -> String
transmitWithErrors = decode.brokenChannel.encode

brokenChannel :: [Bit] -> [Bit]
brokenChannel = tail
