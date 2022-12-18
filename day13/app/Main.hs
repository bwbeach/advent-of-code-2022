module Main where

import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day13a $ input
  print . day13b $ input

data Packet
  = IntPacket Int
  | ListPacket [Packet]
  deriving (Eq)

instance Show Packet where
  show (IntPacket n) = show n
  show (ListPacket ps) = show ps

instance Ord Packet where
  compare a b =
    case a of
      IntPacket na ->
        case b of
          IntPacket nb -> compare na nb
          ListPacket _ -> compare (ListPacket [a]) b
      ListPacket as ->
        case b of
          IntPacket _ -> compare a (ListPacket [b])
          ListPacket bs -> compareLists as bs

traceIt :: Show a => String -> a -> a
traceIt s a = trace (s ++ " " ++ show a) a

compareLists :: [Packet] -> [Packet] -> Ordering
compareLists [] [] = EQ
compareLists [] (_ : _) = LT
compareLists (_ : _) [] = GT
compareLists (a : as) (b : bs) =
  case compare a b of
    EQ -> compareLists as bs
    LT -> LT
    GT -> GT

day13a :: String -> Int
day13a = sum . map fst . filter snd . zip [1 ..] . map (uncurry (<)) . parseInput

day13b :: String -> Int
day13b = product . map (+ 1) . elemIndexes dividerPackets . sort . (++ dividerPackets) . concatMap pairToList . parseInput

elemIndexes :: Eq a => [a] -> [a] -> [Int]
elemIndexes toFind inList = map (\x -> fromJust (elemIndex x inList)) toFind

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a, b]

dividerPackets :: [Packet]
dividerPackets = map parsePacketFromLine ["[[2]]", "[[6]]"]

parseInput :: String -> [(Packet, Packet)]
parseInput = map parsePair . splitOn [""] . lines

parsePair :: [String] -> (Packet, Packet)
parsePair [a, b] = (parsePacketFromLine a, parsePacketFromLine b)
parsePair _ = error "bad pair"

parsePacketFromLine :: String -> Packet
parsePacketFromLine s =
  case parsePacket s of
    (p, "") -> p
    _ -> error "didn't parse entire string"

parsePacket :: String -> (Packet, String)
parsePacket s
  | isDigit (head s) = parseInt s
  | '[' == head s = parseList s
  | otherwise = error "bad packet"

parseInt :: String -> (Packet, String)
parseInt s = (IntPacket (read (takeWhile isDigit s)), dropWhile isDigit s)

parseList :: String -> (Packet, String)
parseList ('[' : s1) =
  let (ps, s2) = parseListInsides s1
   in case s2 of
        (']' : s3) -> (ListPacket ps, s3)
        _ -> error "bad list"
parseList _ = error "bad list"

-- Parses the stuff inside the brackets of a list
parseListInsides :: String -> ([Packet], String)
parseListInsides s =
  if head s == ']'
    then ([], s)
    else
      let (p, s1) = parsePacket s
          (ps, s2) = parseMoreListInsides s1
       in (p : ps, s2)

-- Parses the stuff following some packets in a list
parseMoreListInsides :: String -> ([Packet], String)
parseMoreListInsides s =
  if head s == ','
    then parseListInsides (tail s)
    else ([], s)
