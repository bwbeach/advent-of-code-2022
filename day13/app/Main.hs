module Main where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day13 $ input

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

day13 :: String -> Int
day13 = sum . map fst . filter snd . zip [1 ..] . map (uncurry (<)) . parseInput

parseInput :: String -> [(Packet, Packet)]
parseInput = map parsePair . splitOn [""] . lines

parsePair :: [String] -> (Packet, Packet)
parsePair [a, b] = (parsePacketFromLine a, parsePacketFromLine b)
parsePair _ = error "bad pair"

parsePacketFromLine :: String -> Packet
parsePacketFromLine s = fst (parsePacket s)

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

parseListInsides :: String -> ([Packet], String)
parseListInsides s =
  if head s == ']'
    then ([], s)
    else
      let (p, s1) = parsePacket s
       in if head s1 == ','
            then let (ps, s2) = parseListInsides (tail s1) in (p : ps, s2)
            else ([p], s1)
