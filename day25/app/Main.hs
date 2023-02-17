module Main where

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: FilePath -> IO ()
runWithFile fileName = do
  putStrLn fileName
  input <- readFile fileName
  putStrLn . integerToSnafu . sum . map snafuToInteger . lines $ input

snafuToInteger :: String -> Integer
snafuToInteger =
  foldl addDigit 0
  where
    addDigit soFar c = soFar * 5 + valueOfDigit c

    valueOfDigit c =
      case c of
        '=' -> (-2)
        '-' -> (-1)
        '0' -> 0
        '1' -> 1
        '2' -> 2
        _ -> error ("bad digit: " ++ [c])

integerToSnafu :: Integer -> String
integerToSnafu number =
  if number == 0
    then "0"
    else convert number
  where
    convert n =
      if n == 0
        then ""
        else convert (n `div` 5 + carry) ++ [digit]
      where
        (digit, carry) = modToDigitAndCarry (n `mod` 5)

    modToDigitAndCarry n =
      case n of
        0 -> ('0', 0)
        1 -> ('1', 0)
        2 -> ('2', 0)
        3 -> ('=', 1)
        4 -> ('-', 1)
        _ -> error ("bad mod: " ++ show n)