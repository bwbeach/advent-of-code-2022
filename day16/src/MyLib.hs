module MyLib (someFunc, add1) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add1 :: Int -> Int
add1 = (+ 1)
