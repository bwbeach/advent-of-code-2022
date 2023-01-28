main :: IO ()
main = do
  print . head . filter root . concatMap (\x -> [x, negate x]) $ [0..]

root :: Integer -> Bool
