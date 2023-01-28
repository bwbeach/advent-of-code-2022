main :: IO ()
main = do
  print . head . filter root . concatMap (\x -> [x, negate x]) $ [0..]

root :: Integer -> Bool
root x = pppw x == sjmn x
dbpl _ = 5
cczh x = sllz x + lgvd x
zczc _ = 2
ptdq x = humn x - dvpt x
dvpt _ = 3
lfqf _ = 4
humn x = x
ljgn _ = 2
sjmn x = drzm x * dbpl x
sllz _ = 4
pppw x = cczh x `div` lfqf x
lgvd x = ljgn x * ptdq x
drzm x = hmdt x - zczc x
hmdt _ = 32
