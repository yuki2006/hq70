module Main where


toBinary :: Int -> String
toBinary value
  | value == 0  = ""
  | otherwise   = (toBinary $ value `div` 2) ++ (show $ value `mod` 2)

isPalindrome :: String -> Bool
isPalindrome value = value == reverse value

joinDay :: (Int,Int,Int) -> Int
joinDay (x,y,z) = x*10000 + y*100 + z


check :: (Int,Int,Int) -> Bool
check (a,b,c)
  | otherwise = True
main = do
   
   putStrLn $ show $ filter (isPalindrome . toBinary) $ map joinDay $ filter check [(yy,mm,dd)| yy <- [1964..2020],mm <- [1..12],dd <- [1..31] ]


-- YYYYMMDD → 2進数 → 逆順にして → 10進数 →同じ日付のものを探す