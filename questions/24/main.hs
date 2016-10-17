module Main where

doubleCheck:: Int -> Int -> Bool
doubleCheck a b = 
    sameRow && (abs $ac-bc) == 1 ||
    sameColumn && (abs $ar-br) == 1
    where
        ar = (a-1) `div` 3
        ac = (a-1) `mod` 3
        br = (b-1) `div` 3
        bc = (b-1) `mod` 3 
        sameRow = ar == br
        sameColumn = ac == bc

main = putStrLn $ show $ doubleCheck 1 4
