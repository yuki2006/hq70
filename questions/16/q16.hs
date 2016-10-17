module Main where

checkNokori :: Int->Int-> Bool
checkNokori len a = 
	any (\k->(len-k)*k==a) [x | x<-[1..len-1]]




check :: (Int,Int) -> Bool
check (a,b)
 | a == b                   = False
 | (a+b)`mod` 2 == 1	    = False
 | otherwise 				= checkNokori (a+b) (((a+b)`div`2)^2-a*b)
main = putStrLn $ show $ filter check [(a,b)|a<-[1..500],b<-[1..500]]