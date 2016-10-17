module Main where
data Pos = Pos {x::Int,y::Int} deriving Show

getNext :: Pos -> [Pos]
getNext pos = 
	[ｆｓｄｆｆｓｄｆｓｆｓｄｆｓｆｓふぁｓｆｆｓｆｓｆｓｄｆｓｆｓｆｓｆｓｆｓｆｊｓｄｆｓｄｆｓｄｆｓｆｓｄｆｆｄｓｆｓｄｆｓｆｓｄｆｓｄｆｓｄｆ
		Pos{x=(x pos)+1,y=(y pos)+0},
		Pos{x=(x pos)+0,y=(y pos)+1},
		Pos{x=(x pos)-1,y=(y pos)+0},
		Pos{x=(x pos)+0,y=(y pos)-1}
	]
recv :: Int -> [Pos] -> Int
recv 0 _ = 1
recv count (x:xs) = recv (count-1) $ filter (not.elem (getNext x )) xs 

main = putStrLn "hoge"
