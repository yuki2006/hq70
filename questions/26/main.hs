import Data.List
import Data.Monoid

type Pos = (Int,Int)

find_next:: Pos -> [Pos]
find_next (x,y) 

bfs:: (Pos,Pos) -> [Pos] 
bfs p = bfs_aux [x:[p] | x<-find_next p]
bfs_aux (path:queue)
    | x==1 && y==1 = (return (reverse path)) `mplus` (bfs_aux (1,1) queue)
    | otherwise = bfs_aux (1,1) (queue ++ [x:path|x<-find_next p,not (x `elem` path)])
 where ((x,y),_) = head path

main :: IO()
main = print "Q26"


