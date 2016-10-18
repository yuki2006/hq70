import Data.List
import Data.Monoid

data Shot = Single Int | Double Int Int deriving (Show)

type Sequence = [Shot]

allShots :: Sequence
allShots = (Single <$> [1..9]) ++
        ((uncurry Double) <$> filter doubleCheck [(a,b) | a <- [1..9], b <- [1..9]])

numbers :: Shot -> [Int]
numbers (Single x) = [x]
numbers (Double x y) = [x,y]


isOverlapping :: Shot -> Shot -> Bool
isOverlapping x y = (not . null) $ intersect (numbers x) (numbers y)

except :: Shot -> Sequence -> Sequence
except x = filter (not . (isOverlapping x))

pitch :: Sequence -> [Sequence]
pitch [] = [[]]
pitch shots = do
    x <- shots
    fmap (x:) $ pitch (except x shots)

main' :: IO ()
main' = print $ length $ pitch allShots


pitchM :: Sequence -> Sum Int
pitchM [] = Sum 1
pitchM shots = mconcat $ do
    x <- shots
    return $ pitchM (except x shots)

doubleCheck:: (Int, Int) -> Bool
doubleCheck (5 , _) = False
doubleCheck (_ , 5) = False
doubleCheck (a , b) = 
    a < b &&
    a /= b && 
    ( sameRow || sameColumn )
    where
        ar = (a-1) `div` 3
        ac = (a-1) `mod` 3
        br = (b-1) `div` 3
        bc = (b-1) `mod` 3 
        sameRow = ar == br && (abs $ ac-bc) == 1
        sameColumn = ac == bc && (abs $ ar-br) == 1


main :: IO()
main = print $ getSum $ pitchM allShots


