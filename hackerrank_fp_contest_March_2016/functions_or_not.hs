-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import Control.Applicative
import Data.List

readInts :: IO [Int]
readInts = map read . words <$> getLine

check [[x,y]] = "YES"
check ([x1,y1]:t@([x2,y2]:_)) 
    | x1 == x2 && y1 /= y2  = "NO"
    | otherwise = check t
main = do 
    n <- read <$> getLine 
    replicateM n $ do
        k <- read <$> getLine
        putStrLn . (check . sort) =<< replicateM k readInts
