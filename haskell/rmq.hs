-- https://www.hackerrank.com/challenges/range-minimum-query
import Data.Array
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Monad
import Control.Applicative

data Tree = Leaf Int Int | Tree Int Int Int Tree Tree

readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

minValue (Leaf _ v) = v
minValue (Tree _ _ v _ _) = v

build l r lst | l == r = Leaf l $ head lst
              | otherwise =
                        let m = (l+r) `div` 2
                            (l1, l2) = splitAt ((r-l) `div` 2 + 1) lst
                            ltree = build l m l1
                            rtree = build (m+1) r l2
                        in  Tree l r (min (minValue ltree) (minValue rtree)) ltree rtree

query tree l r = go tree
        where
                go (Leaf id v) = if l<=id && id<=r then v else 10^6
                go (Tree ll rr v lson rson) | l <= ll && rr <= r = v
                                            | l > rr || r < ll = 10^6
                                            | otherwise = min (go lson) (go rson)



main = do
        [n,m]<-readInts
        nums <- readInts
        let tree = build 0 (n-1) nums
        forM_ [1..m] $ \_ ->
                do
                        [l,r]<-readInts
                        putStrLn $ show $ query tree l r

