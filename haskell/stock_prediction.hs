-- https://www.hackerrank.com/challenges/stocks-prediction
import Data.Array
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Monad
import Control.Applicative

--leaf index value   | tree  leftIndex rightIndex minValue maxValue leftson rightson 
data Tree = Leaf Int Int | Tree Int Int Int Int Tree Tree

readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

minValue (Leaf _ v) = v
minValue (Tree _ _ v _ _ _) = v

maxValue (Leaf _ v) = v
maxValue (Tree _ _ _ v _ _) = v

build l r lst | l == r = Leaf l $ head lst
              | otherwise =
                        let m = (l+r) `div` 2
                            (l1, l2) = splitAt ((r-l) `div` 2 + 1) lst
                            ltree = build l m l1
                            rtree = build (m+1) r l2
                            minv = (min (minValue ltree) (minValue rtree))
                            maxv = (max (maxValue ltree) (maxValue rtree))
                        in  Tree l r minv maxv ltree rtree

query tree l r = go tree
        where
                go (Leaf id v) = if l<=id && id<=r then (v,v) else (10^9,0)
                go (Tree ll rr minv maxv lson rson) | l <= ll && rr <= r = (minv, maxv)
                                            | l > rr || r < ll = (10^9,0)
                                            | otherwise = let (leftmin, leftmax) = go lson
                                                              (rightmin, rightmax) = go rson
                                                          in (min leftmin rightmin, max leftmax rightmax)




find tree k minv maxv n =
        let   goRight l r | l>r = r
                          | check $ query tree k m  = goRight (m+1) r
                          | otherwise = goRight l (m-1)
                          where m = (l+r) `div` 2
              goLeft l r | l>r = l
                         | check $ query tree m k = goLeft l (m-1)
                         | otherwise = goLeft (m+1) r
                         where m = (l+r) `div` 2
              check (a, b) = minv<=a && b<=maxv
        in goRight k (n-1)  - goLeft 0 k + 1

main = do
        [n] <- readInts
        nums <- readInts
        let tree = build 0 (n-1) nums
        let ary = listArray (0, n-1) nums
        [m] <- readInts
        forM_ [1..m] $ \_ ->
                do
                        [k,d]<-readInts
                        let minv = ary Data.Array.! k
                        print $ find tree k  minv (minv + d) n

