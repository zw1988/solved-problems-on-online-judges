import qualified Data.IntMap.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

data Leftist = Leaf | Node Int Int Leftist Leftist


readInts = map (fst . fromJust. B.readInt) . B.words

singleton k = Node 1 k Leaf Leaf

getRank Leaf = 0
getRank (Node rank _ _ _) = rank


merge Leaf t = t
merge t Leaf = t
merge t1@(Node _ v1 l r) t2@(Node _ v2 _ _)
    | v1 < v2 = merge t2 t1
    | True    = let merged = merge r t2
                    lrank = getRank l
                    rrank = getRank merged
                in 
                    if lrank >= rrank then Node (rrank + 1) v1 l merged else Node (lrank + 1) v1 merged l

insert x t = merge t $ singleton x
delete (Node _ _ l r) = merge l r
getMax Leaf = 0
getMax (Node _ v _ _) = v


getMapValue Nothing = Leaf
getMapValue (Just v) = v

main = do
        B.getContents >>= putStr . unlines . map show . solve M.empty . map readInts . tail. B.lines


solve :: M.IntMap Leftist -> [[Int]] -> [Int]
solve _ [] = []
solve m ([1, i] : t)  = (getMax . getMapValue $ M.lookup i m) : solve m t
solve m ([2, i] : t)  = let new = delete . getMapValue $ M.lookup i m
                            m' = M.insert i new m
                        in solve m' t

solve m ([3, i, c] : t) = let new = merge (singleton c) . getMapValue $ M.lookup i m
                              m' = M.insert i new m
                          in solve m' t
solve m ([4, i, j] : t) = let new = merge  (getMapValue $ M.lookup  i m). getMapValue $ M.lookup j m
                              m' = M.insert i new m
                          in solve m' t
