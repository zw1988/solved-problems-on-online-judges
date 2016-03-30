import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

data Tree = Tree {
             getVal :: Int,
             getSons :: [Tree]
          }deriving (Show, Eq)

data Path = Path {
                getPathVal :: Int,
                getLeftTree :: [Tree],
                getRightTree :: [Tree]
          }deriving (Show, Eq)

main = interact $ unlines . map show .solve. map words . tail . lines

solve  =  go (Tree 0 [])  [] 
     where    
        go now paths [] = []
        go now paths (["print"] : xs)  = getVal now : go now paths xs
        go now paths (["change", v] : xs) = go (now { getVal=read v }) paths xs
        go now (p:pt) (["visit", "parent"] : xs)  = 
                            let val = getPathVal p
                                sons = getLeftTree p ++ now : getRightTree p
                            in  go (Tree val sons)  pt xs

        go now (Path val left right:pt) (["visit", "left"] : xs) =
                            let 
                                (left', last) = splitAt (length left - 1) left
                                right' = now : right
                                p' = Path val left' right'
                            in go (head last) (p':pt) xs
        go now (Path val left right:pt) (["visit", "right"] : xs) =
                          let left' = left  ++ [now]
                              right' = tail right
                              now' = head right
                              p' = Path val left' right'
                          in go now' (p':pt) xs
                                        
                                              
        go now paths (["visit", _, n] : xs) = 
                                     let (left, right') = splitAt (read n - 1) $ getSons now
                                         now' = head right'
                                         right = tail right'
                                         p = Path (getVal now) left right
                                     in  go now' (p:paths) xs

        go now paths (["insert", "child", n] : xs) = 
                                     let newSon = Tree (read n) []
                                         now' = now {getSons = newSon:getSons now}
                                     in  go now' paths xs
        go now (Path val left right:pt) (["insert", d, n] : xs) = 
                                     let newSon = Tree (read n) []
                                         p' = if d=="left" then Path val (left ++ [newSon]) right else Path val left (newSon:right)
                                     in go now (p':pt) xs
        
        go now (Path val left right : pt) (["delete"] : xs) = go (Tree val (left ++ right)) pt xs
