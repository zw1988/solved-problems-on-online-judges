-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List
main = interact $ solve . map ((\[x,y]->(x,y)) . map read. words) . tail . lines

type Point = (Int, Int)
solve :: [Point] -> String
solve xs = let 
                cmpyx (x1, y1) (x2, y2) = y1 < y2 || (y1 == y2 && x1 < x2) 
                p0 = foldl1 (\p0 p1 -> if cmpyx p1 p0 then p1 else p0) xs 
                det (x0, y0) (x1, y1) (x2, y2) = (x1 - x0) *  (y2 - y0) - (y1 - y0) * (x2 - x0)
                dist (x0, y0) (x1, y1) = (x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1)
                cmp p1 p2  = let res= det p0 p1 p2
                             in if res > 0 || (res == 0 && dist p0 p1 < dist p0 p2) then LT else GT
                     
                            
                xs' = sortBy cmp xs
                (first:second:third:t) = xs'
                go p0 p1 p2 [] = if det p1 p2 p0 >= 0 && det p2 first p1 >= 0 && det  first second p2  >= 0 then "NO" else "YES" 
                go p0 p1 p2 (h:t) 
                    | det p1 p2 p0 < 0 = "YES"
                    | otherwise = go p1 p2 h t  
            in go first second third t
                        
