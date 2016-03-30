-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Text.Printf (printf)
main = interact $ show. solve. (\x -> last x : x) . map (map read. words). tail. lines

solve :: [[Int]] -> Float
solve [[x,y]] = 0.0
solve ([x1,y1]:t@([x2,y2]:_)) = (sqrt $ fromIntegral (((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)))) + solve t
