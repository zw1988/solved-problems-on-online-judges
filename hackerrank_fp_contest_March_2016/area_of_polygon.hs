j- Enter your code here. Read input from STDIN. Print output to STDOUT
main = interact $ show . (\x -> abs $ fromIntegral x / 2) . solve . map ((\[x, y] -> (x, y)). map read . words) . tail . lines


type Point = (Int, Int)
solve :: [Point] -> Int
solve (p0:p1:xs) = go p0 p1 xs
    where go p1 p2 (p3:t) = mul p1 p2 + go p2 p3 t
          go p1 p2 [] = mul p1 p2 + mul p2 p0
          mul (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
