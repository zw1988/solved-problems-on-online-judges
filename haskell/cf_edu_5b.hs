--http://codeforces.com/problemset/problem/616/B
main = interact $ show. maximum . map (minimum . map (read ::String->Int). words) . tail . lines
