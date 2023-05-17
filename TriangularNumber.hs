module Main where

suma :: Int -> Int
suma 0 = 0
suma n = n + suma (n-1)

dzielniki :: Int -> [Int]
dzielniki n = [i | i<-[1..n], mod n i == 0]
            

najmniejsza :: [Int] -> Int -> Int -> Int
najmniejsza lista n x = if length(dzielniki(lista!!n)) > x then lista !! n else (najmniejsza lista (n+1) x)



main :: IO()
main = do print("Podaj liczbe")
          n <- readLn 
          let liczbyTrojkatne = [suma(x) | x <- [1,2..]]
          let i = 0
          print(najmniejsza liczbyTrojkatne i n)