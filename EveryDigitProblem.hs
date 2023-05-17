module Main where




countDigits :: String -> Char -> Int
countDigits x d = sum(map (\x -> if x == d then 1 else 0 ) x)

checkDigits :: Int->Int -> Int ->Bool
checkDigits x p 9 = True
checkDigits x p i = do
                  let digits = ['0'..'9']
                  if countDigits (show x) (digits!!i) > p then False else (checkDigits x p (i+1))

checkNum :: Int ->Int -> Bool
checkNum 0 m = True
checkNum x m= do
           let y = (x`div`10) * 10
           let d = x - y
           if(d>m) then False else (checkNum (x`div`10) m)




result :: Int->Int->Int->Int->Int
result 0 m p r = r-1
result n m p r = do
                 if (not (checkDigits r p 0) || not (checkNum r m)) then result n m p (r+1) else result (n-1) m p (r+1)


main :: IO()


main= do print("Podaj n m p")
         n <- readLn
         m <- readLn
         p <- readLn
         let i = 0
         print (result n m p i)


