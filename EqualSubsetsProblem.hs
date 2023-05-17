module Main where

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

checkLength :: [[Int]] -> Int -> [[Int]] -> [[Int]]
checkLength (x:xs) num tab = do
    if null xs
       then if length x == num
            then do
                x:tab
            else
                tab
    else
        if length x == num
            then do
                let newtab = x:tab
                checkLength xs num newtab
        else
            checkLength xs num tab

compareTwo :: [Int] -> Int -> Int -> Bool
compareTwo tab nchange change = do
    let a = tab!!change
    if nchange == a
        then True
    else do
        if change+1 == length tab
            then False
        else
            compareTwo tab nchange (change+1)

compareSum :: [[Int]] -> Int -> Bool
compareSum tab num = do
  let s = map sum tab
  if num == (-1)
      then do
          compareSum tab (num+1)
  else
      if num+2 == length tab
          then s!!num == s!!(num+1)
      else
        if compareTwo s (s!!num) (num+1) == True
            then True
        else
            compareSum tab (num+1)


checkIfSubsExist :: [Int] -> Int -> Bool
checkIfSubsExist tab num = do
    let setofsubs = subsets tab
    if num == 0
        then do
            let nowy_num = 2
            let dummy_tab = checkLength setofsubs nowy_num []
            if compareSum dummy_tab (-1) == True
                then True
            else checkIfSubsExist tab (nowy_num+1)
    else do
        if num <= ((length tab) `div` 2)
            then do
                let dummy_tab = checkLength setofsubs num []
                if compareSum dummy_tab (-1) == True
                    then True
                else checkIfSubsExist tab (num+1)
        else False


main :: IO()
main = do print("Podaj liste")
          a<-readLn
          print(checkIfSubsExist a 0)