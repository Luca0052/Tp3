{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use and" #-}
import Foreign (lengthArray0)
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
hd :: [a] -> a --retorna el primer elemento de una lista.
hd (x:_)= x

tl :: [a] -> [a] --retorna toda la lista menos el primer elemento.
tl [x] = []
tl (_:xs) = xs

last2:: [a] -> a --retorna el  ́ultimo elemento de la lista.
last2 [x]= x
last2 (x:xs) = last2 xs

init2:: [a] -> [a] --retorna toda la lista menos el  ́ultimo elemento.
init2 [x] = []
init2 (x:xs) =  x : init2 xs

listRev :: [a]->[a]
listRev xs = reverse xs

listaiguales :: Eq a =>[a]->[a]->Bool
listaiguales [] []= True
listaiguales list1 list2 = list1==list2

palindromo :: Eq a => [a]-> Bool
palindromo [] = True
palindromo xs = xs==reverse (xs)

listPrimo :: Int->[Int]
listPrimo n = [x|x<-[1..(n-1)],primo (x)]

divisores :: Int -> [Int]
divisores n =[x| x<-[1..n], mod n x ==0 ]

primo :: Int->Bool
primo n = divisores n == [1,n]

listOnlyPrimo :: [Int]->[Int]
listOnlyPrimo xs = filter (primo) xs

listDig :: Int->[Int]
listDig n   |n<0 = listDig (abs n)
            |n<10 = [n]
            |otherwise =   listDig (div n 10) ++ [mod n 10]


listUno :: [Int]
listUno = [1, 1..]


listNat :: Int -> [Int]
listNat n = [n..]

listPrimerosN :: Int ->[Int]
listPrimerosN n = [0..n-1]

listPrimerFive :: [Int]
listPrimerFive = [1..] 

retorfivelement :: [Int]
retorfivelement = take 5 listPrimerFive

listEnteros :: [Int]->[Int]
listEnteros xs = map (^2) xs

enterosPositivos :: Int->[Int]
enterosPositivos n = filter(\x->mod n x ==0) [1..n]

listPrimos :: [Int]->[Int]
listPrimos xs = filter(primo) xs

sumaDeCuadrados :: [Int]->Int
sumaDeCuadrados xs = foldl (+) 0(map (^2) xs)

listSucc :: [Int]->[Int]
listSucc xs = map (+1) xs

sumaElement :: [Int]->Int
sumaElement xs = foldl (+) 0 xs

factoriall :: Int->Int
factoriall n = foldl (*) 1 [1..n]

andd :: [Bool]->Bool
andd xs = foldr (&&) True xs
--
tam :: [a]->Int
tam xs = foldl(\acc _ -> acc+1) 0 xs
--
tamv2 :: [a]-> Int
tamv2 xs = foldr (\_ acc-> 1+acc)0 xs
--[expresion |generador,Condicion]
listEnterosv2 :: [Int]->[Int]
listEnterosv2 xs = [x+1|x<-xs]
--
listCuad :: [Int]->[Int]
listCuad xs = [n^2|n<-xs]
--
listEnterosv3 :: [Int]->[Int]
listEnterosv3 xs = [n|n<-xs, even n && n>10]
--
listDiv :: Int->[Int]
listDiv n = [x|x<-[1..n], mod n x ==0 ]
--
todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn  xs ys = null [n|n<-xs, not(n `elem`ys)]
--
listNatP :: Int->[Int]
listNatP n = [x|x<-[2..n], primo(x)]
--
listInf :: [Int]
listInf  = [x|x<-[0..], mod x 2==0]
--
listInff :: [Int]
listInff = [0,2..]
--
listCart :: [Int]->[Int]->[(Int,Int)]
listCart xs ys = [(x,y)|x<-xs,y<-ys]
--
listNatv2 :: [Int]
listNatv2  = [n|n<-[1..5000], mod n 1 ==0 ,mod n 2 ==1,mod n 3 ==2,mod n 4 ==3,mod n 5 ==4,mod n 6 ==5,mod n 7 ==6,mod n 8 ==7,mod n 9 ==8,mod n 10 ==9] 
--
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias n xs = length [y|y<-xs, y==n]
--
--34 y 35 no las entiendo