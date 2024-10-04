hd :: [a] -> a --retorna el primer elemento de una lista.
hd (x:_)= x

tl :: [a] -> [a] --retorna toda la lista menos el primer elemento.
tl [x] = []
tl (_:xs) = xs 

last2:: [a] -> a --retorna el  ́ultimo elemento de la lista.
last2 [x]= x
last2 (x:xs) = last2(xs) 

init2:: [a] -> [a] --retorna toda la lista menos el  ́ultimo elemento.
init2 [x] = []
init2 (x:xs) =  x : init2 xs

listRev :: [a]->[a]
listRev xs = reverse(xs)

listaiguales :: Eq a =>[a]->[a]->Bool
listaiguales [] []= True
listaiguales list1 list2 = list1==list2

palindromo :: Eq a => [a]-> Bool
palindromo [] = True
palindromo xs = xs==reverse(xs)

listPrimo :: Int->[Int]
listPrimo n = [x|x<-[1..(n-1)],primo(x)]

divisores :: Int -> [Int]
divisores n =[x| x<-[1..n], mod n x ==0 ] 

primo :: Int->Bool
primo n = divisores n == [1,n]

listOnlyPrimo :: [Int]->[Int]
listOnlyPrimo xs = filter (primo) xs

listDig :: Int->[Int]
listDig n   |n<0 = listDig(abs n)
            |n<10 = [n]
            |otherwise =   listDig(div n 10) ++ [mod n 10]


listUno :: [Int]
listUno = [1, 1..]
