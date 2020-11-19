square :: Int -> Int
square x = x * x

sumSquare :: Int -> Int -> Int -> Int
sumSquare x y z = square(x) + square(y) + square(z)

sumSquareMax :: Int -> Int -> Int -> Int
sumSquareMax x y z = sumSquare x y z - square(min (min x y) z)

f2c :: Float -> Float
f2c x = (x - 32) * 5/9 

reductionRate :: Float -> Float
reductionRate nbPers
	| nbPers < 2  = 0
	| nbPers < 5 = 0.25
	| nbPers < 11 = 0.50
	| otherwise = 0.75 

tarif :: Float -> Float
tarif x = 0.52 * x

travelExpenses :: Float -> Float -> Float
travelExpenses nbKm nbPers = tarif(nbKm) * reductionRate(nbPers) * nbPers

decode :: Int -> Char
decode n = toEnum n

code :: Char -> Int
code c = fromEnum c

nextUpperCase :: Char -> Char
nextUpperCase c
	| code(c) == 122 = 'a'
	| otherwise = decode(code(c)+1)

collatz :: Int -> Int
collatz n
	| n == 2 = 1
	| even n = div n 2
	| otherwise = 3 * n + 1

nbCalls :: Int -> Int
nbCalls n = if(n == 1) then 0 else (1 + nbCalls(collatz n)) 
{-
syracuse :: Int -> [Int]
syracuse n
	| nbCalls(collatz n) == 0 = (collatz n):[]
	| otherwise = syracuse n
-}
allEven :: [Int] -> Bool
allEven [] = True
allEven xs 
	| odd(head xs) = False	
	| otherwise = allEven(tail xs)

laugh :: Int -> String
laugh n = if(n == 0) then "" else ("HA " ++ laugh(n-1))	

double :: [String] -> String
double str = if(head str == []) then "Finit" else (head str ++ " " ++ head str ++ " " ++ double(tail str))

sommeListe :: [Int] -> Int
sommeListe l = if (l == []) then 0 else (head l)*(head l) + (sommeListe (tail l))

sumSquareEven :: Int -> Int
sumSquareEven n = sommeListe([x | x <- [2..n*2], even x])

mystery :: [Int] -> [Int] -- la fonction transforme le tableau d'entrée en tableau croissant
mystery [] = []
mystery (x:xs) = mystery [y | y <- xs, y <= x] ++ [x] ++ mystery [y | y <- xs, y > x]

