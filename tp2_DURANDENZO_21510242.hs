type Facteur = Int
type Exposant = Int
type Couple = (Facteur, Exposant)
type Decomposition = [Couple]

rep2int :: Decomposition -> Int
rep2int [] = 0
rep2int ((x,y):xs) = (x^y)*(rep2int xs)

estPremier :: Decomposition -> Boolean
estPremier [(1,_)] = True
estPremier (_:_)   = False

pgcd :: Decomposition -> Decomposition
pgcd [] _ = []
pgcd _ [] = []
pgcd ((k1,d1):p1) ((k2,d2):p2) 
   | k1 == k2 = (k1, min d1 d2) : (pgcd p1 p2)
   | k1 < k2 = pgcd p1 ((k2,d2):p2)
   | k2 < k1  = pgcd ((k1,d1):p1) p2

pgcm :: Decomposition -> Decomposition
pgcm [] _ = []
pgcm _ [] = []
pgcm ((k1,d1):p1) ((k2,d2):p2) 
   | k1 == k2 = (k1, max d1 d2) : (pgcm p1 p2)
   | k1 < k2 = pgcm p1 ((k2,d2):p2)
   | k2 < k1  = pgcm ((k1,d1):p1) p2

nbDiviseurs :: Decomposition -> Int
nbDiviseurs xs = product [ (n+1) | (x, n) <- xs]

diviseurs :: Decomposition -> [Int]
diviseurs [] = [1]
diviseurs (x:xs) = op x (diviseurs xs)

primes :: [Int]
primes =  sieve [2 .. ]
	where sieve (p:xs) = p : (sieve [x | x <- xs, mod x p > 0])

--take n primes --> la liste des n premiers nombres premiers
--takeWhile (<= n) primes --> [x] -- > liste des nombres premiers inferieurs a n
--length (takeWhile (<= n) primes) --> x --> nombre de nombres premiers inferieurs a n


pfactors :: Int -> [Int]
pfactors n = pfactors’ n primes
	where pfactors’ x (p:ps)
		| p > x          = []
		| mod x p == 0   = p : (pfactors' (div x p) (p:ps))
		| otherwise      = pfactors' x ps

prep :: [Int] -> Decomposition
prep [] = []
prep (x:xs) = (x, (length (takeWhile (==x) (x:xs)))) : (prep (dropWhile (==x) xs))

int2rep :: Int -> Decomposition
int2rep = prep . pfactors
