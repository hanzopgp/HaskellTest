

-- On reprend le type arbre binaire non etiquete du CM #06
--

data Tree a = Tip a | Bin (Tree a) (Tree a)
    deriving Show

data Etape = Gauche | Droite
     deriving Show

type Chemin = [Etape]



-- 0.1 Petit utilitaire de visualisation d un (Tree a)
--
voir :: (Show a) => (Tree a) -> IO()
voir t = putStr (visuTree t)

visuTree :: Show a => Tree a -> String
visuTree t = visu t 1
   where visu (Tip x) n = [' ' | i <- [1..n]] ++ (show x) ++ ['\n']
         visu (Bin t1 t2) n = (visu t1 (n+5)) ++ 
                              [' ' | i <- [1..n]] ++ "o" ++ ['\n'] ++
                              (visu t2 (n+5))
{- 
Exemples d utilisation sur des textes de petite taille
> voir (build (calculFrequence "aaabbcddaad"))
> voir (build (calculFrequence "hello world"))
> voir (build (calculFrequence "abcdefaaaeeff"))
-}



-- 0.2 Un exemple d arbre de plus grande taille (bati sur alphabet)
--
-- arbre de Huffman avec comme feuilles les c_i uniquement
--
ht = (Bin (Bin (Tip 'e') (Bin (Bin (Bin (Bin (Bin (Tip 'x') (Tip 'g')) (Tip 'b')) (Tip 'm')) (Tip 'u')) (Bin (Tip 'o') (Tip 'l')))) (Bin (Bin (Bin (Bin (Bin (Bin (Tip 'f') (Tip 'v')) (Bin (Bin (Tip 'h') (Bin (Bin (Bin (Tip 'z') (Bin (Tip 'k') (Tip 'w'))) (Tip 'j')) (Tip 'y'))) (Tip 'q'))) (Tip 'p')) (Tip 'r')) (Bin (Tip 'a') (Bin (Tip 'c') (Tip 'd')))) (Bin (Bin (Tip 'i') (Tip 'n')) (Bin (Tip 's') (Tip 't')))))

-- liste des frequences
--
fr = [('k',1), ('w',1), ('z',1), ('j',5), ('y',10), ('h',15), ('x',20), ('g',21), ('f',22), ('v',28), ('q',40), ('b',46), ('m',95), ('p',126), ('c',131), ('d',144), ('u',226), ('o',236), ('l',245), ('r',257), ('a',259), ('i',277), ('n',284), ('s',332), ('t',360), ('e',713)]

-- arbre de Huffman avec comme feuilles les (c_i, f_i)
--
htFreq = (Bin (Bin (Tip ('e',713)) (Bin (Bin (Bin (Bin (Bin (Tip ('x',20)) (Tip ('g',21))) (Tip ('b',46))) (Tip ('m',95))) (Tip ('u',226))) (Bin (Tip ('o',236)) (Tip ('l',245))))) (Bin (Bin (Bin (Bin (Bin (Bin (Tip ('f',22)) (Tip ('v',28))) (Bin (Bin (Tip ('h',15)) (Bin (Bin (Bin (Tip ('z',1)) (Bin (Tip ('k',1)) (Tip ('w',1)))) (Tip ('j',5))) (Tip ('y',10)))) (Tip ('q',40)))) (Tip ('p',126))) (Tip ('r',257))) (Bin (Tip ('a',259)) (Bin (Tip ('c',131)) (Tip ('d',144))))) (Bin (Bin (Tip ('i',277)) (Tip ('n',284))) (Bin (Tip ('s',332)) (Tip ('t',360))))))




-- -------------------------------------------------------------
--
-- 1. Le codage
--
-- On suppose l'arbre de Huffman construit (se servir de l'arbre de l'exemple)

-- D�terminer le codage du caractere x selon t

 
codeChar ::  Tree Char ->  Char -> Chemin
codeChar (Tip y) x = []
codeChar (Bin t1 t2) x 
        | appartient t1 x = Gauche:(codeChar t1 x)
        | otherwise       = Droite:(codeChar t2 x)

appartient :: Tree Char -> Char -> Bool
appartient (Tip y) x = x==y
appartient (Bin t1 t2) x = (appartient t1 x) || (appartient t2 x)



-- D�terminer le codage d'une chaine xs selon t
-- on applique codeChar a chaque caractere de la String et on concatene le tout

code :: Tree Char -> String -> Chemin
code (Bin t1 t2) x = concat (map (codeChar (Bin t1 t2)) x)


-- -------------------------------------------------------------
--
-- 2. Le decodage
--
-- il y a deux arbres en parametre : l'initial (qui ne bougera pas)
-- et le courant qui evolue jusqu a donner la feuille recherchee
--


decode :: Tree Char -> Chemin -> String
decode t ps = trace t t ps

trace :: Tree Char -> Tree Char -> Chemin -> String
trace t (Tip x) [] = ""
trace t (Tip x) (p:ps) = x:(trace t t (p:ps))
trace t (Bin t1 t2) (Gauche:ps) = trace t t1 (ps)
trace t (Bin t1 t2) (Droite:ps) = trace t t2 (ps)


-- ------------------------------------------------------------
--
-- 3. Experimentations
-- 

-- 3.1
-- 

type Frequence = (Char,Integer)
type Frequences = [Frequence]

ajouteFrequence :: Char -> Frequences -> Frequences
ajouteFrequence c [] = [(c,1)]
ajouteFrequence c ((x,n):fs)
	| x == c = (x,n+1):(ajouteFrequence c fs)
	| otherwise = (x,n):(ajouteFrequence c fs)

qsort :: Frequences -> Frequences 
qsort [] = []
qsort ((x,n):fs) = (qsort [(c,m) | (c,m)<-fs , m<n]) ++ [(x,n)] ++ (qsort [(c,m) | (c,m)<-fs , m>n]) 	


calculFrequence :: String -> Frequences
calculFrequence l = qsort (calculFrequence2 l [])

calculFrequence2 :: String -> Frequences -> Frequences
calculFrequence2 "" fs = fs
calculFrequence2 (c:cs) fs = calculFrequence2 cs (ajouteFrequence c fs)

-- 3.2
--
{-
etapeToBool :: Etape -> Char
etapeToBool etp
	| etp == Gauche = '0'
	| otherwise = '1'

codeText :: String -> String
codeText "" = ""
codeText str = map etapeToBool (code ht str) 
-}

-- pour verifier que decoder le codage d�une chaine retourne bien cette chaine
tester :: String -> Bool
tester l = decode t (code t l) == l
       where t = build (calculFrequence l)

-- 3.3
--
-- On suppose que la taille de l'arbre de Huffman est n�gligeable vs la taille du texte a compresser
--

--tauxDeCompression :: String -> Float



-- ------------------------------------------------------------
--
-- 4. Construction de l'arbre
-- 


-- Dans un arbre de Frequence, les feuilles ne sont plus des caracteres
-- Mais des couples (caractere, frequence d'apparition de ce caractere)
--


-- Le poids d'un arbre a partir de celui de ses feuilles
--
poids :: Tree Frequence -> Integer
poids (Tip (_, p)) = p
poids (Bin t1 t2) = poids t1 + poids t2


-- ins�rer un arbre de Frequence dans une liste d'arbres de Frequence
-- la liste etant ordonnee selon les poids des arbres de Frequence
--
insert :: Tree Frequence -> [Tree Frequence] -> [Tree Frequence]
insert u [] = [u]
insert u (t:ts) 
       | poids u <= poids t = u:t:ts
       | otherwise          = t:(insert u ts)


-- Construire l'arbre des (c_i, p_i) a partir de la liste des (c_i, p_i)
-- on procede par insertions successives des sous-arbres jusqu'a ce qu'il n'en reste plus qu'un seul
--
build :: Frequences ->  Tree Char
build = unlabel . head . until single combine . (map (\x->Tip x))


combine :: [Tree Frequence] -> [Tree Frequence]
combine (t1:t2:ts) = insert (Bin t1 t2) ts

unlabel ::  Tree Frequence ->  Tree Char
unlabel (Tip (x, n)) = Tip x
unlabel (Bin t1 t2) = Bin (unlabel t1) (unlabel t2)

single :: [a] -> Bool
single [_] = True
single _   = False

