data Formule = Var String
   | Non Formule
   | Imp Formule Formule
   | Et Formule Formule
   | Ou Formule Formule
   | Equi Formule Formule
       deriving (Eq, Show)

f1 = (Ou (Et (Var "c") (Var "d")) (Et (Var "a") (Var "b")))
f2 = (Imp (Var "d") (Ou (Var "c") (Non (Var "b"))))
f3 = (Equi  (Var "d") (Et (Var "c") (Var "d")))
f4 = (Imp (Non (Var "d")) (Ou (Var "c") (Var "b")))
f5 = (Non (Non (Var "a")))
f6 = (Imp (Non (Ou (Var "a") (Var "d"))) (Ou (Var "c") (Var "b")))

{- PARTIE 1 -}

{- Q1 -} 
visuFormule :: Formule -> String
visuFormule (Var p)   = p
visuFormule (Non f)   = "~" ++ visuFormule f
visuFormule (Et g d)  = "(" ++ (visuFormule g) ++ " & " ++ (visuFormule d) ++ ")"
visuFormule (Ou g d)  = "(" ++ (visuFormule g) ++ " v " ++ (visuFormule d) ++ ")"
visuFormule (Imp g d) = "(" ++ (visuFormule g) ++ " => " ++ (visuFormule d) ++ ")"
visuFormule (Equi g d) = "(" ++ (visuFormule g) ++ " <=> " ++ (visuFormule d) ++ ")"

{- Q2 -}
{-
- (Imp g d) et (Ou (Non g) d) sont egaux, En effet on a :
  1) (vrai => vrai = vrai) et (~ vrai v vrai = vrai)
  2) (faux => faux = vrai) et (~ faux v faux = vrai)
  3) (faux => vrai = vrai) et (~ faux v vrai = vrai)
  4) (vrai => faux = faux) et (~ vrai v faux = faux)
  On a donc : (Imp g d) <=> (Ou (Non g) d).

- (Equi g d) <=> (Imp g d) & (Imp d g)
  (Imp g d) <=> (Ou (Non g) d)
  (Imp d g) <=> (Ou (Non d) g)
  On a donc : (Equi g d) <=> ((Ou (Non g) d) & (Ou (Non d) g))
-}

{- Q3 -}
elimine :: Formule -> Formule
elimine (Var p) = (Var p)
elimine (Non f) = (Non f)
elimine (Et g d) = (Et g d)
elimine (Ou g d) = (Ou g d)
elimine (Imp g d) = (Ou (Non g) d)
elimine (Equi g d) = (Et (Ou (Non g) d) (Ou (Non d) g))

{- Q4 -}
{- 
- (~ (~ f)) <=> f
-}

{- Q5 -}
{-
~ (g v d) <=> (~ g & ~ d)
~ (g & d) <=> (~ g v ~ d)
-}

f2b = (Imp (Non (Var "d")) (Ou (Var "c") (Var "b")))

{- Q6 -}
{-
- la fonction disNon supprime les doubles négations et applique les 2 lois De Morgan
-}

ameneNon, disNon :: Formule -> Formule

ameneNon (Var p)   = (Var p)
ameneNon (Non f)   = disNon f
ameneNon (Et g d)  = (Et (ameneNon g) (ameneNon d))
ameneNon (Ou g d)  = (Ou (ameneNon g) (ameneNon d))

disNon (Var p)   = (Var p)
disNon (Non f)   = f
disNon (Et g d)  = (Ou (Non g) (Non d))
disNon (Ou g d)  = (Et (Non g) (Non d))

{- Q7 -}
normalise :: Formule -> Formule
normalise (Et g d) = concEt (normalise g) (normalise d)
normalise (Ou g d) = developper (normalise g) (normalise d)
normalise f        = f

concEt :: Formule -> Formule -> Formule
concEt (Et g d) f = (Et g (concEt d f))
concEt g f        = (Et g f)

developper :: Formule -> Formule -> Formule
developper (Var p) f  = (Et (Ou (Var p) (recupererA f)) (Ou (Var p) (recupererB f)))
developper (Et g d) f = (concEt (developper g f) (developper d f))

recupererA :: Formule -> Formule
recupererA (Var p)  = (Var p)
recupererA (Et g d) = (recupererA g)

recupererB :: Formule -> Formule
recupererB (Var p)  = (Var p)
recupererB (Et g d) = (recupererB d)

{- Q8 -}
formeClausale :: Formule -> Formule
formeClausale f = (normalise (ameneNon (elimine f)))

{- PARTIE 2 -}

type Clause = [Formule]
type FormuleBis = [Clause]

{- Q9 -}
etToListe :: Formule -> FormuleBis
etToListe (Et g d) = (ouToListe g) : etToListe d
etToListe f        = [ouToListe f]

ouToListe :: Formule -> Clause
ouToListe (Ou g d) = g : ouToListe d
ouToListe  f       = [f]

{- pour meilleure lisibilite -}
formeClausaleBis :: Formule -> FormuleBis
formeClausaleBis = etToListe

{- Q10 -}
neg :: Formule -> Formule
neg (Var p)       = (Non (Var p))
neg (Non (Var p)) = (Var p)   

{- Q11 -}
sontLiees :: Clause -> Clause -> Bool
sontLiees [] _          = False
sontLiees (x:xs) ys = (neg x) `elem` ys || (sontLiees xs ys)

{- Q12 -}
resolvante :: Clause -> Clause -> Clause
resolvante [] _   = []
resolvante (x:xs) ys
     | sontLiees (x:xs) ys == False = []
     | (neg x) `elem` ys            = xs ++ supprimer (neg x) ys
     | x `elem` ys                  = [] ++ resolvante xs ys
     | otherwise                    = [x] ++ (resolvante xs ys)
          where supprimer a [] = []
                supprimer a (y:ys)
                    | a == y    = [] ++ supprimer a ys
                    | otherwise = [y] ++ supprimer a ys

{- FIN PARTIE 2 -}

deduire :: Formule -> Clause
deduire x = resoudre (head sorite) (tail sorite)
   where sorite = (formeClausaleBis (formeClausale x))

resoudre :: Clause -> FormuleBis -> Clause
resoudre xs [] = xs
resoudre xs (ys:yss)
   | sontLiees xs ys  = resoudre (resolvante xs ys) yss
   | otherwise        = resoudre xs (yss ++ [ys])

{- exemple 1 -}
exemple1 = (Et (Imp (Var "A") (Var "B"))
             (Et (Imp (Var "B") (Var "C"))
               (Et (Imp (Var "C") (Non (Var "D")))
                  (Et (Imp (Non (Var "D")) (Non (Var "E")))
                      (Imp (Non (Var "E")) (Var "F"))))))

{- exemple 2 -}
exemple2 = (Et (Imp (Non (Var "A")) (Var "B"))
             (Et (Imp (Var "C") (Non (Var "D")))
               (Et (Imp (Var "E") (Non (Var "F")))
                 (Et (Imp (Non (Var "D")) (Non (Var "B")))
                     (Imp (Var "A") (Var "F"))))))

{- exemple 2 bis -}
a = "etre un exercice qui me fait ronchonner"
b = "etre un exercice que je comprends"
c = "etre parmi ces sorites"
d = "etre dispose regulierement, comme les exercices auxquels je suis habitue"
e = "etre un exercice facile"
f = "etre un exercice qui me donne mal a la tete"

testBis = (Et (Imp (Non (Var a)) (Var b))
           (Et (Imp (Var c) (Non (Var d)))
               (Et (Imp (Var e) (Non (Var f)))
                   (Et (Imp (Non (Var d)) (Non (Var b)))
                       (Imp (Var a) (Var f))))))

{- exemple 3 -}
bebe = Et (Imp (Var "bebe") (Non (Var "logique")))
          (Et (Imp (Var "tuer crocodile") (Non (Var "meprise")))
              (Imp (Non (Var "logique")) (Var "meprise")))

