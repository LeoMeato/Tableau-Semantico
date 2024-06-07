data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

tableau :: String -> Tree
tableau entrada = montaTableau (Leaf ["(~, "++entrada++")"])

testaTableau :: String -> Bool
testaTableau entrada = tautologia [] (tableau entrada)

------------------------------------------ Avaliação --------------------------------------------------
-- Ainda não funcionando 100%

tautologia :: [String] -> Tree -> Bool
tautologia listaAcumulada (Node lista fst snd) | avalia (listaAcumulada++lista) lista == True = True
                                               | (tautologia (listaAcumulada++lista) fst) && (tautologia (listaAcumulada++lista) snd) = True
                                               | otherwise = False
tautologia listaAcumulada (Leaf lista) | avalia (listaAcumulada++lista) lista == True = True
                                       | otherwise = False

avalia :: [String] -> [String] -> Bool
avalia acumulada (h:t) = (isIn h acumulada) || (avalia acumulada t)
avalia acumulada [] = False

isIn :: String -> [String] -> Bool
isIn ('(':'~':',':' ':'(':formula) lista = elem ('(':(achaFimSimples formula "" (-1))) lista
isIn formula lista = elem ("(~, "++formula++")") lista

-------------------------------------------------------------------------------------------------------

------------------------------------------------------- Montagem da Árvore -------------------------------------------------------------------

montaTableau :: Tree -> Tree
montaTableau (Node (h:t) fst snd) = junta h (aplicaRegra (detectaPadrao h) (Node t fst snd))
montaTableau (Node [] fst snd) = (Node [] (montaTableau fst) (montaTableau snd))
montaTableau (Leaf []) = (Leaf [])
montaTableau (Leaf (h:t)) = junta h (aplicaRegra (detectaPadrao h) (Leaf t))

regraTransitiva :: (Int, [String]) -> Tree -> Tree

regraTransitiva (regra, [formula1, formula2]) (Node t fst snd) = (Node t (regraTransitiva (regra, [formula1, formula2]) fst) (regraTransitiva (regra, [formula1, formula2]) snd))
regraTransitiva (regra, [formula]) (Node t fst snd) = (Node t (regraTransitiva (regra, [formula]) fst) (regraTransitiva (regra, [formula]) snd))

regraTransitiva (1, [formula1, formula2]) (Leaf t) = (Leaf (t++[formula1]++[formula2]))
regraTransitiva (2, [formula1, formula2]) (Leaf t) = (Node t (Leaf [formula1]) (Leaf [formula2]))
regraTransitiva (3, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["(~, "++formula1++")"]) (Leaf [formula2]))
regraTransitiva (4, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["(^, "++formula1++", "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), (~, "++formula2++"))"]))
regraTransitiva (5, [formula]) (Leaf t) = (Leaf (t++[formula]))
regraTransitiva (6, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["(~, "++formula1++")"]) (Leaf ["(~, "++formula2++")"]))
regraTransitiva (7, [formula1, formula2]) (Leaf t) = (Leaf (t++["(~, "++formula1++")"]++["(~, "++formula2++")"]))
regraTransitiva (8, [formula1, formula2]) (Leaf t) = (Leaf (t++[formula1]++["(~, "++formula2++")"]))
regraTransitiva (9, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]) (Leaf ["(^, "++formula1++", (~, "++formula2++"))"]))
regraTransitiva (10, [formula]) (Leaf t) = (Leaf t)
regraTransitiva (11, [formula]) (Leaf t) = (Leaf t)

aplicaRegra :: (Int, [String]) -> Tree -> Tree

aplicaRegra (regra, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (regraTransitiva (regra, [formula1, formula2]) fst) (regraTransitiva (regra, [formula1, formula2]) snd))
aplicaRegra (regra, [formula]) (Node t fst snd) = montaTableau (Node t (regraTransitiva (regra, [formula]) fst) (regraTransitiva (regra, [formula]) snd))

aplicaRegra (1, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++[formula1]++[formula2]))
aplicaRegra (2, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf [formula1]) (Leaf [formula2]))
aplicaRegra (3, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(~, "++formula1++")"]) (Leaf [formula2]))
aplicaRegra (4, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(^, "++formula1++", "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), (~, "++formula2++"))"]))
aplicaRegra (5, [formula]) (Leaf t) = montaTableau (Leaf (t++[formula]))
aplicaRegra (6, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(~, "++formula1++")"]) (Leaf ["(~, "++formula2++")"]))
aplicaRegra (7, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++["(~, "++formula1++")"]++["(~, "++formula2++")"]))
aplicaRegra (8, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++[formula1]++["(~, "++formula2++")"]))
aplicaRegra (9, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]) (Leaf ["(^, "++formula1++", (~, "++formula2++"))"]))
aplicaRegra (10, [formula]) (Leaf t) = montaTableau (Leaf t)
aplicaRegra (11, [formula]) (Leaf t) = montaTableau (Leaf t)

junta :: String -> Tree -> Tree
junta h (Node t fst snd) = (Node (h:t) fst snd)
junta h (Leaf t) = (Leaf (h:t))

-----------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------- Detecção de Padrão -----------------------------------------------

detectaPadrao :: String -> (Int, [String])
detectaPadrao ('(':'^':',':' ':resto) = (1, (auxDetecta resto))
detectaPadrao ('(':'v':',':' ':resto) = (2, (auxDetecta resto))
detectaPadrao ('(':'-':'>':',':' ':resto) = (3, (auxDetecta resto))
detectaPadrao ('(':'<':'-':'>':',':' ':resto) = (4, (auxDetecta resto))
detectaPadrao ('(':'~':',':' ':'(':'~':',':' ':'(':resto) = (5, [achaFimSimples resto "" (-1)])
detectaPadrao ('(':'~':',':' ':'(':'^':',':' ':resto) = (6, (auxDetecta resto))
detectaPadrao ('(':'~':',':' ':'(':'v':',':' ':resto) = (7, (auxDetecta resto))
detectaPadrao ('(':'~':',':' ':'(':'-':'>':',':' ':resto) = (8, (auxDetecta resto))
detectaPadrao ('(':'~':',':' ':'(':'<':'-':'>':',':' ':resto) = (9, (auxDetecta resto))

{-detectaPadrao ('(':'~':',':' ':'(':resto) = (10, ['~':(tiraParenteses resto)])
detectaPadrao ('(':resto) = (11, [tiraParenteses resto])
detectaPadrao _ = (12, ["Deu ruim aí, irmão"])-}

detectaPadrao ('(':'~':',':' ':'(':resto) = (10, [('(':'~':',':' ':'(':resto)])
detectaPadrao ('(':resto) = (11, [('(':resto)])
detectaPadrao _ = (12, ["Deu ruim aí, irmão"])

auxDetecta :: String -> [String]
auxDetecta ('(':resto) = achaFimDuplo resto "" (-1)

auxDetecta2 :: String -> String -> Int -> String
auxDetecta2 (',':' ':'(':resto) x count = achaFimSimples resto x count

achaFimDuplo :: String -> String -> Int -> [String]
achaFimDuplo (h:t) x count | count == 0 = ['(':x, (auxDetecta2 (h:t) "" (-1))]
                    | count < 0 = achaFimDuplo t (x++[h]) (count + (decresce h))
                    | otherwise = ["Tem coisa errada aí"]

achaFimSimples :: String -> String -> Int -> String
achaFimSimples (h:t) x count | count == 0 = '(':x
                             | count < 0 = achaFimSimples t (x++[h]) (count + (decresce h))
                             | otherwise = "Tem coisa errada aí"

decresce :: Char-> Int

decresce '(' = -1
decresce ')' = 1
decresce _ = 0

----------------------------------------------------------------------------------------------------------



teste :: Int -> Int
teste x | (elem x [1..4]) || (elem x [6..9]) = 1
        | otherwise = 2
