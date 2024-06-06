data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

tableau :: String -> Tree
tableau entrada = montaTableau (Leaf ["(~, "++entrada++")"])

------------------------------------------ Avaliação --------------------------------------------------



-------------------------------------------------------------------------------------------------------

------------------------------------------------------- Montagem da Árvore -------------------------------------------------------------------

montaTableau :: Tree -> Tree
montaTableau (Node (h:t) fst snd) = junta h (aplicaRegra (detectaPadrao h) (Node t fst snd))
montaTableau (Node [] fst snd) = (Node [] (montaTableau fst) (montaTableau snd))
montaTableau (Leaf []) = (Leaf [])
montaTableau (Leaf (h:t)) = junta h (aplicaRegra (detectaPadrao h) (Leaf t))

aplicaRegra :: (Int, [String]) -> Tree -> Tree

aplicaRegra (1, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (1, [formula1, formula2]) fst) (aplicaRegra (1, [formula1, formula2]) snd))
aplicaRegra (1, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++[formula1]++[formula2]))

aplicaRegra (2, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (2, [formula1, formula2]) fst) (aplicaRegra (2, [formula1, formula2]) snd))
aplicaRegra (2, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf [formula1]) (Leaf [formula2]))

aplicaRegra (3, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (3, [formula1, formula2]) fst) (aplicaRegra (3, [formula1, formula2]) snd))
aplicaRegra (3, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(~, "++formula1++")"]) (Leaf [formula2]))

aplicaRegra (4, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (4, [formula1, formula2]) fst) (aplicaRegra (4, [formula1, formula2]) snd))
aplicaRegra (4, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(^, "++formula1++", "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), (~, "++formula2++"))"]))

aplicaRegra (5, [formula]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (5, [formula]) fst) (aplicaRegra (5, [formula]) snd))
aplicaRegra (5, [formula]) (Leaf t) = montaTableau (Leaf (t++[formula]))

aplicaRegra (6, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (6, [formula1, formula2]) fst) (aplicaRegra (6, [formula1, formula2]) snd))
aplicaRegra (6, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(~, "++formula1++")"]) (Leaf ["(~, "++formula2++")"]))

aplicaRegra (7, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (7, [formula1, formula2]) fst) (aplicaRegra (7, [formula1, formula2]) snd))
aplicaRegra (7, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++["(~, "++formula1++")"]++["(~, "++formula2++")"]))

aplicaRegra (8, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (8, [formula1, formula2]) fst) (aplicaRegra (8, [formula1, formula2]) snd))
aplicaRegra (8, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t++[formula1]++["(~, "++formula2++")"]))

aplicaRegra (9, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (9, [formula1, formula2]) fst) (aplicaRegra (9, [formula1, formula2]) snd))
aplicaRegra (9, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]))

aplicaRegra (10, [formula]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (10, [formula]) fst) (aplicaRegra (10, [formula]) snd))
aplicaRegra (10, [formula]) (Leaf t) = montaTableau (Leaf t)

aplicaRegra (11, [formula]) (Node t fst snd) = montaTableau (Node t (aplicaRegra (11, [formula]) fst) (aplicaRegra (11, [formula]) snd))
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
