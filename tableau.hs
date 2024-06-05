data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

tableau :: String -> Tree
tableau entrada = montaTableau (Leaf [entrada])

------------------------------------------------------- Montagem da Árvore -------------------------------------------------------------------

-- Ainda falta jogar as conclusões pras folhas

montaTableau :: Tree -> Tree
montaTableau (Node (h:t) fst snd) = aplicaRegra (detectaPadrao h) h (Node t fst snd)
montaTableau (Node [] (Node (h1:t1) fst1 snd1) (Node (h2:t2) fst2 snd2)) = (Node [] (aplicaRegra (detectaPadrao h1) h1 (Node t1 fst1 snd1)) (aplicaRegra (detectaPadrao h2) h2 (Node t2 fst2 snd2)))
montaTableau (Leaf []) = (Leaf [])
montaTableau (Leaf (h:t)) = aplicaRegra (detectaPadrao h) h (Leaf t)

aplicaRegra :: (Int, [String]) -> String -> Tree -> Tree

aplicaRegra (1, [formula1, formula2]) h (Node t fst snd) = junta h (montaTableau (Node (t++[formula1]++[formula2]) fst snd))
aplicaRegra (1, [formula1, formula2]) h (Leaf t) = junta h (montaTableau (Leaf (t++[formula1]++[formula2])))

aplicaRegra (2, [formula1, formula2]) h (Node t fst snd) = junta h (Node t (Leaf ((content fst)++[formula1])) (Leaf ((content snd)++[formula2])))
aplicaRegra (2, [formula1, formula2]) h (Leaf t) = junta h (Node t (Leaf [formula1]) (Leaf [formula2]))

aplicaRegra (3, [formula1, formula2]) h (Node t fst snd) = junta h (Node t (Leaf ((content fst)++["(~, "++formula1++")"])) (Leaf ((content snd)++[formula2])))
aplicaRegra (3, [formula1, formula2]) h (Leaf t) = junta h (Node t (Leaf ["(~, "++formula1++")"]) (Leaf [formula2]))

aplicaRegra (4, [formula1, formula2]) h (Node t fst snd) = junta h (Node t (Leaf ((content fst)++["(^, "++formula1++", "++formula2++")"])) (Leaf ((content snd)++["(^, (~, "++formula1++"), (~, "++formula2++"))"])))
aplicaRegra (4, [formula1, formula2]) h (Leaf t) = junta h (Node t (Leaf ["(^, "++formula1++", "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), (~, "++formula2++"))"]))

aplicaRegra (5, [formula]) h (Node t fst snd) = junta h (montaTableau (Node (t++[formula]) fst snd))
aplicaRegra (5, [formula]) h (Leaf t) = junta h (montaTableau (Leaf (t++[formula])))

aplicaRegra (6, [formula1, formula2]) h (Node t fst snd) = junta h (Node t (Leaf ((content fst)++["(~, "++formula1++")"])) (Leaf ((content snd)++["(~, "++formula2++")"])))
aplicaRegra (6, [formula1, formula2]) h (Leaf t) = junta h (Node t (Leaf ["(~, "++formula1++")"]) (Leaf ["(~, "++formula2++")"]))

aplicaRegra (7, [formula1, formula2]) h (Node t fst snd) = junta h (montaTableau (Node (t++["(~, "++formula1++")"]++["(~, "++formula2++")"]) fst snd))
aplicaRegra (7, [formula1, formula2]) h (Leaf t) = junta h (montaTableau (Leaf (t++["(~, "++formula1++")"]++["(~, "++formula2++")"])))

aplicaRegra (8, [formula1, formula2]) h (Node t fst snd) = junta h (montaTableau (Node (t++[formula1]++["(~, "++formula2++")"]) fst snd))
aplicaRegra (8, [formula1, formula2]) h (Leaf t) = junta h (montaTableau (Leaf (t++[formula1]++["(~, "++formula2++")"])))

aplicaRegra (9, [formula1, formula2]) h (Node t fst snd) = junta h (Node t (Leaf ((content fst)++["(^, (~, "++formula1++"), "++formula2++")"])) (Leaf ((content snd)++["(^, (~, "++formula1++"), "++formula2++")"])))
aplicaRegra (9, [formula1, formula2]) h (Leaf t) = junta h (Node t (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]) (Leaf ["(^, (~, "++formula1++"), "++formula2++")"]))

aplicaRegra (10, [formula]) h (Node t fst snd) = junta h (montaTableau (Node t fst snd))
aplicaRegra (10, [formula]) h (Leaf t) = junta h (montaTableau (Leaf t))

aplicaRegra (11, [formula]) h (Node t fst snd) = junta h (montaTableau (Node t fst snd))
aplicaRegra (11, [formula]) h (Leaf t) = junta h (montaTableau (Leaf t))

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
