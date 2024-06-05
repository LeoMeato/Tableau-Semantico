data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

{-
tableau :: String -> Tree
tableau entrada = montaTableau (Node [entrada] (Leaf []) (Leaf []))

montaTableau :: Tree -> Tree
montaTableau (Node (h:t) fst snd) = aplicaRegra (detectaPadrao h) h (Node t fst snd)

aplicaRegra :: (Int, [String]) -> String -> Tree -> Tree

aplicaRegra (1, [formula1, formula2]) h (Node t fst snd) = Node(junta h (Node t++[formula1]++[formula2] fst snd))

junta :: String -> Tree -> Tree
junta h (Node t fst snd) = (Node (h:t) fst snd)
-}

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
