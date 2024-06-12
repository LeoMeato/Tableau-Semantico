import Data.List (intercalate)
import Text.Printf

data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

tableau :: String -> Tree
tableau entrada = montaTableau (Leaf ["F:" ++ entrada])

testaTableau :: String -> Bool
testaTableau entrada = tautologia [] (tableau entrada)

tableauFormatado :: String -> IO ()
tableauFormatado entrada = printAsLines [(treeToString (tableau entrada) "")]

------------------------------------------ Avaliação --------------------------------------------------

tautologia :: [String] -> Tree -> Bool
tautologia listaAcumulada (Node lista fst snd)
  | avalia (listaAcumulada ++ lista) lista == True = True
  | (tautologia (listaAcumulada ++ lista) fst) && (tautologia (listaAcumulada ++ lista) snd) = True
  | otherwise = False
tautologia listaAcumulada (Leaf lista)
  | avalia (listaAcumulada ++ lista) lista == True = True
  | otherwise = False

avalia :: [String] -> [String] -> Bool
avalia acumulada (h : t) = (isIn h acumulada) || (avalia acumulada t)
avalia acumulada [] = False

isIn :: String -> [String] -> Bool
isIn ('F' : ':' : formula) lista = elem ('V' : ':' : formula) lista
isIn ('V' : ':' : formula) lista = elem ('F' : ':' : formula) lista

-------------------------------------------------------------------------------------------------------

------------------------------------------------------- Montagem da Árvore -------------------------------------------------------------------

montaTableau :: Tree -> Tree
montaTableau (Node (h : t) fst snd) = junta h (aplicaRegra (detectaPadrao h) (Node t fst snd))
montaTableau (Node [] fst snd) = (Node [] (montaTableau fst) (montaTableau snd))
montaTableau (Leaf []) = (Leaf [])
montaTableau (Leaf (h : t)) = junta h (aplicaRegra (detectaPadrao h) (Leaf t))

regraTransitiva :: (Int, [String]) -> Tree -> Tree
-- Quando aplicaRegra é aplicada sobre um nó (e não uma folha), o resultado da aplicação da regra deve ser adicionado nas folhas. A regraTransitiva faz esse papel.
-- Caso isso fosse feito chamando aplicaRegra recursivamente, a chamada recursiva de montaTableau ficaria fora de controle.
regraTransitiva (regra, [formula1, formula2]) (Node t fst snd) = (Node t (regraTransitiva (regra, [formula1, formula2]) fst) (regraTransitiva (regra, [formula1, formula2]) snd))
regraTransitiva (regra, [formula]) (Node t fst snd) = (Node t (regraTransitiva (regra, [formula]) fst) (regraTransitiva (regra, [formula]) snd))
regraTransitiva (1, [formula1, formula2]) (Leaf t) = (Leaf (t ++ ["V:" ++ formula1] ++ ["V:" ++ formula2]))
regraTransitiva (2, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["V:" ++ formula1]) (Leaf ["V:" ++ formula2]))
regraTransitiva (3, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["F:" ++ formula1]) (Leaf ["V:" ++ formula2]))
regraTransitiva (4, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["V:(^, " ++ formula1 ++ ", " ++ formula2 ++ ")"]) (Leaf ["V:(^, (~, " ++ formula1 ++ "), (~, " ++ formula2 ++ "))"]))
regraTransitiva (5, [formula]) (Leaf t) = (Leaf (t ++ ["V:" ++ formula]))
regraTransitiva (6, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["F:" ++ formula1]) (Leaf ["F:" ++ formula2]))
regraTransitiva (7, [formula1, formula2]) (Leaf t) = (Leaf (t ++ ["F:" ++ formula1] ++ ["F:" ++ formula2]))
regraTransitiva (8, [formula1, formula2]) (Leaf t) = (Leaf (t ++ ["V:" ++ formula1] ++ ["F:" ++ formula2]))
regraTransitiva (9, [formula1, formula2]) (Leaf t) = (Node t (Leaf ["V:(^, (~, " ++ formula1 ++ "), " ++ formula2 ++ ")"]) (Leaf ["V:(^, " ++ formula1 ++ ", (~, " ++ formula2 ++ "))"]))
regraTransitiva (10, [formula]) (Leaf t) = (Leaf (t ++ ["F:" ++ formula]))
regraTransitiva (11, [formula]) (Leaf t) = (Leaf t)

aplicaRegra :: (Int, [String]) -> Tree -> Tree
aplicaRegra (regra, [formula1, formula2]) (Node t fst snd) = montaTableau (Node t (regraTransitiva (regra, [formula1, formula2]) fst) (regraTransitiva (regra, [(formula1), formula2]) snd))
aplicaRegra (regra, [formula]) (Node t fst snd) = montaTableau (Node t (regraTransitiva (regra, [formula]) fst) (regraTransitiva (regra, [formula]) snd))
aplicaRegra (1, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t ++ ["V:" ++ formula1] ++ ["V:" ++ formula2]))
aplicaRegra (2, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["V:" ++ formula1]) (Leaf ["V:" ++ formula2]))
aplicaRegra (3, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["F:" ++ formula1]) (Leaf ["V:" ++ formula2]))
aplicaRegra (4, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["V:(^, " ++ formula1 ++ ", " ++ formula2 ++ ")"]) (Leaf ["V:(^, (~, " ++ formula1 ++ "), (~, " ++ formula2 ++ "))"]))
aplicaRegra (5, [formula]) (Leaf t) = montaTableau (Leaf (t ++ ["V:" ++ formula]))
aplicaRegra (6, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["F:" ++ formula1]) (Leaf ["F:" ++ formula2]))
aplicaRegra (7, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t ++ ["F:" ++ formula1] ++ ["F:" ++ formula2]))
aplicaRegra (8, [formula1, formula2]) (Leaf t) = montaTableau (Leaf (t ++ ["V:" ++ formula1] ++ ["F:" ++ formula2]))
aplicaRegra (9, [formula1, formula2]) (Leaf t) = montaTableau (Node t (Leaf ["V:(^, (~, " ++ formula1 ++ "), " ++ formula2 ++ ")"]) (Leaf ["V:(^, " ++ formula1 ++ ", (~, " ++ formula2 ++ "))"]))
aplicaRegra (10, [formula]) (Leaf t) = montaTableau (Leaf (t ++ ["F:" ++ formula]))
aplicaRegra (11, [formula]) (Leaf t) = montaTableau (Leaf t)

junta :: String -> Tree -> Tree
junta h (Node t fst snd) = (Node (h : t) fst snd)
junta h (Leaf t) = (Leaf (h : t))

-----------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------- Detecção de Padrão -----------------------------------------------

detectaPadrao :: String -> (Int, [String]) -- Recebe uma fórmula e retorna um número de 1 a 10, que indica a regra a ser aplicada (e o 11 que significa que não se encaixa em nenhuma) e
                                           -- uma lista com as sub-fórmulas imediatas da fórmula em questão, podendo ser uma ou duas, dependendo da regra.
detectaPadrao ('V' : ':' : '(' : '^' : ',' : ' ' : resto) = (1, (auxDetecta resto))
detectaPadrao ('V' : ':' : '(' : 'v' : ',' : ' ' : resto) = (2, (auxDetecta resto))
detectaPadrao ('V' : ':' : '(' : '-' : '>' : ',' : ' ' : resto) = (3, (auxDetecta resto))
detectaPadrao ('V' : ':' : '(' : '<' : '-' : '>' : ',' : ' ' : resto) = (4, (auxDetecta resto))
detectaPadrao ('F' : ':' : '(' : '~' : ',' : ' ' : '(' : resto) = (5, [achaFimSimples resto "" (-1)])
detectaPadrao ('F' : ':' : '(' : '^' : ',' : ' ' : resto) = (6, (auxDetecta resto))
detectaPadrao ('F' : ':' : '(' : 'v' : ',' : ' ' : resto) = (7, (auxDetecta resto))
detectaPadrao ('F' : ':' : '(' : '-' : '>' : ',' : ' ' : resto) = (8, (auxDetecta resto))
detectaPadrao ('F' : ':' : '(' : '<' : '-' : '>' : ',' : ' ' : resto) = (9, (auxDetecta resto))
detectaPadrao ('V' : ':' : '(' : '~' : ',' : ' ' : '(' : resto) = (10, [achaFimSimples resto "" (-1)])
detectaPadrao formula = (11, [(formula)])

-- funções para isolar as sub-fórmulas

auxDetecta :: String -> [String]
auxDetecta ('(' : resto) = achaFimDuplo resto "" (-1)

auxDetecta2 :: String -> String -> Int -> String
auxDetecta2 (',' : ' ' : '(' : resto) x count = achaFimSimples resto x count

achaFimDuplo :: String -> String -> Int -> [String]
achaFimDuplo (h : t) formula1 count
  | count == 0 = ['(' : formula1, (auxDetecta2 (h : t) "" (-1))]
  | count < 0 = achaFimDuplo t (formula1 ++ [h]) (count + (decresce h))
  | otherwise = ["Tem coisa errada aí"]

achaFimSimples :: String -> String -> Int -> String
achaFimSimples (h : t) formula count
  | count == 0 = '(' : formula
  | count < 0 = achaFimSimples t (formula ++ [h]) (count + (decresce h))
  | otherwise = "Tem coisa errada aí"

decresce :: Char -> Int
decresce '(' = -1
decresce ')' = 1
decresce _ = 0

-----------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------- Formatação da Árvore -----------------------------------------------

printAsLines :: [String] -> IO ()
printAsLines xs = mapM_ putStr xs

showFormulaList :: [String] -> String
showFormulaList formulaList =
  intercalate "   /--/   " $
    formulaList

treeToString :: Tree -> String -> String
treeToString (Leaf content) blank = blank ++ (showFormulaList content) ++ "\n"
treeToString (Node content fstchild sndchild) blank =
  blank
    ++ (showFormulaList content)
    ++ "\n"
    ++ (treeToString (fstchild) (blank ++ ".     "))
    ++ (treeToString (sndchild) (blank ++ ".     "))
    ++ "\n"

--------------------------------------------------------------------------------------------