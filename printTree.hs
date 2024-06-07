import  Data.List (intercalate)
import  Text.Printf

data Tree = Leaf {content :: [String]} | Node {content :: [String], fstchild :: Tree, sndchild :: Tree} deriving (Show)

printAsLines :: [String] -> IO ()
printAsLines xs = mapM_ putStr  xs  -- a loop in the IO monad



showFormulaList :: [String] -> String
showFormulaList formulaList = intercalate ","  $
                            map (printf "%-9s") formulaList



treeToString :: Tree -> String -> String
treeToString (Leaf content) blank = blank ++ (showFormulaList content) ++ "\n"
treeToString (Node content fstchild sndchild) blank = (showFormulaList content) ++ "\n"  ++ "      " ++ 
                                                (treeToString (fstchild) (blank++"     ")) ++ (treeToString (sndchild) (blank++"     ")) ++ "\n" ++ "      "


