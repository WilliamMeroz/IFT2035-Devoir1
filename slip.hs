-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t
-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False
s2l (Ssym s) = Lvar s

s2l (Snode (Ssym "if") [e1, e2, e3]) =
    Ltest (s2l e1) (s2l e2) (s2l e3)

-- Cas pour 'fob'
s2l (Snode (Ssym "fob") [paramsSexp, body]) =
    let paramNames = extractParamNames paramsSexp
    in Lfob paramNames (s2l body)
  where
    extractParamNames Snil = []
    extractParamNames (Ssym s) = [s]
    extractParamNames (Snode phead ptail) = map extractParam (phead : ptail)
    extractParamNames _ = error "Paramètres invalides dans 'fob'"
    
    extractParam (Ssym s) = s
    extractParam _ = error "Paramètre non symbolique dans 'fob'"


s2l (Snode (Ssym "let") [assignations, body]) = -- let assignations in body
    case assignations of -- Pour chaque assignation
        Snode (Ssym var) [e1] -> Llet var (s2l e1) (s2l body) -- Si assignations = Snode --> on retourne Llet avec var, e1 et body 
        _ -> error "Assignations invalides"


s2l (Snode (Ssym "fix") [defs, body]) = -- fix defs in body
    let lDefs = case defs of -- pour chaque définitions
            Snil -> []  -- cas liste vide
            Snode x xs -> map extractor (x : xs)  -- si Snode, on extrait itérativement les définitions
            _ -> error "Assignations invalides pour 'fix'"  -- autrement = erreur

        -- Fonction pour extraire les assignations des définitions
        extractor assignations = case assignations of
            Snode varS [exp1] -> case varS of -- pour la var de Snode
                Ssym var -> (var, s2l exp1)  -- si assignation simple --> retourne la variable et le Lexp de exp1
                Snode (Ssym var) args ->  -- Si assignation avec plusieurs arguments (un Snode)

                    -- Fonction pour extraire les noms des arguments
                    let extractArgVars [] = []
                        extractArgVars (Ssym x : xs) = x : extractArgVars xs -- si premier élément est un Ssym, on l'ajoute à la liste
                        extractArgVars (_ : xs) = extractArgVars xs -- autrement, on passe à l'élément suivant
                        argVar = extractArgVars args  -- On extrait les noms des arguments
                        f = Lfob argVar (s2l exp1)  -- on construit fonction Lfob avec liste des noms d'arguments puis on transforme exp1 en Lexp
                    in (var, f)  -- on retourne l'assignation
                _ -> error "Variable invalide dans 'fix'"  -- autrement = erreur
            _ -> error "Assingation invalide dans 'fix'"  -- format incorrect 
    
    in Lfix lDefs (s2l body)  -- On retourne un Lfix avec les définitions et le body


s2l (Snode func args) = Lsend (s2l func) (map s2l args)


s2l se = error ("Expression Psil inconnue: " ++ showSexp se ++ "\nDebug: s2l reçu : " ++ show se)


-- s2l (Snode (Ssym "+") [e1, e2]) = Lsend (Lvar "+") [s2l e1, s2l e2]
---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
eval _ (Lnum n) = Vnum n
eval _ (Lbool b) = Vbool b
eval env (Lvar var) = extractVal env var

eval env (Ltest eIf eThen eElse) = -- if e1 then e2 else e3
    case eval env e1 of
        Vbool True -> eval env e2
        Vbool False -> eval env e3
        _ -> error "La condition 'if' n'est pas booléen"


eval env (Lfob params body) = Vfob env params body

eval env (Lsend funcExpr argExprs) =
    let funcVal = eval env funcExpr
        argVals = map (eval env) argExprs
    in case funcVal of
        Vbuiltin f -> f argVals
        Vfob closureEnv params body ->
            if length params /= length argVals
            then error "Nombre d'arguments incorrect"
            else let newEnv = zip params argVals ++ closureEnv
                 in eval newEnv body
        _ -> error "Tentative d'appeler une valeur non fonction"

eval env (Llet var e1 body) = --let var = 3 in body
    let val1 = eval env e1
        env2 = (var, val1) : env
    in eval env2 body


eval env (Lfix definitions body) = -- fix definitions in body
    -- prend un (var, val), un env, et evalu la valeur + ajoute la variable à l'env
    let addAssignations (var, val) accEnv = (var, eval newEnv val) : accEnv  
        newEnv = foldr addAssignations env definitions -- nouvel env avec définitions évaliées
    in eval newEnv body -- on évalue le body avec le nouvel env

---------------------------------------------------------------------------
-- fonctions ajoutées                                                    --
---------------------------------------------------------------------------

-- Fonction pour extraire la valeur d'une variable
extractVal :: VEnv -> Var -> Value
extractVal [] var = error("Variable" ++ var ++ "non définie")
extractVal ((var', val'):xs) var  =
    if var == var'
    then val'
    else extractVal xs var


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
