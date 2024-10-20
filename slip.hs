-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
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

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente
s2l :: Sexp -> Lexp

-- Un nombre litéral
s2l (Snum n) = Lnum n

-- Booléens litéraux
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False

-- Variables
s2l (Ssym s) = Lvar s

-- Condition avec keyword if
s2l (Snode (Ssym "if") [e1, e2, e3]) =
    Ltest (s2l e1) (s2l e2) (s2l e3)

-- fob
s2l (Snode (Ssym "fob") [params, body]) =
    let nomParams = getParamsFromSexp params
    in Lfob nomParams (s2l body) --On évalue le body de la fonction séparémment
                                 --(probablement un Snode, la plupart du temps)

s2l (Snode (Ssym "let") [declarations, body]) =
    case declarations of
        Snode (Ssym nouvelleVariable) [expressions] -> 
            Llet nouvelleVariable (s2l expressions) (s2l body) 
        _ -> error "déclarations de variables invalides" 

-- Fix, bonne chance!
s2l (Snode (Ssym "fix") [defs, body]) =
    let lDefs = case defs of -- pour chaque définitions
            Snil -> []  -- cas liste vide (vu dans énoncé)
            Snode x xs -> map getDefinition (x : xs)
            _ -> error "Erreur avec fix (sûrement mauvaise syntaxe)"

        -- Fonction pour extraire les assignations des définitions
        getDefinition assignations = case assignations of
            Snode varS [exp1] -> case varS of
                Ssym var -> (var, s2l exp1)
                Snode (Ssym var) args ->  -- plusieurs arguments (un Snode)

                    -- Fonction pour extraire les noms des arguments
                    let extractArgVars [] = []
                        extractArgVars (Ssym x : xs) = x : extractArgVars xs 
                        extractArgVars (_ : xs) = extractArgVars xs 
                        argVar = extractArgVars args  -- On extrait args
                        f = Lfob argVar (s2l exp1) 
                    in (var, f)  -- on retourne l'assignation
                _ -> error "Varialbe invalide"
            _ -> error "Asignation invalde invalide dans"  -- format incorrect 
    
    in Lfix lDefs (s2l body)  -- On retourne un Lfix avec les défs et body

-- Lsend
-- Si on voit un Snode avec aucun keyword (fix, let, etc), 
-- on évalue le corps de la fonction et les arguments
s2l (Snode func args) = Lsend (s2l func) (map s2l args) 

s2l se = 
    error ("Expression Psil inconnue: " ++ showSexp se ++ 
          "\nDebug: s2l reçu : " ++ show se)

-- ==== Fonctions auxiliaires ====
-- Utilisée pour obtenir une liste de paramètres à partir d'un Sexp
getParamsFromSexp :: Sexp -> [Var]
getParamsFromSexp Snil = []
getParamsFromSexp (Ssym s) = [s]
-- On devrait seulement entrer ici si on passe
-- une expression en tant qu'arguments (Slip utilise call by value)
getParamsFromSexp (Snode l1 l2) = map getParamFromSsym (l1 : l2)
getParamsFromSexp _ = error "Erreur générale (paramètres invalides? )"

-- Utilisé quand on à un Snode avec une liste d'arguments,
-- plus simple d'utiliser une fonction à part que d'écrire
-- le code directement dans getParamsFromSexp
getParamFromSsym :: Sexp -> Var
getParamFromSsym (Ssym s) = s
getParamFromSsym _ = error "Erreur générale pour évaluation de paramètre"

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
-- Retourne un nom litéral
eval _ (Lnum n) = Vnum n

-- Retourne un booléen litéral
eval _ (Lbool b) = Vbool b

-- Vérifier si la variable existe dans l'envrionnement
eval env (Lvar var) = extractVal env var

-- Condition if
eval env (Ltest e1 e2 e3) = -- if e1 then e2 else e3
    case eval env e1 of
        Vbool True -> eval env e2
        Vbool False -> eval env e3
        _ -> error ("la condition n'est pas true ou false après" ++
                    "avoir été évaluée (Call by value)")

-- Un fonction object.
eval env (Lfob params body) = Vfob env params body

-- Lsend
eval env (Lsend expression argOfExpression) =
    let function = eval env expression
        -- On évalue pour obtenir les arguments et la fonction qu'on appel
        arguments = map (eval env) argOfExpression 
    in case function of
        Vbuiltin f -> f arguments
        Vfob corps arguments' body ->
            -- La fonction est pas built in, on doit s'assurer que le nombre 
            -- d'arguments est bon et on doit les ajouter à l'environnement
            if length arguments' /= length arguments
            then error "Nombre d'arguments ou syntaxe incorrect"
            else let newEnv = zip arguments' arguments ++ corps
                 in eval newEnv body
        _ -> error "Erreure générale, probablement une erreur de syntaxe"

eval env (Llet var e1 body) = --let var = 3 in body
    let val1 = eval env e1
        env2 = (var, val1) : env
    in eval env2 body

-- prend un (var, val), un env, et evalu la valeur + ajoute la variable à l'env
eval env (Lfix definitions body) =
    let addAssignations (var, val) accEnv = (var, eval newEnv val) : accEnv  
        -- nouvel env avec définitions évaluées
        newEnv = foldr addAssignations env definitions
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
