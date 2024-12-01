

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}



--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage


---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec
    ( alphaNum,
      anyChar,
      char,
      digit,
      satisfy,
      space,
      eof,
      many1,
      (<|>),
      many,
      parse,
      pzero,
      Parser ) -- Bibliothèque d'analyse syntaxique.
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

data Type = Terror String        -- Utilisé quand le type n'est pas connu.
          | Tnum                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tfob [Type] Type     -- Type des fobjets.
          deriving (Show, Eq)

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [(Var, Type)] Lexp -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Fonction auxiliaire pour extraire la liste des éléments d'un Snode
s2list :: Sexp -> [Sexp]
s2list (Snode _ xs) = xs
s2list _ = error "Pas une liste"

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
-- Gestion des bools
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False

-- Gestion des variables
s2l (Ssym s) = Lvar s

-- Gestion des ifs
s2l (Snode (Ssym "if") [cond, then_, else_]) = 
    Ltest (s2l cond) (s2l then_) (s2l else_)
s2l (Snode (Ssym "if") _) = 
    error "L'expression 'if' nécessite 3 arguments"

-- Gestion des let
s2l (Snode (Ssym "let") [Ssym var, val, body]) = 
    Llet var (s2l val) (s2l body)
s2l (Snode (Ssym "let") _) = 
    error "L'expression 'let' nécessite 3 arguments: var, val, et body"

-- Gestion des fob
s2l (Snode (Ssym "fob") [args, body]) =
    let getVars (Snode _ xs) = map (\(Ssym x) -> x) xs
        getVars _ = error "Liste de variables malformée"
    in Lfob (map (\v -> (v, Tnum)) (getVars args)) (s2l body)
s2l (Snode (Ssym "fob") _) = 
    error "Syntaxe de fob incorrecte"

-- Gestion des fix
s2l (Snode (Ssym "fix") [decls, body]) =
    let sdecl2ldecl (Snode (Snode (Ssym v) args) [e]) =
            (v, Lfob (map (\(Ssym x) -> (x, Tnum)) args) (s2l e))
        sdecl2ldecl se = error ("Declaration Psil inconnue: " ++ showSexp se)
    in Lfix (map sdecl2ldecl (s2list decls)) (s2l body)

-- Gestion des appels de fobjets
s2l (Snode f args) = Lsend (s2l f) (map s2l args)

-- ¡¡COMPLÉTER ICI!!
s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv Int Dexp -- L'entier indique le nombre d'arguments.

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type Env = [(Var, Type, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
           intbin = Tfob [Tnum, Tnum] Tnum
           boolbin = Tfob [Tnum, Tnum] Tbool

       in [("+", intbin,  binop Vnum (+)),
           ("*", intbin,  binop Vnum (*)),
           ("/", intbin,  binop Vnum div),
           ("-", intbin,  binop Vnum (-)),
           ("<", boolbin, binop Vbool (<)),
           (">", boolbin, binop Vbool (>)),
           ("≤", boolbin, binop Vbool (<=)),
           ("≥", boolbin, binop Vbool (>=)),
           ("=", boolbin, binop Vbool (==)),
           ("true",  Tbool, Vbool True),
           ("false", Tbool, Vbool False)]

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = [(Var, Type)]

-- `check c Γ e` renvoie le type de `e` dans l'environnement `Γ`.
-- Si `c` est vrai, on fait une vérification complète, alors que s'il
-- est faux, alors on présume que le code est typé correctement et on
-- se contente de chercher le type.
check :: Bool -> TEnv -> Lexp -> Type
check _ _ (Lnum _) = Tnum
check _ _ (Lbool _) = Tbool
-- ¡¡COMPLÉTER ICI!!

-- Gestion des variables
check _ env (Lvar v) = 
    case lookup v env of
        -- La variable existe dans l'environnement, renvoyer son type
        Just t -> t  
        -- La variable n'est pas définie, erreur
        Nothing -> error ("Variable non définie: " ++ v)  

-- Gestion des Ifs
check c env (Ltest cond then_ else_) =
    let tcond = check c env cond
        tthen = check c env then_
        telse = check c env else_
    in if c && tcond /= Tbool 
       then error "La condition doit être un booléen"
       else if tthen /= telse
            then error "Les branches 'then' et 'else' doivent avoir le même type"
            else tthen

-- Gestion des Let
check c env (Llet var e1 e2) =
    let t1 = check c env e1
        env' = (var, t1) : env
    in check c env' e2

-- Gestion de Lsend
check c env (Lsend f args) =
    case check c env f of
        Tfob argTypes retType ->
            if length args /= length argTypes then
                error "Nombre incorrect d'arguments"
            else if c then
                -- En mode vérification, on vérifie que les types correspondent
                let argTypes' = map (check c env) args in
                if argTypes' == argTypes then retType
                else error "Type des arguments incorrect"
            else retType
        _ -> error "Ce n'est pas une fonction"


-- AjOUT: Gestion des fob !!
check _ env (Lfob args body) =
    let targs = map snd args
        env' = zip (map fst args) targs ++ env
    in check True env' body

-- Gestion des déclarations récursives fix
check c env (Lfix decls body) =
    let tempEnv = [(var, Terror "Type temporaire") | (var, _) <- decls] ++ env
        guessedEnv = foldl (\acc (var, expr) ->
            case check False acc expr of
                Tfob argTypes retType -> (var, Tfob argTypes retType) : acc
                _ -> error "Erreur dans l'inférence des types pour fix"
            ) tempEnv decls
        verifiedEnv = foldl (\acc (var, expr) ->
            case check True guessedEnv expr of
                Tfob argTypes retType -> (var, Tfob argTypes retType) : acc
                _ -> error "Erreur de typage lors de la vérification pour fix"
            ) [] decls
    in check c (verifiedEnv ++ env) body
---------------------------------------------------------------------------
-- Pré-évaluation
---------------------------------------------------------------------------

-- Dexp simplifie le code en éliminant deux aspects qui ne sont plus
-- utiles lors de l'évaluation:
-- - Les annotations de types.
-- - Les noms de variables, remplacés par des entiers qui représentent
--   la position de la variable dans l'environnement.  On appelle ces entiers
--   des [indexes de De Bruijn](https://en.wikipedia.org/wiki/De_Bruijn_index).

type VarIndex = Int

data Dexp = Dnum Int             -- Constante entière.
          | Dbool Bool           -- Constante Booléenne.
          | Dvar VarIndex        -- Référence à une variable.
          | Dtest Dexp Dexp Dexp -- Expression conditionelle.
          | Dfob Int Dexp        -- Construction de fobjet de N arguments.
          | Dsend Dexp [Dexp]    -- Appel de fobjet.
          | Dlet Dexp Dexp       -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Renvoie le "De Buijn index" de la variable, i.e. sa position
-- dans le contexte.
lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _) : xs) x2 n = if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI _ x _ = error ("Variable inconnue: " ++ show x)

-- Conversion de Lexp en Dexp.
-- Les types contenus dans le "TEnv" ne sont en fait pas utilisés.
l2d :: TEnv -> Lexp -> Dexp
-- Conversion des constantes
l2d _ (Lnum n) = Dnum n
l2d _ (Lbool b) = Dbool b

-- Gestion des variables
l2d tenv (Lvar v) = Dvar (lookupDI tenv v 0)

-- Gestion des fonctions fob
l2d env (Lfob args body) = 
    let env' = map (\(var, _) -> (var, undefined)) args ++ env
    in Dfob (length args) (l2d env' body)

-- Gestion des Ifs
l2d env (Ltest cond then_ else_) = 
    Dtest (l2d env cond) (l2d env then_) (l2d env else_)

-- Gestion des Let
l2d env (Llet var e1 e2) = 
    Dlet (l2d env e1) (l2d ((var, undefined):env) e2)

-- Gestion des annotations de type
l2d env (Ltype expr _) = l2d env expr  -- Ignorer les annotations de type

-- Conversion des déclarations récursives fix
l2d env (Lfix decls body) =
    let tempEnv = [(var, undefined) | (var, _) <- decls] ++ env
        -- Convertir les expressions de `decls` en Dexp
        decls' = map (\(var, expr) -> l2d tempEnv expr) decls
    in Dfix decls' (l2d tempEnv body)

-- Ajouter une gestion de Lsend (appel de fonction)
l2d env (Lsend f args) = Dsend (l2d env f) (map (l2d env) args)

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
-- Évaluation des constantes
eval _ (Dnum n) = Vnum n
eval _ (Dbool b) = Vbool b

-- Évaluation des variables (via les indices de De Bruijn)
eval env (Dvar idx) =
    if idx < length env then env !! idx
    else error ("Variable inconnue à l'index : " ++ show idx)

-- Évaluation des expressions conditionnelles
eval env (Dtest cond then_ else_) =
    case eval env cond of
        Vbool True -> eval env then_
        Vbool False -> eval env else_
        _ -> error "La condition doit être un booléen"

-- Évaluation des déclarations let
eval env (Dlet e1 e2) =
    let v1 = eval env e1
    in eval (v1 : env) e2

-- Évaluation des fonctions fob
eval env (Dfob argCount body) =
    Vfob env argCount body

-- Évaluation des appels de fonctions
eval env (Dsend f args) =
    case eval env f of
        Vbuiltin func -> func (map (eval env) args)
        Vfob closureEnv argCount body ->
            if length args /= argCount then
                error ("Nombre incorrect d'arguments : attendu " ++ show argCount ++
                       ", reçu " ++ show (length args))
            else eval (map (eval env) args ++ closureEnv) body
        _ -> error "Tentative d'appel sur une expression non fonctionnelle"

-- Évaluation des déclarations récursives fix
eval env (Dfix decls body) =
    let tempEnv = replicate (length decls) (error "Référence circulaire dans fix") -- Déclaration temporaire
        -- Convertir les déclarations en Dexp et évaluer
        fixedValues = map (eval tempEnv) decls
    in eval (fixedValues ++ env) body




-- Fonction auxiliaire pour remplacer une valeur dans une liste
replace :: Int -> [a] -> [a] -> [a]
replace idx newVal xs = take idx xs ++ newVal ++ drop (idx + 1) xs

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_x,_t,v) -> v) env0

-- Évaluation sans vérification de types.
evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se = let le = s2l se
               in case check True tenv0 le of
                    Terror err -> Right err
                    t -> Left (t, eval venv0 (l2d tenv0 le))


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

