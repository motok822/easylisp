module Lisp where

import Parser
import Prelude hiding (null, fail)
import Data.Char (isSpace)
import Debug.Trace (trace)
import Data.Map as M (Map, insert, lookup, empty, member, fromList)

data SExpr
    = SNum Int
    | SBool Bool
    | Null
    | SSymbol String
    | SList [SExpr]
    deriving (Eq, Show)

data SMacro = SMacro
    { smacroParams :: [String]
    , smacroRest :: Maybe String
    , smacroBody :: SExpr
    }
    deriving (Eq, Show)

data TopSExpr
    = SDefine String SExpr
    | SDefMacro String SMacro
    | STopExpr SExpr
    deriving (Eq, Show)

data Expr
    = Num Int
    | Bool Bool
    | Symbol String
    | Quote SExpr
    | If Expr Expr Expr
    | Lambda [String] Expr
    | App Expr [Expr]
    deriving (Eq, Show)

data Macro = Macro
    { macroParams :: [String]
    , macroRest :: Maybe String
    , macroBody :: Expr
    }
    deriving (Eq, Show)

data Top
    = Define String Expr
    | DefMacro String Macro
    | TopExpr Expr
    deriving (Eq, Show)

isAtomChar :: Char -> Bool
isAtomChar c = not (isSpace c) && c `notElem` "()'`,\";"

ident :: Parser String String
ident = token (some (sat isAtomChar))

sidentifier :: Parser String SExpr
sidentifier = do {s <- some (sat isAtomChar); return (SSymbol s)}

quote :: Parser String SExpr
quote = do {symbol "'"; return (SSymbol "quote")}

sdigit = do {n <- nat; return (SNum n)}
snull = do {symbol "null"; return Null}
sbool = do {symbol "#t"; return (SBool True)} <|> do {symbol "#f"; return (SBool False)}

primitive = token (quote >>= rest) <|> token (sdigit <|> sbool <|> snull <|> sidentifier)
    where
    rest :: SExpr -> Parser String SExpr
    rest (SSymbol "quote") = do {e <- primitive; return (SList [SSymbol "quote", e])}
    rest _ = fail

slist :: Parser String SExpr
slist = token (quote >>= rest) <|> token (do {symbol "("; xs <- many sexpr; symbol ")"; return (SList xs)})
    where
        rest :: SExpr -> Parser String SExpr
        rest (SSymbol "quote") = do {e <- slist; return (SList [SSymbol "quote", e])}
        rest _ = fail

sexpr :: Parser String SExpr
sexpr = token (primitive <|> slist)

top :: Parser String TopSExpr
top = token (define <|> defmacro <|> topexpr)
    where
        define = do {symbol "define"; name <- ident; e <- sexpr; return (SDefine name e)}
        paramlist = do {symbol "("; params <- somewith (symbol ",") ident; symbol ")"; return params}
        defmacro = do {symbol "defmacro"; name <- ident; params <- paramlist; body <- sexpr; return (SDefMacro name (SMacro params Nothing body))}
        topexpr = do {e <- sexpr; return (STopExpr e)}

bindMacroArgs :: [String] -> Maybe String -> [Value] -> Either String [(String, Value)]
bindMacroArgs params rest args 
    | length args < n = Left "Not enough arguments for macro"
    | otherwise = case (rest, extra) of
        (Nothing, []) -> Right (zip params args)
        (Just r, extra) -> Right (zip params (take n args) ++ [(r, VList extra)])
        (Nothing, extra) -> Left "Too many arguments for macro"
    where
        n = length params
        extra = drop n args

valueToSExpr :: Value -> Either String SExpr
valueToSExpr (VNum n) = Right (SNum n)
valueToSExpr (VBool b) = Right (SBool b)
valueToSExpr VNull = Right Null
valueToSExpr (VSymbol s) = Right (SSymbol s)
valueToSExpr (VList vs) = do
    sexprs <- mapM valueToSExpr vs
    Right (SList sexprs)
valueToSExpr (VClosure _ _ _) = Left "Cannot convert closure to SExpr" -- Macroによって関数を定義することは許さない

expandMacro :: Scope -> Macro -> [SExpr] -> Either String SExpr
expandMacro scope (Macro params rest body) args = do
    bindings <- bindMacroArgs params rest (map sexprToValue args)
    let macroScope = (createScope scope) {scopeVars = M.fromList bindings}
    v <- eval macroScope body
    valueToSExpr v

sexprToexpr :: Scope -> SExpr -> Either String Expr
sexprToexpr _ (SNum n) = Right (Num n)
sexprToexpr _ (SBool b) = Right (Bool b)
sexprToexpr _ Null = Right (Symbol "null")
sexprToexpr _ (SSymbol s) = Right (Symbol s)
sexprToexpr _ (SList [SSymbol "quote", e]) = Right (Quote e)
sexprToexpr scope (SList [SSymbol "if", e1, e2, e3]) = do
    cond <- sexprToexpr scope e1
    thenExpr <- sexprToexpr scope e2
    elseExpr <- sexprToexpr scope e3
    return (If cond thenExpr elseExpr)
sexprToexpr scope (SList [SSymbol "lambda", SList params, body]) = do
    paramNames <- mapM extractParamName params
    bodyExpr <- sexprToexpr (createScope scope) body
    return (Lambda paramNames bodyExpr)
    where
        extractParamName :: SExpr -> Either String String
        extractParamName (SSymbol name) = Right name
        extractParamName _ = Left "Invalid parameter in lambda"
sexprToexpr scope (SList (SSymbol name : args))
    | Just (Right macro) <- searchDef name scope = do
        expanded <- expandMacro scope macro args
        sexprToexpr scope expanded
sexprToexpr scope (SList (func : args)) = do
    funcExpr <- sexprToexpr scope func
    argExprs <- mapM (sexprToexpr scope) args
    return (App funcExpr argExprs)

topsexprTotop :: Scope -> TopSExpr -> Either String Top
topsexprTotop scope (SDefine name e) = do
    expr <- sexprToexpr scope e
    Right (Define name expr)
topsexprTotop scope (SDefMacro name (SMacro params rest body)) = do
    bodyExpr <- sexprToexpr (createScope scope) body
    Right (DefMacro name (Macro params rest bodyExpr))
topsexprTotop scope (STopExpr e) = do
    expr <- sexprToexpr scope e
    Right (TopExpr expr)

gatherMacros :: Scope -> [TopSExpr] -> Map String Macro
gatherMacros scope tops = foldl insertMacro (scopeMacros scope) tops
    where
        insertMacro :: Map String Macro -> TopSExpr -> Map String Macro
        insertMacro macros (SDefMacro name (SMacro params rest body)) =
            let bodyExpr = case sexprToexpr (createScope scope) body of
                    Right expr -> expr
                    Left _ -> error "Invalid macro body" -- This should not happen if the macro is valid
            in M.insert name (Macro params rest bodyExpr) macros
        insertMacro macros _ = macros

sprogramToProgram :: Scope -> [TopSExpr] -> Either String ([Top], Scope)
sprogramToProgram scope tops = do
    let macros = gatherMacros scope tops
    let newScope = scope { scopeMacros = macros }
    tops' <- mapM (topsexprTotop newScope) tops
    Right (tops', newScope)

type MacroEnv = Map String Macro
type DefEnv = Map String Expr

data Scope = Scope {
    scopeVars :: Map String Value,
    scopeMacros :: Map String Macro,
    parent :: Maybe Scope
} deriving (Eq, Show)

createScope :: Scope -> Scope
createScope parentScope = Scope {
    scopeVars = M.empty,
    scopeMacros = M.empty,
    parent = Just parentScope
}

data Value
    = VNum Int
    | VBool Bool
    | VNull
    | VSymbol String
    | VList [Value]
    | VClosure [String] Expr Scope
    | BuiltIn String
    deriving (Eq, Show)

sexprToValue :: SExpr -> Value
sexprToValue (SNum n) = VNum n
sexprToValue (SBool b) = VBool b
sexprToValue Null = VNull
sexprToValue (SSymbol s) = VSymbol s
sexprToValue (SList xs) = VList (map sexprToValue xs)

globalScope :: Scope
globalScope = Scope {
    scopeVars = M.fromList [
        ("null", VNull),
        ("list", BuiltIn "list"),
        ("+", BuiltIn "+"),
        ("-", BuiltIn "-"),
        ("*", BuiltIn "*"),
        ("/", BuiltIn "/")
    ],
    scopeMacros = M.empty,
    parent = Nothing
}

searchDef :: String -> Scope -> Maybe (Either Value Macro)
searchDef name scope =
    case M.lookup name (scopeVars scope) of
        Just v  -> Just (Left v)
        Nothing -> case M.lookup name (scopeMacros scope) of
            Just m -> Just (Right m)
            Nothing -> case parent scope of
                Just p  -> searchDef name p
                Nothing -> Nothing

extractNum :: Scope -> Value -> Either String Int
extractNum _ (VNum n)= Right n
extractNum scope (VSymbol s)=
    case searchDef s scope of
        Just (Left (VNum n)) -> Right n
        Just (Left _)        -> Left ("Expected a number for symbol: " ++ s)
        Just (Right _)       -> Left ("Expected a number, but found a macro: " ++ s)
        Nothing              -> Left ("Undefined symbol: " ++ s)
extractNum _ _ = Left "Expected a number"

applyFunc :: Value -> [Value] -> Scope -> Either String Value
applyFunc (BuiltIn "list") args _ = Right (VList args)
applyFunc (BuiltIn "+") args scope = do
    nums <- mapM (extractNum scope) args
    return (VNum (sum nums))
applyFunc (BuiltIn "-") args scope = do
    nums <- mapM (extractNum scope)args
    case nums of
        [] -> Left "Subtraction requires at least one argument"
        [x] -> return (VNum (-x))
        (x:xs) -> return (VNum (foldl (-) x xs))
applyFunc (BuiltIn "*") args scope = do
    nums <- mapM (extractNum scope) args
    return (VNum (product nums))
applyFunc (BuiltIn "/") args scope = do
    nums <- mapM (extractNum scope) args
    case nums of
        [] -> Left "Division requires at least one argument"
        [x] -> if x == 0 then Left "Division by zero" else return (VNum (1 `div` x))
        (x:xs) -> if any (== 0) xs then Left "Division by zero" else return (VNum (foldl div x xs))
applyFunc (VClosure params body closureEnv) args scope =
    if length params /= length args
    then Left "Incorrect number of arguments"
    else let newEnv = insertParams params args (createScope closureEnv) in
        eval newEnv body
    where
        insertParams :: [String] -> [Value] -> Scope -> Scope
        insertParams ps vs env = env { scopeVars = foldl (\m (p, v) -> M.insert p v m) (scopeVars env) (zip ps vs) }

eval :: Scope -> Expr -> Either String Value
eval scope (Num n) = Right (VNum n)
eval scope (Bool b) = Right (VBool b)
eval scope (Symbol s) =
    case searchDef s scope of
        Just (Left v)  -> Right v
        Just (Right _) -> Left ("Expected a value, but found a macro: " ++ s)
        Nothing        -> Left ("Undefined symbol: " ++ s)
eval scope (Quote sexpr) = Right (sexprToValue sexpr)
eval scope (If cond thenExpr elseExpr) = do
    condVal <- eval scope cond
    case condVal of
        VBool True  -> eval scope thenExpr
        VBool False -> eval scope elseExpr
        _           -> Left "Condition must evaluate to a boolean"
eval scope (Lambda params body) = Right (VClosure params body scope)
eval scope (App funcExpr argExprs) = do
    funcVal <- eval scope funcExpr
    argVals <- mapM (eval scope) argExprs
    applyFunc funcVal argVals scope

evalTop :: Scope -> Top -> Either String (Value, Scope)
evalTop scope (Define name expr) = do
    val <- eval scope expr
    let newScope = scope { scopeVars = M.insert name val (scopeVars scope) }
    Right (val, newScope)
evalTop scope (DefMacro name macro) = Right (VNull, scope) -- マクロはSExpr → Exprの変換時に展開されるので、ここでは何もしない
evalTop scope (TopExpr expr) = do
    val <- eval scope expr
    Right (val, scope)

runAST :: Scope -> [Top] -> Either String Value
runAST scope [] = Right VNull
runAST scope (top:tops) = do
    ans <- evalTop scope top
    if length tops == 0
    then Right (fst ans)
    else let newScope = snd ans in
        runAST newScope tops

runProgram :: String -> Either String Value
runProgram input = do
    let parsed = apply (many top) $ trace ("\n" ++ show input) input
    case trace ("parsed: " ++ show parsed) parsed of
        [] -> Left "Parse error"
        (tops, ""):_ -> do
            (tops', newScope) <- sprogramToProgram globalScope tops
            runAST newScope tops'
        otherwise -> Left "Parse error: unexpected input"
