{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import System.Environment
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

-- Data type supporting equality
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- Data type for Lisp values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool

-- make vals showable
instance Show LispVal where show = showVal

-- Show values
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- Data type for different errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- Type of things that throw errors
type ThrowsError = Either LispError

-- Error helpers
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return  val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True  -> eval conseq
    otherwise  -> throwError $ TypeMismatch "boolean" result
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Apply some function to arguments
apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function args" f)
                     ($ args)
                     (lookup f primitives)

-- List of operators
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("not", unaryOp not'),
              ("boolean?", unaryOp boolP),
              ("list?", unaryOp listP),
              ("symbol?", unaryOp symbolP),
              ("char?", unaryOp charP),
              ("string?", unaryOp stringP),
              ("symbol->string", unaryOp symToStr),
              ("string->symbol", unaryOp strToSym),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("string?", isString),
              ("string-length", stringLength)]

-- Unary operator helpers
not' :: LispVal -> LispVal
not' (Bool x) = Bool . not $ x
not' _        = Bool False

boolP :: LispVal -> LispVal
boolP (Bool _) = Bool True
boolP _        = Bool False

listP :: LispVal -> LispVal
listP (List _)         = Bool True
listP (DottedList _ _) = Bool True
listP _                = Bool False

symbolP :: LispVal -> LispVal
symbolP (Atom _) = Bool True
symbolP _        = Bool False

charP :: LispVal -> LispVal
charP (Char _) = Bool True
charP _        = Bool False

stringP :: LispVal -> LispVal
stringP (String _) = Bool True
stringP _          = Bool False

symToStr :: LispVal -> LispVal
symToStr (Atom s) = String s
symToStr _        = error "Expected an Atom"

strToSym :: LispVal -> LispVal
strToSym (String s) = Atom s
strToSym _          = error "Expected a string"

-- Apply binary operation to list of parameters
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- Unary operators
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op []      = throwError $ NumArgs 1 []
unaryOp op [param] = return $ op param

-- General binary op returning bool
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if (length args) /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

-- Extract number from LispVal
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError . TypeMismatch "number" $ String n
                             else return . fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- Head of list
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badarg]              = throwError $ TypeMismatch "pair" badarg
car badArgList            = throwError $ NumArgs 1 badArgList

-- tail of list
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- Append two lists
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- check for equality - both eq? and eqv? in scheme
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return . Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return . Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return . Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return .Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return . Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- equal? - testing for weakly-typed equivalence

-- Helper to unpack lispvals
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

-- Checks for weakly-typed equality
equal :: [LispVal] -> ThrowsError LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List xs), (List ys)] = return . Bool $ (length xs == length ys) &&
                                               (all equalPair $ zip xs ys)
      where equalPair (x,y) = case equal [x,y] of
                                Left err -> False
                                Right (Bool val) -> val
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- String functions
isString :: [LispVal] -> ThrowsError LispVal
isString [(String s)] = return $ Bool True
isString [x]          = return $ Bool False
isString badArgs    = throwError $ NumArgs 1 badArgs

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [(String s)] = (return . Number) . toInteger $ length s
stringLength [x]          = throwError $ TypeMismatch "string" x
stringLength badArgs      = throwError $ NumArgs 1 badArgs
