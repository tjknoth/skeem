module Eval where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex)
import Control.Monad.Error

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

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Type of things that throw errors
type ThrowsError = Either LispError

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- Apply some function to arguments
apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

-- Binary numeric operators
primitives :: [(String, [LispVal] -> LispVal)]
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
              ("string->symbol", unaryOp strToSym)]

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
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number . foldl1 op $ fmap unpackNum params

-- Unary operators
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [param] = op param

-- Extract number from LispVal
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
