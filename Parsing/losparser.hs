-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

--module Parsing.HashParser
--where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, spaces)
import Text.Parsec (parse, ParseError)
import Text.Parsec.Combinator (many1)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), Applicative, many, (<|>))
import Data.Char (digitToInt)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Char (letter)
import Text.Parsec (try)
import qualified Data.Map as M
import Control.Monad (join)
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Combinator (chainl1)
import Text.Parsec.Expr

import Control.Monad (void)

err = "An error has occurred"

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse p err

parseDigit :: Parser Int
parseDigit = digitToInt <$> digit

number :: Parser Int
number = read <$> many1 digit

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

negative :: Parser Int
negative = read <$> char '-' <:> many1 digit

integer :: Parser Int
integer = number <|> negative

token :: Parser a -> Parser a
token = (<* spaces)

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

integer' = token integer

----------------------
symbol :: Char -> Parser Char
symbol = token . char

data MyList = WordList { text :: [String] }
            | IntList  { ints :: [Int] } deriving (Eq, Show)


-- Some helper functions
brackets :: Parser a -> Parser a
brackets = (<* symbol ']') . (symbol '[' *>)

elements :: Parser a -> Parser [a]
elements = flip sepBy1 (symbol ',')

listOf :: Parser a -> Parser [a]
listOf = brackets . elements . token

list :: Parser MyList
list = intlist <|> strlist
    where intlist  = IntList  <$> listOf integer'
          strlist  = WordList <$> listOf (many1 letter)
          
------------
list' :: Parser MyList
list' = try intlist <|> strlist
    where intlist  = IntList  <$> listOf integer'
          strlist  = WordList <$> listOf (many1 letter)

---------------
data Expression = Val   Int
                | Var   String
                | Plus  Expression Expression
                | Minus Expression Expression
                  deriving Show

data Assignment = Assignment String Expression deriving Show

type Program = [Assignment]
                             
type VarTable = M.Map String Int

eval :: VarTable -> Expression -> Maybe Int
eval vt e = case e of (Val   v)   -> Just v
                      (Var   v)   -> M.lookup v vt
                      (Plus  a b) -> (+) <$> eval vt a <*> eval vt b
                      (Minus a b) -> (-) <$> eval vt a <*> eval vt b
                      
assign :: Assignment -> VarTable -> Maybe VarTable
assign (Assignment v e) vt = insert v vt <$> eval vt e
    where insert k = flip (M.insert k)
    
run :: Program -> Maybe VarTable
run = run' (Just M.empty)
    where run' vt []     = vt
          run' vt (a:as) = run' (join $ assign a <$> vt) as

--------------------
variable :: Parser String
variable = token $ letter <:> many alphaNum


operator :: Parser (Expression -> Expression -> Expression)
operator = plus <|> minus
    where plus  = symbol '+' *> return Plus
          minus = symbol '-' *> return Minus

expression :: Parser Expression
expression = term `chainl1` operator
    where term = val <|> var
          var  = Var <$> variable
          val  = Val <$> integer'



-- Both of our operators have the same priority
table = [[binary '+' Plus, binary '-' Minus]]
        where binary name f = Infix (f <$ (symbol name)) AssocLeft
        
expression' :: Parser Expression
expression' = buildExpressionParser table other
    where other = var <|> val
          var   = Var <$> variable
          val   = Val <$> integer'



assignment :: Parser Assignment
assignment = Assignment <$> variable <*> (symbol '=' *> expression)

program :: Parser Program
program = many1 assignment

interpret :: String -> Maybe VarTable
interpret s = case prog of Left  e -> Nothing
                           Right p -> run   p
    where prog = betterParse program s

-----------------------------------------------------
-----------------------------------------------------
mToken :: Parser a -> Parser a
mToken p = do
    x <- p
    void $ spaces -- Spaces are read, but they are not stored. We just "skip" them.
    return x
    
mSymbol :: Char -> Parser Char
mSymbol = mToken . char

mNumber :: Parser Int
mNumber = do
    num <- many1 digit
    return . read $ num
    
mNegative :: Parser Int
mNegative = do
    pref <- char '-'
    num  <- many1 digit
    return . read $ pref : num

mInteger :: Parser Int
mInteger = mToken p
    where p = mNumber <|> mNegative

mVariable :: Parser String
mVariable = do
    h <- letter
    t <- many alphaNum
    mToken . return $ h : t
    
mTable = [[binary '+' Plus, binary '-' Minus]]
    where binary sym f = Infix (mkParser sym f) AssocLeft
          mkParser s f = do
              void $ mSymbol s -- read +/-, but discard it. It HAS to be there but is not used.
              return f

mExpression :: Parser Expression
mExpression = buildExpressionParser mTable other
    where other = var <|> val
          var   = do
              v <- mVariable
              return $ Var v
          val   = do
              v <- mInteger
              return $ Val v
              
mAssignment :: Parser Assignment
mAssignment = do
    name <- mVariable
    void $ mSymbol '='
    expr <- mExpression
    return $ Assignment name expr
    
mProgram :: Parser Program
mProgram = many1 mAssignment

mInterpret :: String -> Maybe VarTable
mInterpret s = case prog of Right p -> run p
                            _       -> Nothing
            where prog = betterParse mProgram s










