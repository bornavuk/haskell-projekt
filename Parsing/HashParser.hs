-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

module HashParser
(parsiraj)
where

{-
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), Applicative, many, (<|>))
import Data.Char (digitToInt)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Char (letter)
import Text.Parsec.Char (digit, char, spaces)
import qualified Data.Map as M
import Control.Monad (join, void)
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Combinator (chainl1)
import Text.Parsec.Expr
-}
import Control.Applicative ((<$>), many, (<|>), (<*), (*>), (<*>))
import Control.Monad (void)
import Text.Parsec (parse, try, ParseError)
import Text.Parsec.Char (satisfy, char, spaces)
import Text.Parsec.Combinator (many1, eof, choice)
import Text.Parsec.String (Parser)
import Expressions
import Data.Char (isAlpha, isDigit)

-- bottom level expression parsing
parseExpr :: Parser Expr
parseExpr = do
    spaces
    r <- try parseVar <|> parseStr
    spaces
    return r

parseVar :: Parser Expr
parseVar = do
    char '$'
    var <- many1 (satisfy (\x -> isAlpha x || isDigit x || x `elem` "/_.,:-\\"))
    return $ Var var

parseStr :: Parser Expr
parseStr = do
    spaces
    str <- many1 (satisfy (\x -> isAlpha x || isDigit x || x `elem` "/_.,:-\\"))
    spaces
    return $ Str str

-- Cmd parsing
parseCmd1 :: Parser Cmd
parseCmd1 = do
    name <- parseExpr
    args <- many parseExpr
    return $ Cmd name args

parseAssign :: Parser Cmd
parseAssign = do
    var <- parseStr
    char '='
    val <- parseExpr
    return $ Assign var val

parseCmd :: Parser Cmd
parseCmd = do
    r <- try parseAssign <|> parseCmd1
    char ';'
    return r

-----------
-- parses a given keyword and ignores it
keyword :: String -> Parser ()
keyword [] = return ()
keyword (s:ss) = do
    char s
    keyword ss

-- Comp parsing
parseComp :: Parser Comp
parseComp = try parseCEQ <|>
            try parseCNE <|>
            try parseCGE <|>
            try parseCGT <|>
            try parseCLE <|>
            try parseCLT <|>
            parseCLI

-- parsing binary operator with given keyword
parseBin :: (Expr -> Expr -> Comp) -> String -> Parser Comp
parseBin op k = (op <$> parseExpr) <*> (keyword k *> parseExpr)

parseCEQ :: Parser Comp
parseCEQ = parseBin CEQ "=="

parseCNE :: Parser Comp
parseCNE = parseBin CNE "/="

parseCGE :: Parser Comp
parseCGE = parseBin CGE ">="

parseCGT :: Parser Comp
parseCGT = parseBin CGT ">"

parseCLE :: Parser Comp
parseCLE = parseBin CLE "<="

parseCLT :: Parser Comp
parseCLT = parseBin CLT "<"

parseCLI :: Parser Comp
parseCLI = do
    r <- parseExpr
    return $ CLI r


-- Pred parsing
parsePred' :: Parser Pred
parsePred' = do
    spaces
    comp <- parseComp
    spaces
    return $ Pred comp

parseAnd :: Parser Pred
parseAnd = do
    spaces
    l <- parsePredMono
    spaces
    char '&'
    spaces
    r <- parsePredMono
    spaces
    return $ And l r

parseOr :: Parser Pred
parseOr = do
    spaces
    l <- parsePredMono
    spaces
    char '|'
    spaces
    r <- parsePredMono
    spaces
    return $ Or l r

parseNot :: Parser Pred
parseNot = do
    spaces
    char '!'
    spaces
    pred <- parsePred
    spaces
    return $ Not pred

parseParens :: Parser Pred
parseParens = do
    spaces
    char '('
    spaces
    pred <- parsePred
    spaces
    char ')'
    spaces
    return $ Parens pred

parsePredMono :: Parser Pred
parsePredMono = try parseParens <|>
                try parseNot <|>
                parsePred'

parsePred :: Parser Pred
parsePred = try parseAnd <|> 
            try parseOr <|> parsePredMono


-- conditional parsing
parseIfElse :: Parser Conditional
parseIfElse = do
    (If cond cthen) <- parseIf
    spaces
    keyword "else"
    spaces
    char '{'
    celse <- many parseCmd
    char '}'
    spaces
    return $ IfElse cond cthen celse

parseIf :: Parser Conditional
parseIf = do
    spaces
    keyword "if"
    spaces
    cond <- parsePred
    spaces
    keyword "then"
    spaces
    char '{'
    cthen <- many parseCmd
    char '}'
    spaces
    return $ If cond cthen

parseConditional :: Parser TLExpr
parseConditional = do
    cnd <- try parseIfElse <|> parseIf
    return $ TLCnd cnd

-- top level parsing functions
parseCmdTL :: Parser TLExpr
parseCmdTL = do
    cmd <- parseCmd
    return $ TLCmd cmd

parseTLExpr :: Parser TLExpr
parseTLExpr = try parseConditional <|> parseCmdTL

-- exported parsing function
parsiraj :: String -> [TLExpr]
parsiraj str = do
    case parseWithEof (many parseTLExpr) str of
        Left x -> error "dogodila se sintakticka pogreska"
        Right x -> x





-- helper functions for exported parsing function
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (doKraja p) "Nekaj ne valja (u parseru)"

doKraja :: Parser a -> Parser a
doKraja p = do
    r <- p
    eof
    return r
