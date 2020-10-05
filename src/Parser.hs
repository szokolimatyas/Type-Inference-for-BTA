module Parser where

import qualified Data.Map as Map
import Data.Functor
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language

import Common


languageDef =
     emptyDef { Tok.commentStart    = "/*"
              , Tok.commentEnd    = "*/"
              , Tok.commentLine     = "//"
              , Tok.identStart      = letter
              , Tok.identLetter     = alphaNum
              , Tok.reservedNames   = [ "if"
                                        , "then"
                                        , "else"
                                        , "fix"
                                        , "let"
                                        , "in"
                                        , "true"
                                        , "false"
                                        , "not"
                                        , "and"
                                        , "or"
                                        , "Int"
                                        , "Bool"
                                        , "Untyped"
                                        , "forall"
                                        , "Expr"
                                        , "Env"
                                        ,"_"
                                        ]
              , Tok.reservedOpNames = ["+", "-", "*", "/", ":=", "==", "::","!@!"
                                        , ">", "<", "<=", ">=","!=","and", "or", "not",  "@"
                                        ]
              }

lexer = Tok.makeTokenParser languageDef


reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
parens = Tok.parens lexer
integer = Tok.integer lexer
comma = Tok.comma lexer
braces = Tok.braces lexer
dot = Tok.dot lexer

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

variable :: Parser Expr
variable = EVar_ <$> braces identifier
    <|> EVar <$> identifier

lit :: Parser Expr
lit = integer $> EConst
    <|> reserved "true" $> EConst
    <|> reserved "false" $> EConst

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many1 identifier
    reservedOp "."
    body <- expr
    return $ foldr EAbs body args
{-
letExpr :: Parser Expr
letExpr = do 
    reserved "let"
    name <- identifier
    reservedOp "="
    ex <- expr -- term instead?
    reserved "in"
    ELet name ex <$> expr
-}
ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    cond <- expr
    reserved "then"
    ex1 <- expr
    reserved "else"
    ex2 <-  expr
    return $ EIf cond ex1 ex2

fixExpr :: Parser Expr
fixExpr = do
    reserved "fix"
    expr <- parens expr
    return $ EFix expr

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> lambda
    <|> lit
    -- <|> letExpr
    <|> ifExpr
    <|> fixExpr
{-
operators = [ [Infix (reservedOp "*"    >> return (\l r -> EBinOp (l,TInt) (r,TInt) TInt))   AssocLeft,  
    Infix (reservedOp "/"    >> return (\l r -> EBinOp (l,TInt) (r,TInt) TInt))   AssocLeft]
 , [Infix (reservedOp "+"    >> return (\l r -> EBinOp (l,TInt) (r,TInt) TInt))  AssocLeft,
    Infix (reservedOp "-"    >> return (\l r -> EBinOp (l,TInt) (r,TInt) TInt)) AssocLeft]
-- , [Prefix (reservedOp "not" >> _)            ]
 , [Infix (reservedOp "<="   >> return (\l r -> EBinOp (l,TInt) (r,TInt) TBool))   AssocLeft,
    Infix (reservedOp ">="   >> return (\l r -> EBinOp (l,TInt) (r,TInt) TBool))   AssocLeft,
    Infix (reservedOp "<"   >> return (\l r -> EBinOp (l,TInt) (r,TInt) TBool))   AssocLeft,
    Infix (reservedOp ">"   >> return (\l r -> EBinOp (l,TInt) (r,TInt) TBool))   AssocLeft]
 , [Infix (reservedOp "=="   >> return (\l r -> EBinOp (l,TInt) (r,TInt) TBool))    AssocLeft]
 , [Infix (reservedOp "and"  >> return (\l r -> EBinOp (l,TBool) (r,TBool) TBool))   AssocLeft,
    Infix (reservedOp "or"   >> return (\l r -> EBinOp (l,TBool) (r,TBool) TBool))    AssocLeft] 
 , [Infix (reservedOp "@" >> return EApp) AssocLeft]
 ]-}
operators = [[Infix (reservedOp "@" >> return EApp) AssocLeft,
            Infix (reservedOp "!@!" >> return EApp_) AssocLeft]]

expr :: Parser Expr
expr = buildExpressionParser operators term

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

--parseTVar :: Parser

parseType' :: Parser Type
parseType' = parens parseType 
            <|> (integer >>= (return . TVar . fromInteger )) 
            <|> reserved "Int" $> TBase []
            <|> reserved "Bool" $> TBase []
            <|> reserved "Untyped" $> TUntyped

-- right associative
parseType :: Parser Type 
parseType = do
    ts <- sepBy1 parseType' (reservedOp "->")
    return (foldr1 (\t t'-> TArrow t t' []) ts)

parseEnv :: Parser TypeEnv
parseEnv = do
    ds <- sepBy pair comma
    return $ TypeEnv $ Map.fromList ds
    where
        pair = do
            n <- identifier
            reservedOp "::"
            s  <- parseType
            return (n,s)

parseEnvTest :: String -> Either ParseError TypeEnv
parseEnvTest = parse (contents parseEnv) "<stdin>"


parseTypeTest :: String -> Either ParseError Type
parseTypeTest = parse (contents parseType) "<stdin>"


