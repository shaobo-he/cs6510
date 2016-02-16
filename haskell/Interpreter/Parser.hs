module Interpreter.Parser(parseLisp) where

import Text.ParserCombinators.Parsec hiding (spaces)

-- import System.Environment
import Interpreter.Types
import Interpreter.Desugar

parseExpr :: Parser ExprS
parseExpr =  try parseId
         <|> try parseLambda
         <|> try parseThunk
         <|> try parseLet
         <|> try parseForce
--         <|> try parseString
         <|> try parseNum
         <|> try parseErr
         <|> try parsePlus
         <|> try parseMult
         <|> try parseEq
         <|> try parseIf
         <|> try parseBool
         <|> try parseApp

symbol :: Parser Char
symbol = oneOf "!#$%&|:<=>?@^_~"

parseIdent :: Parser String
parseIdent = do
               first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               return $ first:rest

parseId :: Parser ExprS
parseId = do
            id <- parseIdent
            return $ IdS id
              
spaces1 :: Parser ()
spaces1 = skipMany1 space

spaces :: Parser ()
spaces = skipMany space

parseString :: Parser String
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ x

parseErr :: Parser ExprS
parseErr = do
            begin >> string "error"
            msg <- spaces >> parseString
            end
            return $ ErrS msg
            
begin = spaces >> char '(' >> spaces
end = spaces >> char ')' >> spaces

parseGroup :: Parser ExprS
parseGroup = do
               begin
               e <- parseExpr
               end
               return $ e

parseLambda :: Parser ExprS
parseLambda = do
                begin >> string "lambda" 
                spaces >> char '(' >> spaces
                names_f <- parseIdent
                names_r <- many (spaces1 >> parseIdent)
                char ')'
                body    <- spaces >> parseExpr
                end
                return $ LamS (names_f:names_r) body

parsePair :: Parser (String, ExprS)
parsePair = do
              char '[' >> spaces
              name <- spaces >> parseIdent
              exp <- spaces >> parseExpr
              spaces >> char ']' >> spaces
              return $ (name, exp)

parseLet :: Parser ExprS
parseLet = do
             begin >> string "let"
             spaces >> char '('
             pairs_f <- spaces >> parsePair
             pairs_r <- many (spaces >> parsePair)
             char ')' >> spaces
             body <- parseExpr
             end
             return $ LetS (pairs_f:pairs_r) body

parseApp :: Parser ExprS
parseApp = do
             begin
             fun <- spaces >> parseExpr
             args_f <- spaces >> parseExpr
             args_r <- many (spaces >> parseExpr)
             end
             return $ AppS fun (args_f:args_r)
             
floating :: Parser String
floating = do
             whole <- many1 digit
             char '.'
             frac <- many1 digit
             return $ whole ++ ['.'] ++ frac

int :: Parser String
int = do
        n <- many1 digit
        return n

numParse :: Parser String
numParse =  try floating
        <|> int

parseNum :: Parser ExprS
parseNum = do
             num <- numParse
             return $ NumS $ ((read num) :: Double)
             
parseBool :: Parser ExprS
parseBool = do
              p <- string "true" <|> string "false"
              return $ BoolS (p == "true")

parseIf :: Parser ExprS
parseIf = do
            begin >> string "if"
            c <- spaces >> parseExpr
            t <- spaces >> parseExpr
            e <- spaces >> parseExpr
            end
            return $ IfS c t e

parseBinop :: Char -> (ExprS -> ExprS -> ExprS) -> Parser ExprS
parseBinop op cnstr = do
                        begin >> char op
                        l <- spaces >> parseExpr
                        r <- spaces >> parseExpr
                        end
                        return $ cnstr l r

parsePlus :: Parser ExprS
parsePlus = parseBinop '+' PlusS

parseMult :: Parser ExprS
parseMult = parseBinop '*' MultS

parseEq :: Parser ExprS
parseEq = parseBinop '=' EqS

parseThunk :: Parser ExprS
parseThunk = do
               begin >> string "delay"
               body <- spaces1 >> parseExpr
               end
               return $ ThunkS body

parseForce :: Parser ExprS
parseForce = do
               begin >> string "force"
               body <- spaces1 >> parseExpr
               end
               return $ ForceS body

readExpr :: String -> String
readExpr input = case parse parseExpr "lang" input of
  Left err -> "No match: " ++ show err
  Right v -> "Found value: " ++ show v
  
parseLisp :: String -> Either ParseError ExprS
parseLisp input = parse parseExpr "" input
  
