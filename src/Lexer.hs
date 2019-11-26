module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char (oneOf, letter)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser tiger
    where
        ops = ["+", "-", "*", "/", "=", "<>", "<", "<=", ">", ">=", "&", "|", ":="]
        reserved = ["while", "for", "to", "break", "let", "in", "end", "function", "var", "type", "array", "if", "then", "else", "do", "of", "nil"]
        tiger = emptyDef {
            Tok.commentLine     = "",       -- No single line comment
            Tok.commentStart    = "/*",     -- C syntax multi line comments
            Tok.commentEnd      = "*/",
            Tok.nestedComments  = True,     -- Comment might be nested
            Tok.identStart      = letter,   -- Identifier start with a letter
            Tok.opStart         = oneOf ",:;()[]{}.+-*/=<>&|",
            Tok.opLetter        = oneOf ",:;()[]{}.+-*/=<>&|",
            Tok.reservedNames   = reserved,
            Tok.reservedOpNames = ops
            }

integer :: Parser Integer
integer = Tok.integer lexer

string :: Parser String
string = Tok.stringLiteral lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

whiteSpace = Tok.whiteSpace lexer
