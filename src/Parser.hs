module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Map.Strict as M

import Lexer
import qualified Syntax as S

getPos = do
    sp <- getPosition
    return $ S.Pos (sourceLine sp) (sourceColumn sp)

decs :: Parser S.Dec
decs = tydec
    <|> vardec
    <|> fundec

tydec :: Parser S.Dec
tydec = do
    p <- getPos
    reserved "type"
    name <- identifier
    reservedOp "="
    tydef <- ty
    return $ S.Tydec name tydef p

ty :: Parser S.Ty
ty = try namety
  <|> try recordty
  <|> arrayty
  where
    namety = do
        p <- getPos
        name <- identifier
        return $ S.NameTy name p
    recordty = do
        p <- getPos
        field <- braces $ commaSep  tyfield
        return $ S.RecordTy field p
    arrayty = do
        p <- getPos
        reserved "array"
        reserved "of"
        name <- identifier
        return $ S.ArrayTy name p

tyfield :: Parser S.Field
tyfield = do
    p <- getPos
    name <- identifier
    reservedOp ":"
    ty <- identifier
    return $ S.Field name ty p

vardec :: Parser S.Dec
vardec = do
    p <- getPos
    reserved "var"
    name <- identifier
    ty <- optionMaybe varty
    reservedOp ":="
    init <- expr
    return $ S.Vardec name ty init p
    where
        varty = do
            reservedOp ":"
            ty <- identifier
            return ty


fundec :: Parser S.Dec
fundec = do
    p <- getPos
    reserved "function"
    name <- identifier
    args <- parens $ commaSep tyfield
    ty <- optionMaybe retype
    reservedOp "="
    body <- expr
    return $ S.Fundec name args ty body p
    where
        retype = do
            reservedOp ":"
            ty <- identifier
            return ty

expr :: Parser S.Exp
expr = whiteSpace >> parsexpr

expr' :: Parser S.Exp
expr' = try callexpr
    <|> try letexpr
    <|> try ifexpr
    <|> try whileexpr
    <|> try forexpr
    <|> try recordexpr
    <|> try arrayexpr
    <|> try assignexpr
    <|> try varexpr
    <|> try breakexp
    <|> try nilexp
    <|> intexp
    <|> stringexp
    <|> seqexpr

var :: Parser S.Var
var = do
    p <- getPos
    id <- identifier
    let v = S.SimpleVar id p
    (var' v)

var' :: S.Var -> Parser S.Var
var' v = (do
            p <- getPos
            dot
            field <- identifier
            var' $ S.FieldVar v field p)
         <|>
         (do
            p <- getPos
            sub <- brackets expr
            var' $ S.SubscriptVar v sub p)
         <|> return v

varexpr :: Parser S.Exp
varexpr = do
    p <- getPos
    v <- var
    return $ S.VarExp v p

breakexp :: Parser S.Exp
breakexp = do
    p <- getPos
    reserved "break"
    return $ S.BreakExp p

nilexp :: Parser S.Exp
nilexp = do
    p <- getPos
    reserved "nil"
    return $ S.NilExp p

intexp :: Parser S.Exp
intexp = do
    p <- getPos
    v <- integer
    return $ S.IntExp v p

stringexp :: Parser S.Exp
stringexp = do
    p <- getPos
    s <- Lexer.string
    return $ S.StringExp s p


recordexpr :: Parser S.Exp
recordexpr = do
    p <- getPos
    rectype <- identifier
    fields <- braces $ commaSep field
    return $ S.RecordExp rectype (M.fromList fields) p
    where
        field = do
            id <- identifier
            reservedOp "="
            value <- expr
            return (id, value)

arrayexpr :: Parser S.Exp
arrayexpr = do
    p <- getPos
    arrtype <- identifier
    size <- brackets expr
    reserved "of"
    init <- expr
    return $ S.ArrayExp arrtype size init p

assignexpr :: Parser S.Exp
assignexpr = do
    p <- getPos
    v <- var
    reservedOp ":="
    e <- expr
    return $ S.AssignExp v e p

callexpr :: Parser S.Exp
callexpr = do
    p <- getPos
    name <- identifier
    args <- parens $ commaSep expr
    return $ S.CallExp name args p

letexpr :: Parser S.Exp
letexpr = do
    p <- getPos
    reserved "let"
    decs <- many $ decs
    reserved "in"
    body <- S.SeqExp <$> (semiSep $ expr)
    reserved "end"
    return $ S.LetExp decs body p

ifexpr :: Parser S.Exp
ifexpr = do
    p <- getPos
    reserved "if"
    cond <- expr
    reserved "then"
    true <- expr
    (do
        reserved "else"
        false <- expr
        return (S.IfExp cond true (Just false) p)
        <|>
        return (S.IfExp cond true Nothing p))

whileexpr :: Parser S.Exp
whileexpr = do
    p <- getPos
    reserved "while"
    cond <- expr
    reserved "do"
    body <- expr
    return $ S.WhileExp cond body p

forexpr :: Parser S.Exp
forexpr = do
    p <- getPos
    reserved "for"
    v <- var
    reservedOp ":="
    begin <- expr
    reserved "to"
    end <- expr
    reserved "do"
    body <- expr
    return $ S.ForExp v begin end body p

seqexpr :: Parser S.Exp
seqexpr = do
    es <- parens $ semiSep expr
    return $ S.SeqExp es


binary name op assoc pos  = Ex.Infix (reservedOp name >> return (\er el ->S.OpExp op er el pos)) assoc
prefix name op pos      = Ex.Prefix (reservedOp name >> return (\e -> S.OpExp op zero e pos))
    where
        zero = S.IntExp 0 (S.Pos 0 0)
amperCmp pos = Ex.Infix (reservedOp "&" >> return (\e1 e2 -> S.IfExp e1 e2 (Just false) pos)) Ex.AssocNone
    where
        false = S.IntExp 0 (S.Pos 0 0)
pipeCmp pos = Ex.Infix (reservedOp "|" >> return (\e1 e2 -> S.IfExp e1 true (Just e2) pos)) Ex.AssocNone
    where
        true = S.IntExp 0 (S.Pos 0 0)

table p = [[prefix "-"    S.MinusOp p],
          [binary "*"    S.TimesOp   Ex.AssocLeft p,
           binary "/"    S.DivOp     Ex.AssocLeft p],
          [binary "+"    S.PlusOp    Ex.AssocLeft p,
           binary "-"    S.MinusOp   Ex.AssocLeft p],
          [binary "="    S.EqOp      Ex.AssocNone p,
           binary "<>"   S.NeqOp     Ex.AssocNone p,
           binary "<"    S.LtOp      Ex.AssocNone p,
           binary "<="   S.LeOp      Ex.AssocNone p,
           binary ">"    S.GtOp      Ex.AssocNone p,
           binary ">="   S.GeOp      Ex.AssocNone p],
          [amperCmp p],
           [pipeCmp p]]

parsexpr :: Parser S.Exp
parsexpr = do
    p <- getPos
    try $ Ex.buildExpressionParser (table p) expr'
