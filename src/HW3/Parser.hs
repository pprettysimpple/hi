{-# LANGUAGE TypeFamilies #-}
module HW3.Parser (
        parse
) where

import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Data.List.NonEmpty hiding(tail, head)
-- import Data.Text hiding(tail, head, foldl, null)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific(toRationalRepetend)
import Control.Monad.Combinators.Expr

import HW3.Base (
        HiFun(..),
        HiValue(..),
        HiExpr(..),
        HiError(..),
        HiAction (..))
import qualified Control.Monad
import qualified Data.Functor.Identity
import Control.Monad (void)
import Data.String (IsString(fromString))
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Sequence as Seq
import Data.Maybe (maybeToList)
import Data.List (intercalate)

type Parser = Parsec Void String

lexe :: Parser a -> Parser a
lexe = L.lexeme space

-- parse only boolean
bool :: Parser HiExpr
bool = lexe $
    (HiExprValue . HiValueBool . (const False)) <$> string "false"
    <|> (HiExprValue . HiValueBool . (const True)) <$> string "true"

-- parse number
rational :: Parser HiExpr
rational = lexe $ ((HiExprValue . HiValueNumber . toRational) <$> (L.signed space L.scientific))

-- parse string
hiString :: Parser HiExpr
hiString = lexe $ (HiExprValue . HiValueString . fromString) <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- parse null
hiNull :: Parser HiExpr
hiNull = lexe $ (HiExprValue . const HiValueNull) <$> string "null"

-- parse list [A, B, C, ...] form
hiList :: Parser HiExpr
hiList = do
    rejectFollowing "[" "#" -- hit for bytes that are in same brackets
    h <- optional exprInfix
    t <- many $ (lexe $ char ',') >> exprInfix
    (lexe $ char ']')
    return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) (maybeToList h ++ t)

-- parser for one raw byte like "fF" of "0A"
rawByte :: Parser Word8
rawByte = do
    f <- hexDigitChar
    s <- hexDigitChar
    return $ read $ "0x" ++ [f, s]

-- since they have common prefix, lets parse them together in one function
hiBytes :: Parser HiExpr
hiBytes = do
    (lexe $ string "[#")
    bts <- rawByte `sepEndBy` space1
    (lexe $ string "#]")
    return $ HiExprValue $ HiValueBytes $ BS.pack bts

-- cwd keyword parser
parseCwd :: Parser HiExpr
parseCwd = (HiExprValue . HiValueAction . (const HiActionCwd)) <$> (lexe $ string "cwd")

-- "now" keyword parser
parseNow :: Parser HiExpr
parseNow = (HiExprValue . HiValueAction . (const HiActionNow)) <$> (lexe $ string "now")

-- parse dictionary
parseDict :: Parser HiExpr
parseDict = do
    (lexe $ string "{")
    let onePair = do
                    key <- exprInfix
                    (lexe $ string ":")
                    val <- exprInfix
                    return (key, val)
    fst <- optional $ onePair
    rst <- many $ do
        lexe $ char ','
        onePair
    (lexe $ string "}")
    return $ HiExprDict $ rst ++ maybeToList fst

-- helper for functions parsers
defineParserFunc :: HiFun -> Parser HiExpr
defineParserFunc fun = (HiExprValue . HiValueFunction . (const fun)) <$> (lexe $ string $ show fun)

-- parse only function name
atom :: Parser HiExpr
atom = (defineParserFunc HiFunDiv
        <|> defineParserFunc HiFunMul
        <|> defineParserFunc HiFunAdd
        <|> defineParserFunc HiFunSub
        <|> defineParserFunc HiFunLessThan
        <|> defineParserFunc HiFunGreaterThan
        <|> defineParserFunc HiFunEquals
        <|> defineParserFunc HiFunNotGreaterThan
        <|> defineParserFunc HiFunNotLessThan
        <|> defineParserFunc HiFunNotEquals
        <|> defineParserFunc HiFunIf
        <|> defineParserFunc HiFunAnd
        <|> defineParserFunc HiFunOr
        <|> defineParserFunc HiFunLength
        <|> defineParserFunc HiFunToUpper
        <|> defineParserFunc HiFunToLower
        <|> defineParserFunc HiFunReverse
        <|> defineParserFunc HiFunTrim
        <|> defineParserFunc HiFunList
        <|> defineParserFunc HiFunRange
        <|> defineParserFunc HiFunFold
        <|> defineParserFunc HiFunPackBytes
        <|> defineParserFunc HiFunUnpackBytes
        <|> defineParserFunc HiFunZip
        <|> defineParserFunc HiFunUnzip
        <|> defineParserFunc HiFunEncodeUtf8
        <|> defineParserFunc HiFunDecodeUtf8
        <|> defineParserFunc HiFunSerialise
        <|> defineParserFunc HiFunDeserialise
        <|> defineParserFunc HiFunRead
        <|> defineParserFunc HiFunWrite
        <|> defineParserFunc HiFunMkDir
        <|> defineParserFunc HiFunChDir
        <|> defineParserFunc HiFunParseTime
        <|> defineParserFunc HiFunRand
        <|> defineParserFunc HiFunEcho
        <|> defineParserFunc HiFunCount
        <|> defineParserFunc HiFunKeys
        <|> defineParserFunc HiFunValues
        <|> defineParserFunc HiFunInvert
        <|> parseCwd <|> parseNow
        <|> rational <|> bool <|> hiNull
        <|> hiString <|> hiList <|> hiBytes
        <|> parseDict
        <|> defineParserFunc HiFunNot)

-- parse all arguments divided by comma (important) inside brackets
functionArgs :: Parser [HiExpr]
functionArgs = do
    (lexe $ char '(')
    h <- optional exprInfix
    t <- many $ (lexe $ char ',') >> exprInfix
    (lexe $ char ')')
    return $ (maybeToList h ++ t)

-- parse ".foo" -> [str(foo)] for easy foldl in exprTerm
parseDotAccess :: Parser [HiExpr]
parseDotAccess = do
    (lexe $ char '.')
    s <- T.pack <$> (intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-')
    return $ [HiExprValue $ HiValueString s]

-- parse rational <|> bool <|> functionCall <|> null <|> etc...
exprTerm :: Parser HiExpr
exprTerm = do
    name <- (do -- there might be not only func name but also rational or boolean or ... or expr in brackets
        lexe $ char '('
        e <- exprInfix
        lexe $ char ')'
        return e) <|> atom
    argss <- many $ do
        call <- optional $ functionArgs <|> parseDotAccess
        case call of
            (Just result) -> return $ Left result
            (Nothing) -> do
                lexe $ char '!'
                return $ Right ()

    return $ if null argss
        then name
        else foldl
                (\acc alt -> case alt of
                    (Left applyFun) -> HiExprApply acc applyFun
                    (Right _) -> HiExprRun acc)
                (case head argss of
                    (Left applyFun) -> (HiExprApply name applyFun)
                    (Right _) -> HiExprRun name)
                $ tail argss

exprInfix :: Parser HiExpr
exprInfix = makeExprParser exprTerm table

table = [
          [ binary' InfixL "*" HiFunMul
          , binaryRej "/" "=" HiFunDiv]
        , [ binary' InfixL "+" HiFunAdd
          , binary' InfixL "-" HiFunSub]
        , [ binary' InfixN ">=" HiFunNotLessThan
          , binary' InfixN "<=" HiFunNotGreaterThan]
        , [ binary' InfixN "<" HiFunLessThan
          , binary' InfixN ">" HiFunGreaterThan]
        , [ binary' InfixN "==" HiFunEquals
          , binary' InfixN "/=" HiFunNotEquals]
        , [binary' InfixR "&&" HiFunAnd]
        , [binary' InfixR "||" HiFunOr]
        ]

genericTableEntry :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
genericTableEntry fun = (\x y -> HiExprApply (HiExprValue $ HiValueFunction $ fun) [x, y])

hiSymOp' :: String -> Parser ()
hiSymOp' name = lexe $ void $ string name

-- takes string -- rj. takes parser p. success only if there no rj after p success
rejectFollowing :: String -> String -> Parser ()
rejectFollowing name reject = void $ lexe . try $ string name <* (notFollowedBy . string) reject

binaryRej :: String -> String -> HiFun -> Operator Parser HiExpr
binaryRej name reject f = InfixL $ genericTableEntry f <$ rejectFollowing name reject

binary' ctor name f = ctor $ genericTableEntry f <$ hiSymOp' name

postfix :: String -> (a -> a) -> Operator Parser a
postfix name f = Postfix (f <$ hiSymOp' name)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space >> exprInfix <* eof) ""
