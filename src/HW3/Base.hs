{-# LANGUAGE DeriveGeneric #-}
module HW3.Base (
        HiFun(..),
        HiValue(..),
        HiExpr(..),
        HiError(..),
        HiAction(..),
        HiMonad(..)
) where
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.ByteString as BS
import Codec.Serialise
import GHC.Generics (Generic)
import qualified Data.Time as Time
import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Char (toLower)
import Data.Ratio

-- function names (e.g. div, sort, length, ...)
data HiFun = HiFunDiv
            | HiFunMul
            | HiFunAdd
            | HiFunSub
            | HiFunNot
            | HiFunAnd
            | HiFunOr
            | HiFunLessThan
            | HiFunGreaterThan
            | HiFunEquals
            | HiFunNotLessThan
            | HiFunNotGreaterThan
            | HiFunNotEquals
            | HiFunIf
            | HiFunLength
            | HiFunToUpper
            | HiFunToLower
            | HiFunReverse
            | HiFunTrim
            | HiFunList
            | HiFunRange
            | HiFunFold
            | HiFunPackBytes
            | HiFunUnpackBytes
            | HiFunEncodeUtf8
            | HiFunDecodeUtf8
            | HiFunZip
            | HiFunUnzip
            | HiFunSerialise
            | HiFunDeserialise
            | HiFunRead
            | HiFunWrite
            | HiFunMkDir
            | HiFunChDir
            | HiFunParseTime
            | HiFunRand
            | HiFunEcho
            | HiFunCount
            | HiFunKeys
            | HiFunValues
            | HiFunInvert deriving(Eq, Ord, Generic)

instance Show HiFun where
    show (HiFunDiv) = "div"
    show (HiFunMul) = "mul"
    show (HiFunAdd) = "add"
    show (HiFunSub) = "sub"
    show (HiFunNot) = "not"
    show (HiFunAnd) = "and"
    show (HiFunOr) = "or"
    show (HiFunLessThan) = "less-than"
    show (HiFunGreaterThan) = "greater-than"
    show (HiFunEquals) = "equals"
    show (HiFunNotLessThan) = "not-less-than"
    show (HiFunNotGreaterThan) = "not-greater-than"
    show (HiFunNotEquals) = "not-equals"
    show (HiFunIf) = "if"
    show (HiFunLength) = "length"
    show (HiFunToUpper) = "to-upper"
    show (HiFunToLower) = "to-lower"
    show (HiFunReverse) = "reverse"
    show (HiFunTrim) = "trim"
    show (HiFunList) = "list"
    show (HiFunRange) = "range"
    show (HiFunFold) = "fold"
    show (HiFunPackBytes) = "pack-bytes"
    show (HiFunUnpackBytes) = "unpack-bytes"
    show (HiFunEncodeUtf8) = "encode-utf8"
    show (HiFunDecodeUtf8) = "decode-utf8"
    show (HiFunZip) = "zip"
    show (HiFunUnzip) = "unzip"
    show (HiFunSerialise) = "serialise"
    show (HiFunDeserialise) = "deserialise"
    show (HiFunRead) = "read"
    show (HiFunWrite) = "write"
    show (HiFunMkDir) = "mkdir"
    show (HiFunChDir) = "cd"
    show (HiFunParseTime) = "parse-time"
    show (HiFunRand) = "rand"
    show (HiFunEcho) = "echo"
    show (HiFunCount) = "count"
    show (HiFunKeys) = "keys"
    show (HiFunValues) = "values"
    show (HiFunInvert) = "invert"

instance Serialise HiFun -- empty

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueBool Bool
            | HiValueNumber Rational
            | HiValueFunction HiFun
            | HiValueNull
            | HiValueString T.Text
            | HiValueList (Seq.Seq HiValue)
            | HiValueBytes BS.ByteString
            | HiValueAction HiAction
            | HiValueTime Time.UTCTime
            | HiValueDict (Map.Map HiValue HiValue) deriving(Eq, Ord, Generic)

-- sorry for the prettyfiing here, but i'm really tied up with "show HiFunc"
-- and can not move prettyPrinting to Pretty.hs
-- :(

displayFinit :: (Integral i, Show i) => Ratio i -> String
displayFinit rat = (if num < 0 then "-" else "") ++ show ip ++ "." ++ (go (abs num - ip * den))
  where
    num = numerator rat
    den = denominator rat
    ip  = abs num `quot` den

    go 0 = ""
    go x = shows d (go next)
      where
        (d, next) = (10 * x) `quotRem` den

displayInf :: Integer -> Integer -> String
displayInf num den = if abs num < den
    then show num ++ "/" ++ show den -- branch with "x/y"
    else let
            q = num `quot` den
            r = num `rem` den
        in show q ++ " " ++ (if r < 0 then ("- " ++ show (-r)) else ("+ " ++ show r)) ++ "/" ++ show den

divUntil :: Integer -> Integer -> Integer
divUntil val d = if val `rem` d == 0
    then divUntil (val `quot` d) d
    else val

showRational :: Rational -> String
showRational rat = let
        den = denominator rat
        num = numerator rat
    in if den == 1 then show num
    else let
        until2 = divUntil den 2
        until5 = divUntil until2 5
        in if until5 == 1
            then displayFinit rat -- finite
            else displayInf num den -- infinite

prettyByteString bytes = "[# " ++ (concatMap (printf "%02x ") $ BS.unpack bytes) ++ "#]"
prettyString str = -- "\"" ++ (take (T.length str) $ drop 1 $ show str) ++ "\""
    show str

instance Show HiValue where
    show (HiValueBool b) = fmap toLower $ show b
    show (HiValueNumber rat) = showRational rat
    show (HiValueFunction fun) = show fun
    show (HiValueNull) = "null"
    show (HiValueString str) = prettyString str
    show (HiValueList ls) = "[" ++ (intercalate "," $ fmap (\x -> " " ++ show x) $ toList ls) ++ " ]"
    show (HiValueBytes bts) = prettyByteString bts
    show (HiValueAction act) = show act
    show (HiValueTime time) = "parse-time(\"" ++ show time ++ "\")"
    show (HiValueDict mp) = "{" ++ (intercalate "," $ (fmap (\(k, v) -> " " ++ show k ++ ": " ++ show v) $ Map.toList mp)) ++ " }"

instance Serialise HiValue -- empty

-- expressions (literals, function calls, ...)
data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
            | HiExprRun HiExpr
            | HiExprDict [(HiExpr, HiExpr)] deriving(Eq, Ord)

instance Show HiExpr where
    show (HiExprValue v) = show v
    show (HiExprApply e exs) = show e ++ show exs
    show (HiExprRun exp) = show exp ++ ":run"
    show (HiExprDict ls) = "{" ++ concatMap show ls ++ "}"

-- evaluation errors (invalid arguments, ...)
data HiError = HiErrorInvalidArgument
            | HiErrorInvalidFunction
            | HiErrorArityMismatch
            | HiErrorDivideByZero deriving(Eq, Ord)

instance Show HiError where
    show (HiErrorInvalidArgument) = "InvalidArgument"
    show (HiErrorInvalidFunction) = "InvalidFunction"
    show (HiErrorArityMismatch) = "ArityMismatch"
    show (HiErrorDivideByZero) = "DivideByZero"

data HiAction = HiActionRead  FilePath
            | HiActionWrite FilePath BS.ByteString
            | HiActionMkDir FilePath
            | HiActionChDir FilePath
            | HiActionCwd
            | HiActionNow
            | HiActionRand Int Int
            | HiActionEcho T.Text deriving(Eq, Ord, Generic)

instance Show HiAction where
    show (HiActionRead p) = "read(" ++ show p ++ ")"
    show (HiActionWrite p c) = "write(" ++ show p ++ ", " ++ prettyByteString c ++ ")"
    show (HiActionMkDir p) = "mkdir(" ++ show p ++ ")"
    show (HiActionChDir p) = "cd(" ++ show p ++ ")"
    show (HiActionCwd) = "cwd"
    show (HiActionNow) = "now"
    show (HiActionRand a b) = "rand(" ++ show a ++ ", " ++ show b ++ ")"
    show (HiActionEcho txt) = "echo(" ++ prettyString txt ++ ")"

instance Serialise HiAction

class Monad m => HiMonad m where
    runAction :: HiAction -> m HiValue

