{-# LANGUAGE LambdaCase #-}
module HW3.Evaluator(
    eval
) where

import HW3.Base (
        HiFun(..),
        HiValue(..),
        HiExpr(..),
        HiError(..),
        HiAction(..),
        HiMonad(..))
import HW3.Action(HiPermission(..), HIO (HIO))
import Data.Either
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Data.Foldable (find, toList)
import qualified Data.Text as T
import Data.Ratio (numerator)
import GHC.Real (denominator)
import Data.Semigroup (stimes)
import Data.String
import qualified Data.Sequence as S
import qualified Data.ByteString as BS
import Data.Word
import Data.Functor
import qualified Data.Text.Encoding as Enc
import Codec.Compression.Zlib (compressWith, defaultCompressParams, compressLevel, bestCompression, decompress)
import qualified Data.ByteString.Lazy as BL
import Codec.Serialise (serialise, deserialiseOrFail)
import Data.ByteString (ByteString)
import qualified Data.Time.Clock as Time
import qualified Text.Read as TR
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Bifunctor
import Data.Tuple (swap)

type HiExcept = ExceptT HiError

arityHelper :: HiMonad m => [Int] -> HiValue -> [HiValue] -> (HiExcept m HiValue)
arityHelper nums fun args = (checkArity nums args) >>= (checkSignatureEval fun)

checkArity :: HiMonad m => [Int] -> [a] -> (HiExcept m [a])
checkArity nums args = case find (== length args) nums of
    (Just _) -> return args
    (Nothing) -> throwError $ HiErrorArityMismatch

getArity :: HiValue -> [Int]
getArity (HiValueFunction (HiFunDiv))            = [2]
getArity (HiValueFunction (HiFunMul))            = [2]
getArity (HiValueFunction (HiFunAdd))            = [2]
getArity (HiValueFunction (HiFunSub))            = [2]
getArity (HiValueFunction (HiFunNot))            = [1]
getArity (HiValueFunction (HiFunAnd))            = [2]
getArity (HiValueFunction (HiFunOr))             = [2]
getArity (HiValueFunction (HiFunLessThan))       = [2]
getArity (HiValueFunction (HiFunGreaterThan))    = [2]
getArity (HiValueFunction (HiFunEquals))         = [2]
getArity (HiValueFunction (HiFunNotLessThan))    = [2]
getArity (HiValueFunction (HiFunNotGreaterThan)) = [2]
getArity (HiValueFunction (HiFunNotEquals))      = [2]
getArity (HiValueFunction (HiFunIf))             = [3]
getArity (HiValueFunction (HiFunLength))         = [1]
getArity (HiValueFunction (HiFunToLower))        = [1]
getArity (HiValueFunction (HiFunToUpper))        = [1]
getArity (HiValueFunction (HiFunReverse))        = [1]
getArity (HiValueFunction (HiFunTrim))           = [1]
getArity (HiValueString s)                       = [1, 2]
getArity (HiValueList l)                         = [1, 2]
getArity (HiValueFunction (HiFunList))           = error "Implementation error: List should not have arity check!" -- no arity, do not call this, it would be error in code
getArity (HiValueFunction (HiFunRange))          = [2]
getArity (HiValueFunction (HiFunFold))           = [2]
getArity (HiValueFunction (HiFunPackBytes))      = [1]
getArity (HiValueFunction (HiFunUnpackBytes))    = [1]
getArity (HiValueFunction (HiFunDecodeUtf8))     = [1]
getArity (HiValueFunction (HiFunEncodeUtf8))     = [1]
getArity (HiValueFunction (HiFunZip))            = [1]
getArity (HiValueFunction (HiFunUnzip))          = [1]
getArity (HiValueFunction (HiFunSerialise))      = [1]
getArity (HiValueFunction (HiFunDeserialise))    = [1]
getArity (HiValueBytes bts)                      = [1, 2]
getArity (HiValueFunction (HiFunRead))           = [1]
getArity (HiValueFunction (HiFunWrite))          = [2]
getArity (HiValueFunction (HiFunMkDir))          = [1]
getArity (HiValueFunction (HiFunChDir))          = [1]
getArity (HiValueFunction (HiFunParseTime))      = [1]
getArity (HiValueFunction (HiFunRand))           = [2]
getArity (HiValueFunction (HiFunEcho))           = [1]
getArity (HiValueFunction (HiFunCount))          = [1]
getArity (HiValueFunction (HiFunKeys))           = [1]
getArity (HiValueFunction (HiFunValues))         = [1]
getArity (HiValueFunction (HiFunInvert))         = [1]
getArity (HiValueDict _)                         = [1]
getArity unknown                                 = error $ "Implementation error: " ++ show unknown -- same

applyHiFunc :: HiMonad m => HiValue -> [HiValue] -> (HiExcept m HiValue)
applyHiFunc fun@(HiValueFunction (HiFunList)) args = checkSignatureEval fun args

applyHiFunc (HiValueNumber _) _ = throwError HiErrorInvalidFunction
applyHiFunc (HiValueBool _) _ = throwError HiErrorInvalidFunction
applyHiFunc (HiValueNull) _ = throwError HiErrorInvalidFunction
applyHiFunc (HiValueAction _) _ = throwError HiErrorInvalidFunction
applyHiFunc (HiValueTime _) _ = throwError HiErrorInvalidFunction

applyHiFunc fun args = arityHelper (getArity fun) fun args

arityLazyHelper :: HiMonad m => [Int] -> HiValue -> [HiExpr] -> (HiExcept m HiValue)
arityLazyHelper nums fun args = (checkArity nums args) >>= (checkSignatureEvalLazy fun)

applyLazyHiFunc :: HiMonad m => HiValue -> [HiExpr] -> HiExcept m HiValue
applyLazyHiFunc (HiValueNumber _) _ = throwError HiErrorInvalidFunction
applyLazyHiFunc (HiValueBool _) _ = throwError HiErrorInvalidFunction
applyLazyHiFunc (HiValueNull) _ = throwError HiErrorInvalidFunction
applyLazyHiFunc (HiValueAction _) _ = throwError HiErrorInvalidFunction
applyLazyHiFunc (HiValueTime _) _ = throwError HiErrorInvalidFunction

applyLazyHiFunc fun args = arityLazyHelper (getArity fun) fun args

-- little string helper
sliceT :: Int -> Int -> T.Text -> T.Text
sliceT start end = T.take (end-start) . T.drop start

-- little list helper
sliceL :: Int -> Int -> (S.Seq HiValue) -> (S.Seq HiValue)
sliceL start end = S.take (end-start) . S.drop start

-- little bytes helper
sliceB :: Int -> Int -> ByteString -> ByteString
sliceB start end = BS.take (end-start) . BS.drop start

-- common things for strings and lists indexing/slices
getSlice :: (a -> Int) -> (Int -> Int -> a -> a) -> a -> a -> Rational -> Rational -> Either HiError a
getSlice len slice empty s left right = let
        l = fromInteger $ numerator left
        r = fromInteger $ numerator right
        left' = if left < 0 then min (len s) $ max 0 $ l + len s else l
        right' = if right < 0 then min (len s) $ max 0 $ r + len s else r
    in if abs (denominator left) == 1 && abs (denominator right) == 1
        then if left' < right'
            then Right $ slice left' right' s
            else Right $ empty
        else Left HiErrorInvalidArgument

wrapBool :: HiMonad m => Bool -> (HiExcept m HiValue)
wrapBool = return . HiValueBool

wrapNum :: HiMonad m => Rational -> (HiExcept m HiValue)
wrapNum = return . HiValueNumber

wrapString :: HiMonad m => T.Text -> (HiExcept m HiValue)
wrapString = return . HiValueString

wrapList :: HiMonad m => S.Seq HiValue -> (HiExcept m HiValue)
wrapList = return . HiValueList

wrapBytes :: HiMonad m => ByteString -> (HiExcept m HiValue)
wrapBytes = return . HiValueBytes

wrapAction :: HiMonad m => HiAction -> (HiExcept m HiValue)
wrapAction = return . HiValueAction

wrapTime :: HiMonad m => Time.UTCTime -> (HiExcept m HiValue)
wrapTime = return . HiValueTime

wrapDict :: HiMonad m => Map.Map HiValue HiValue -> (HiExcept m HiValue)
wrapDict = return . HiValueDict

evalHiFuncDiv :: HiMonad m => Rational -> Rational -> (HiExcept m HiValue)
evalHiFuncDiv a b   | b == 0 = throwError HiErrorDivideByZero
                    | otherwise = wrapNum $ a / b

checkSignatureEval :: HiMonad m => HiValue -> [HiValue] -> (HiExcept m HiValue)
checkSignatureEval (HiValueFunction (HiFunDiv)) [(HiValueNumber a), (HiValueNumber b)] = evalHiFuncDiv a b
checkSignatureEval (HiValueFunction (HiFunDiv)) [(HiValueString a), (HiValueString b)] = wrapString $ T.concat [a, fromString "/", b]

checkSignatureEval (HiValueFunction (HiFunMul)) [(HiValueNumber a), (HiValueNumber b)] = wrapNum $ a * b
checkSignatureEval (HiValueFunction (HiFunMul)) [(HiValueString a), (HiValueNumber b)] = wrapString $ stimes (numerator b) a
checkSignatureEval (HiValueFunction (HiFunMul)) [(HiValueList ls), (HiValueNumber b)] = wrapList $ stimes (numerator b) ls
checkSignatureEval (HiValueFunction (HiFunMul)) [(HiValueBytes bts), (HiValueNumber b)] = wrapBytes $ stimes (numerator b) bts

checkSignatureEval (HiValueFunction (HiFunAdd)) [(HiValueNumber a), (HiValueNumber b)] = wrapNum $ a + b
checkSignatureEval (HiValueFunction (HiFunAdd)) [(HiValueString a), (HiValueString b)] = wrapString $ a <> b
checkSignatureEval (HiValueFunction (HiFunAdd)) [(HiValueList as), (HiValueList bs)] = wrapList $ as <> bs
checkSignatureEval (HiValueFunction (HiFunAdd)) [(HiValueBytes abts), (HiValueBytes bbts)] = wrapBytes $ abts <> bbts
checkSignatureEval (HiValueFunction (HiFunAdd)) [(HiValueTime t), (HiValueNumber num)] = wrapTime $ Time.addUTCTime (fromIntegral $ numerator num) t

checkSignatureEval (HiValueFunction (HiFunSub)) [(HiValueNumber a), (HiValueNumber b)] = wrapNum $ a - b
checkSignatureEval (HiValueFunction (HiFunSub)) [(HiValueTime ta), (HiValueTime tb)] = wrapNum $ toRational $ Time.diffUTCTime ta tb

checkSignatureEval (HiValueFunction (HiFunNot)) [(HiValueBool a)] = wrapBool $ not a

checkSignatureEval (HiValueFunction (HiFunAnd)) [(HiValueBool a), (HiValueBool b)] = wrapBool $ a && b

checkSignatureEval (HiValueFunction (HiFunOr)) [(HiValueBool a), (HiValueBool b)] = wrapBool $ a || b

checkSignatureEval (HiValueFunction (HiFunLessThan)) [a, b] = wrapBool $ a < b

checkSignatureEval (HiValueFunction (HiFunGreaterThan)) [a, b] = wrapBool $ a > b
checkSignatureEval (HiValueFunction (HiFunEquals)) [a, b] = wrapBool $ a == b

checkSignatureEval (HiValueFunction (HiFunNotLessThan)) [a, b] = wrapBool $ a >= b

checkSignatureEval (HiValueFunction (HiFunNotGreaterThan)) [a, b] = wrapBool $ a <= b

checkSignatureEval (HiValueFunction (HiFunNotEquals)) [a, b] = wrapBool $ a /= b

checkSignatureEval (HiValueFunction (HiFunIf)) [(HiValueBool check), a, b] = error "Implementaion error, contact student (tg:@pprettysimpple) if raised"

checkSignatureEval (HiValueFunction (HiFunLength)) [(HiValueString txt)] = wrapNum $ toRational $ T.length txt
checkSignatureEval (HiValueFunction (HiFunLength)) [(HiValueList ls)] = wrapNum $ toRational $ S.length ls
checkSignatureEval (HiValueFunction (HiFunLength)) [(HiValueBytes bts)] = wrapNum $ toRational $ BS.length bts

checkSignatureEval (HiValueFunction (HiFunToUpper)) [(HiValueString txt)] = wrapString $ T.toUpper txt

checkSignatureEval (HiValueFunction (HiFunToLower)) [(HiValueString txt)] = wrapString $ T.toLower txt

checkSignatureEval (HiValueFunction (HiFunReverse)) [(HiValueString txt)] = wrapString $ T.reverse txt
checkSignatureEval (HiValueFunction (HiFunReverse)) [(HiValueList ls)] = wrapList $ S.reverse ls

checkSignatureEval (HiValueFunction (HiFunTrim)) [(HiValueString txt)] = wrapString $ T.strip txt

---- there is strings slices/indexes

-- null handling
checkSignatureEval st@(HiValueString s) [l@(HiValueNumber left), (HiValueNull)] =
    checkSignatureEval st [l, (HiValueNumber $ toRational $ T.length s)]
checkSignatureEval st@(HiValueString s) [(HiValueNull), r@(HiValueNumber right)] =
    checkSignatureEval st [(HiValueNumber 0), r]
checkSignatureEval st@(HiValueString s) [(HiValueNull), (HiValueNull)] =
    checkSignatureEval st [(HiValueNumber 0), (HiValueNumber $ toRational $ T.length s)]

-- 2 args
checkSignatureEval (HiValueString s) [(HiValueNumber a), (HiValueNumber b)] = case getSlice T.length sliceT T.empty s a b of
    (Left err) -> throwError err
    (Right val) -> wrapString val
-- 1 argument
checkSignatureEval (HiValueString s) [(HiValueNumber num)] =
    let idx = fromInteger $ numerator num
    in if idx < T.length s && idx >= 0
        then wrapString $ T.singleton $ T.index s idx
        else return $ HiValueNull

---- here lists slices/indexing

-- 2 args
checkSignatureEval (HiValueList s) [(HiValueNumber a), (HiValueNumber b)] = case getSlice S.length sliceL S.empty s a b of
    (Left err) -> throwError err
    (Right val) -> wrapList val
-- 1 argument
checkSignatureEval (HiValueList s) [(HiValueNumber num)] =
    let idx = fromInteger $ numerator num
    in if idx < S.length s && idx >= 0
        then return $ S.index s idx
        else return $ HiValueNull

-- list slice null handling
checkSignatureEval st@(HiValueList ls) [l@(HiValueNumber left), (HiValueNull)] =
    checkSignatureEval st [l, (HiValueNumber $ toRational $ length ls)]
checkSignatureEval st@(HiValueList _) [(HiValueNull), r@(HiValueNumber right)] =
    checkSignatureEval st [(HiValueNumber 0), r]
checkSignatureEval st@(HiValueList ls) [(HiValueNull), (HiValueNull)] =
    checkSignatureEval st [(HiValueNumber 0), (HiValueNumber $ toRational $ length ls)]

checkSignatureEval (HiValueFunction (HiFunList)) args = return $ HiValueList $ S.fromList args

checkSignatureEval (HiValueFunction (HiFunRange)) [(HiValueNumber a), (HiValueNumber b)] =
    return $ HiValueList $ S.fromList $ fmap HiValueNumber $ [a .. b]

checkSignatureEval (HiValueFunction (HiFunFold)) [f, (HiValueList ls)]
    | S.length ls > 0 = foldl (\acc val -> acc >>= \x -> checkSignatureEval f [x, val]) (return $ S.index ls 0) (S.drop 1 ls)
    | otherwise       = return $ HiValueNull

checkSignatureEval (HiValueFunction (HiFunPackBytes)) [(HiValueList ls)] =
    let
        toByte :: HiMonad m => HiValue -> HiExcept m Word8
        toByte (HiValueNumber num) = if denominator num == 1 && numerator num >= 0 && numerator num < 256
            then return $ ((fromIntegral $ numerator num) :: Word8)
            else throwError HiErrorInvalidArgument
        toByte _ = throwError HiErrorInvalidArgument
    in (mapM toByte $ toList ls) <&> (HiValueBytes . BS.pack)
checkSignatureEval (HiValueFunction (HiFunUnpackBytes)) [(HiValueBytes bts)] =
    return $ HiValueList $ S.fromList $ fmap (HiValueNumber . toRational . fromIntegral) $ BS.unpack bts
checkSignatureEval (HiValueFunction (HiFunEncodeUtf8)) [(HiValueString s)] = wrapBytes $ Enc.encodeUtf8 s
checkSignatureEval (HiValueFunction (HiFunDecodeUtf8)) [(HiValueBytes bts)] = return $
    case Enc.decodeUtf8' bts of
        (Left err)  -> HiValueNull
        (Right txt) -> HiValueString txt
checkSignatureEval (HiValueFunction (HiFunZip)) [(HiValueBytes bts)] = return $ HiValueBytes $ BL.toStrict $
    compressWith defaultCompressParams { compressLevel = bestCompression } $ BL.fromStrict bts
checkSignatureEval (HiValueFunction (HiFunUnzip)) [(HiValueBytes bts)] = wrapBytes $ BL.toStrict $ decompress $ BL.fromStrict bts
checkSignatureEval (HiValueFunction (HiFunSerialise)) [value] = wrapBytes $ BL.toStrict $ serialise value
checkSignatureEval (HiValueFunction (HiFunDeserialise)) [(HiValueBytes bts)] = return $
    case deserialiseOrFail $ BL.fromStrict bts of
        (Left _) -> HiValueNull
        (Right val) -> val

-- bytes slice null handling
checkSignatureEval st@(HiValueBytes bts) [l@(HiValueNumber left), (HiValueNull)] =
    checkSignatureEval st [l, (HiValueNumber $ toRational $ BS.length bts)]
checkSignatureEval st@(HiValueBytes _) [(HiValueNull), r@(HiValueNumber right)] =
    checkSignatureEval st [(HiValueNumber 0), r]
checkSignatureEval st@(HiValueBytes bts) [(HiValueNull), (HiValueNull)] =
    checkSignatureEval st [(HiValueNumber 0), (HiValueNumber $ toRational $ BS.length bts)]

checkSignatureEval (HiValueBytes bts) [(HiValueNumber a), (HiValueNumber b)] = case getSlice BS.length sliceB BS.empty bts a b of
    (Left err) -> throwError err
    (Right val) -> wrapBytes val
checkSignatureEval (HiValueBytes bts) [(HiValueNumber num)] =
    let idx = fromInteger $ numerator num
    in if idx < BS.length bts && idx >= 0
        then wrapNum $ toRational $ BS.index bts idx
        else return $ HiValueNull

checkSignatureEval (HiValueFunction (HiFunRead)) [(HiValueString path)] = wrapAction $ HiActionRead $
    T.unpack path

checkSignatureEval (HiValueFunction (HiFunWrite)) [(HiValueString path), (HiValueString content)] =
    wrapAction $ HiActionWrite (T.unpack path) (fromString $ T.unpack content)

checkSignatureEval (HiValueFunction (HiFunWrite)) [(HiValueString path), (HiValueBytes content)] =
    wrapAction $ HiActionWrite (T.unpack path) content

checkSignatureEval (HiValueFunction (HiFunMkDir)) [(HiValueString path)] =
    wrapAction $ HiActionMkDir (T.unpack path)

checkSignatureEval (HiValueFunction (HiFunChDir)) [(HiValueString path)] =
    wrapAction $ HiActionChDir (T.unpack path)

checkSignatureEval (HiValueFunction (HiFunParseTime)) [(HiValueString time')] =
    case TR.readMaybe $ T.unpack time' of
        (Just time) -> wrapTime $ time
        (Nothing) -> return $ HiValueNull

checkSignatureEval (HiValueFunction (HiFunRand)) [(HiValueNumber a), (HiValueNumber b)] =
    if denominator a == 1 && denominator b == 1
        then wrapAction $ HiActionRand (fromIntegral $ numerator a) (fromIntegral $ numerator b)
        else throwError HiErrorInvalidArgument

checkSignatureEval (HiValueFunction (HiFunEcho)) [(HiValueString txt)] = wrapAction $ HiActionEcho txt

checkSignatureEval (HiValueFunction (HiFunCount)) [(HiValueString txt)] =
    wrapDict
        $ Map.map (HiValueNumber . toRational)
        $ Map.mapKeys (HiValueString . T.singleton)
        $ Map.fromListWith (+) $ [(c, 1) | c <- T.unpack txt]

checkSignatureEval (HiValueFunction (HiFunCount)) [(HiValueBytes bts)] =
    wrapDict
        $ Map.map (HiValueNumber . toRational)
        $ Map.mapKeys (HiValueBytes . BS.singleton)
        $ Map.fromListWith (+) $ [(c, 1) | c <- BS.unpack bts]

checkSignatureEval (HiValueFunction (HiFunCount)) [(HiValueList ls)] =
    wrapDict
        $ Map.map (HiValueNumber . toRational)
        $ Map.fromListWith (+) $ [(c, 1) | c <- toList ls]

checkSignatureEval (HiValueFunction (HiFunKeys)) [(HiValueDict dict)] =
    wrapList $ S.fromList $ Map.keys dict

checkSignatureEval (HiValueFunction (HiFunValues)) [(HiValueDict dict)] =
    wrapList $ S.fromList $ Map.elems dict

checkSignatureEval (HiValueFunction (HiFunInvert)) [(HiValueDict dict)] =
    wrapDict
        $ Map.map (HiValueList . S.fromList)
        $ Map.fromListWith (++)
        $ fmap (\(x, y) -> (y, [x])) (Map.toList dict)

checkSignatureEval (HiValueDict dict) [val] =
    return $ case Map.lookup val dict of
        (Just val) -> val
        (Nothing)  -> HiValueNull

checkSignatureEval (HiValueNumber _) _ = throwError HiErrorInvalidFunction
checkSignatureEval (HiValueBool _) _ = throwError HiErrorInvalidFunction
checkSignatureEval (HiValueNull) _ = throwError HiErrorInvalidFunction
checkSignatureEval _ _ = throwError HiErrorInvalidArgument

checkSignatureEvalLazy :: HiMonad m => HiValue -> [HiExpr] -> (HiExcept m HiValue)
checkSignatureEvalLazy (HiValueFunction (HiFunIf)) [control, left, right] = do
    control' <- (eval' control)
    case control' of
        (HiValueBool val) -> eval' $ if (val) then (left) else (right)
        _ -> throwError HiErrorInvalidArgument

checkSignatureEvalLazy (HiValueFunction (HiFunOr)) [left, right] = do
    left' <- (eval' left)
    case left' of
        (HiValueBool False) -> eval' right
        (HiValueNull) -> eval' right
        other -> return other

checkSignatureEvalLazy (HiValueFunction (HiFunAnd)) [left, right] = do
    left' <- (eval' left)
    case left' of
        (HiValueBool False) -> return $ HiValueBool False
        (HiValueNull) -> return $ HiValueNull
        other -> eval' right

checkSignatureEvalLazy (HiValueNumber _) _ = throwError HiErrorInvalidFunction
checkSignatureEvalLazy (HiValueBool _) _ = throwError HiErrorInvalidFunction
checkSignatureEvalLazy (HiValueNull) _ = throwError HiErrorInvalidFunction
checkSignatureEvalLazy _ _ = throwError HiErrorInvalidArgument

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval exp = runExceptT $ eval' exp

eval' :: HiMonad m => HiExpr -> HiExcept m HiValue
eval' (HiExprValue val) = return val
eval' (HiExprApply func args) = do
        eFun <- eval' func
        case eFun of
            (fun@(HiValueFunction HiFunIf)) -> applyLazyHiFunc fun args
            (fun@(HiValueFunction HiFunAnd)) -> applyLazyHiFunc fun args
            (fun@(HiValueFunction HiFunOr)) -> applyLazyHiFunc fun args
            (fun) -> do
                eArgs <- (mapM eval' args)
                case (mapM Right eArgs) of
                    (Left err) -> throwError err
                    (Right args) -> applyHiFunc fun args
eval' (HiExprRun actExpr) = do
        action <- eval' actExpr
        case action of
            (HiValueAction act) -> lift $ runAction act
            _ -> throwError HiErrorInvalidArgument
eval' (HiExprDict ls) = do
        ls' <- mapM (\(x, y) -> do
            xRes <- eval' x
            yRes <- eval' y
            return (xRes, yRes)) ls
        return $ HiValueDict $ Map.fromList ls'

