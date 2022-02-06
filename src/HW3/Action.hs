{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module HW3.Action(
    HiPermission(..),
    PermissionException(..),
    HIO(..)
) where

import Control.Exception
import Data.Set hiding(map)
import HW3.Base
import Control.Monad
import GHC.TopHandler (runIO)
import GHC.IO
import qualified System.Directory as SysDir
import Data.String
import Data.Sequence (fromList)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Time.Clock as Time
import qualified System.Random as Rand
import qualified System.Random.Stateful as RandS
import Data.Functor
import qualified Data.Text as T

data HiPermission = AllowRead
        | AllowWrite
        | AllowTime deriving(Ord, Eq, Enum, Bounded)

instance Show HiPermission where
    show (AllowRead) = "read-perm"
    show (AllowWrite) = "write-perm"
    show (AllowTime) = "allow-time-perm"

data PermissionException =
    PermissionRequired HiPermission deriving(Show)

instance Exception PermissionException where
    -- nothing

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
    fmap f m = HIO $ (runHIO m >=> (return . f))

instance Applicative HIO where
    pure x = HIO $ \perms -> return x
    m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad HIO where
    return :: a -> HIO a
    return = pure

    (>>=) :: HIO a -> (a -> HIO b) -> HIO b
    m >>= fun = HIO $ \params -> (runHIO m params) >>= \x -> runHIO (fun x) params

checkPermitsWrapper :: [HiPermission] -> IO a -> Set HiPermission -> IO a
checkPermitsWrapper needPermits fun =
    let needPermits' = Set.fromList needPermits
    in \inPermits ->
        if needPermits' `isSubsetOf` inPermits
            then fun
            else throwIO $ PermissionRequired $ Set.elemAt 0 $ difference needPermits' inPermits


instance HiMonad HIO where
    runAction (HiActionRead path) = HIO $ checkPermitsWrapper [AllowRead] $ do
        existsDir  <- (SysDir.doesDirectoryExist path)
        existsFile <- (SysDir.doesFileExist path)
        if existsDir
            then (SysDir.listDirectory path) >>=
                (\files ->
                    return $ HiValueList $ Data.Sequence.fromList $
                        map (HiValueString . fromString) files)
            else if existsFile
                then BS.readFile path >>= ((\bts -> return $ case decodeUtf8' bts of
                        (Left _) -> HiValueBytes bts
                        (Right txt) -> HiValueString txt))
                else return HiValueNull

    runAction (HiActionWrite path content) = HIO $ checkPermitsWrapper [AllowWrite] $ do
        BS.writeFile path content
        return HiValueNull

    runAction (HiActionMkDir path) = HIO $ checkPermitsWrapper [AllowWrite] $ fmap (const HiValueNull) $ SysDir.createDirectory path
    runAction (HiActionChDir path) = HIO $ checkPermitsWrapper [AllowRead] $ fmap (const HiValueNull) $ SysDir.setCurrentDirectory path
    runAction (HiActionCwd) = HIO $ checkPermitsWrapper [AllowRead] $ fmap (HiValueString . fromString) SysDir.getCurrentDirectory
    runAction (HiActionNow) = HIO $ checkPermitsWrapper [AllowTime] $ fmap HiValueTime Time.getCurrentTime
    runAction (HiActionRand a b) = HIO $ checkPermitsWrapper [] $ do
        fmap  (HiValueNumber . toRational) $ RandS.uniformRM (a, b) RandS.globalStdGen
    runAction (HiActionEcho txt) = HIO $ checkPermitsWrapper [AllowWrite] $ (putStrLn $ T.unpack txt) $> HiValueNull

