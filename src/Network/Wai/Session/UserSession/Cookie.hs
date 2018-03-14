{-# LANGUAGE OverloadedStrings #-}

module  Network.Wai.Session.UserSession.Cookie
        ( runTimer
        , getTimer
        , initCookieStore
        , genSessionId
        ) where

import  Network.Wai.Session.UserSession.Types (SessionID(..), VaultKey)

import  Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import  Data.Time.Clock         (UTCTime(..))
import  Data.Default.Class      (def)

import Network.HTTP.Types.Header (hSetCookie, hCookie)
import Network.Wai               (Middleware, Request(..), mapResponseHeaders)
import Web.Cookie                (parseCookies, renderSetCookie, SetCookie(..))

import qualified Data.Vault.Lazy    as Vault
import  Data.ByteString.Char8       as B (ByteString, pack, null)
import  Data.ByteString.Builder     as B (toLazyByteString)
import  Data.ByteString.Lazy        as B (toStrict)

import  Data.IORef
import  Control.Concurrent      (forkIO, threadDelay)
import  Control.Monad           (forever, void)
import  System.IO.Unsafe        (unsafePerformIO)
import  Data.Unique             (newUnique, hashUnique)
import  Data.Ratio              (numerator, denominator)
import  Control.Monad.IO.Class  (liftIO)


-- | Fully parameterised middleware for cookie-based sessions
middleware ::
    SetCookie
    -- ^ Settings for the cookie (name, path, expiry, etc)
    -> VaultKey
    -- ^ 'Data.Vault.Vault' key to use when passing the session through
    -> Middleware
middleware cookieOpts key app req respond = do
    (getCookieVal, access) <- liftIO newGetterSetter
    let
        respond' r = liftIO getCookieVal >>= respond . maybe r
            (\val -> mapResponseHeaders ((hSetCookie, newCookie val):) r)
    app (req {vault = Vault.insert key access (vault req)}) respond'
  where
    name    = setCookieName cookieOpts
    expires = setCookieExpires cookieOpts
    cookie  = SessionID <$>
        (lookup hCookie (requestHeaders req) >>= lookup name . parseCookies)
    
    newGetterSetter = do
        ref <- newIORef Nothing
        let getRef = readIORef ref >>= mapM (\val' -> (,) val' <$> yesterday) 
        return (getRef, (cookie, atomicWriteIORef ref . Just))

    yesterday = do
        UTCTime day time <- posixSecondsToUTCTime <$> getTimer
        return $ UTCTime (pred day) time

    newCookie (SessionID val, tOld) =
        (B.toStrict . B.toLazyByteString . renderSetCookie) cookieOpts
            { setCookieValue    = val
            , setCookieExpires  = if B.null val then Just tOld else expires
            }


initCookieStore :: String -> IO (VaultKey, Middleware)
initCookieStore name = do
    key     <- Vault.newKey -- :: IO VaultKey
    return  ( key
            , middleware (def {setCookiePath = Just "/", setCookieName = B.pack name}) key
            )


-- | Simple session ID generator based on time and 'Data.Unique'
--
-- Useful for session stores that use session IDs.
genSessionId :: IO ByteString
genSessionId = do
    u       <- toInteger . hashUnique <$> newUnique
    now     <- toRational <$> getTimer
    return $ B.pack $ show (numerator now * denominator now * u)

timerRef :: IORef POSIXTime
timerRef = unsafePerformIO (getPOSIXTime >>= newIORef)
{-# NOINLINE timerRef #-}

runTimer :: Int -> IO ()
runTimer granularity = void . forkIO $
    forever (getPOSIXTime >>= atomicWriteIORef timerRef >> threadDelay granularity)

getTimer :: IO POSIXTime
getTimer = readIORef timerRef
