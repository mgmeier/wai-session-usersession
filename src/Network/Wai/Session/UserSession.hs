{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables, ViewPatterns #-}

module Network.Wai.Session.UserSession
        ( newUserSessions
        , module Types
        ) where

import  Network.Wai.Session.UserSession.Types as Types
import  Network.Wai.Session.UserSession.Cookie

import  qualified Data.Vault.Lazy   as Vault
import  Network.Wai                 (Middleware, Request, vault)
import  Data.Time.Clock.POSIX       (POSIXTime)
import  qualified Data.Map.Strict   as M
import  Data.Default.Class          (def)

import  Data.IORef
import  Data.Bifunctor          (first, bimap)
import  Data.Maybe              (fromJust)
import  Control.Concurrent.MVar
import  Control.Concurrent      (threadDelay, forkIO)
import  Control.Monad           (forever, when, unless, (>=>), forM, forM_, void)


type SessionMap local = M.Map SessionID (POSIXTime, IORef local)
type IOSessions local = MVar (SessionMap local)


newUserSessions :: forall s l . UserSessionOptions -> UserSessionFactory s l -> IO (UserSessionHandle s l, Middleware)
newUserSessions UserSessionOptions{..} (UserSessionFactory newS newL killL) = do
    (cKey, middleware) <- initCookieStore usCookieName
    sMapMV <- newMVar M.empty
    sharedRef <- newS >>= newIORef

    let
        -- helper definitions
        finalize        :: Maybe s -> SessionID -> l -> IO ()
        finalize ms i l = maybe readShared return ms >>= \s -> killL s l i
        
        garbageColl     :: SessionMap l -> IO ()
        garbageColl ls  = unless (M.null ls) $ readShared >>= \s ->
            void $ M.traverseWithKey (\sId (_, lRef) -> readIORef lRef >>= finalize (Just s) sId) ls

        readShared = readIORef sharedRef
        mkSession f (uncurry . UserSession -> cons) =
            cons . maybe (Nothing, Nothing) (bimap Just Just) <$> f

        -- UserSessionHandle function definitions
        modShared   = modifyIORefM sharedRef
        modLocal    = modifySession cKey sMapMV
        delete      = closeSession cKey sMapMV (finalize Nothing)
        finalizeAll = swapMVar sMapMV M.empty >>= garbageColl
        create req  = readShared >>= \s -> mkSession (createSession cKey sMapMV (newL s) req) s
        unwrap eSR  = readShared >>= mkSession (unwrapSession cKey sMapMV eSR)
        dump        = readMVar sMapMV >>= \s ->
            sequence [(,,) sId t <$> readIORef ref | (sId, (t, ref)) <- M.toList s]
        traversal f = do
            s <- readShared
            readMVar sMapMV >>= mapM_ ((`modifyIORefM` f s) . snd)
        
    runTimer oneMinute

    -- garbage collector
    forkIO $ let granularity = max oneMinute (oneMinute * usValidMinutes `div` 8) in forever $ do
        threadDelay granularity
        tValid <- subtract keepAlive <$> getTimer
        modifyMVar sMapMV (return . M.partition ((>= tValid) . fst)) >>= garbageColl

    return  ( UserSessionHandle unwrap delete create traversal modShared modLocal finalizeAll dump
            , middleware
            )
  where
    oneMinute = 60 * 1000 * 1000
    keepAlive = 60 * fromIntegral usValidMinutes


-- unwraps a session, either from a specific session id or a request cookie, updating its timestamp on success
-- NB if there's no session for a cookie, the cookie's value will be left unchanged
unwrapSession :: VaultKey -> IOSessions l -> Either SessionID Request -> IO (Maybe (l, SessionID))
unwrapSession cKey sMapMV eSessId = case cLookup cKey eSessId of
    Just sessId -> do
        mSessRef <- modifyMVar sMapMV $ \sMap ->
            case M.lookup sessId sMap of
                Just (tLast, sessRef) -> do
                    tNow <- getTimer
                    return
                        (if tNow > tLast
                            then M.adjust (first (tNow `asTypeOf`)) sessId sMap
                            else sMap
                        , Just sessRef
                        )
                _ -> return (sMap, Nothing)
        forM mSessRef (readIORef >=> return . flip (,) sessId)
    _ -> return Nothing

cLookup :: VaultKey -> Either SessionID Request -> Maybe SessionID
cLookup cKey = either Just (fst . fromJust . Vault.lookup cKey . vault)

closeSession :: VaultKey -> IOSessions l -> (SessionID -> l -> IO ()) -> Request -> IO ()
closeSession cKey sMapMV destruct req = forM_ rLookup $ \sessId -> do
    cInsert def
    lRef <- modifyMVar sMapMV $ \sMap -> return (M.delete sessId sMap, snd <$> M.lookup sessId sMap)
    forM_ lRef (readIORef >=> destruct sessId)
  where
    Just (rLookup, cInsert) = Vault.lookup cKey (vault req)

-- tries to create a local session, generating a new session cookie unless already present
-- an existing cookie is deleted upon failure
-- TODO behaviour when calling create session for an exising session
createSession :: VaultKey -> IOSessions l -> (SessionID -> IO (Maybe l)) -> Request -> IO (Maybe (l, SessionID))
createSession cKey sMapMV construct req = do
    (exists, sessId) <- maybe ((,) False . SessionID <$> genSessionId) (return . (,) True) rLookup
    construct sessId >>= \case
        Just sess -> do
            sessRef <- newIORef sess
            tNow    <- getTimer
            unless exists (cInsert sessId)
            modifyMVar_ sMapMV (return . M.insert sessId (tNow, sessRef))
            return  $ Just (sess, sessId)
        _ -> when exists (cInsert def) >> return Nothing
  where
    Just (rLookup, cInsert) = Vault.lookup cKey (vault req)

modifySession :: VaultKey -> IOSessions l -> (l -> IO l) -> UserSession s l -> IO (UserSession s l)
modifySession cKey sMapMV func sess =
    case sessionId sess >>= cLookup cKey . Left of
        Just sessId -> readMVar sMapMV >>= \sMap ->
            maybe (return sess)
                (\(_, sessRef) -> update <$> modifyIORefM sessRef func)
                (M.lookup sessId sMap)
        _ -> return sess 
  where
    update l = sess {sessionUser = Just l}

modifyIORefM :: IORef a -> (a -> IO a) -> IO a
modifyIORefM ref func =
    readIORef ref >>= func >>= \s -> atomicModifyIORef' ref (const (s, s))