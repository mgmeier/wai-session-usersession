{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import  Network.Wai.Session.UserSession

import  Web.Scotty
import  Network.Wai                     (Middleware)
import  Data.Text.Lazy                  as T (pack)
import  Data.Time.Clock.POSIX           (POSIXTime, getPOSIXTime)

import  Control.Monad.IO.Class
import  Data.Maybe                      (fromJust)


data UserState = UserState {value :: Int, tStamp :: POSIXTime}

instance Show UserState where
    show = show . value

main :: IO ()
main = do
    putStrLn $ unlines
        [ "/new    - new session"
        , "/inc    - increment session state"
        , "/show   - show session (if any)"
        , "/kill   - delete session (if any)"
        ]
    newUserSessions options factory >>= scotty 3000 . exampleApp
  where
    options = UserSessionOptions 2 "WAIEXAMPLESESSION"

    factory :: UserSessionFactory () UserState
    factory = UserSessionFactory (return ()) initState weep
    
    initState _ _   = Just . UserState 0 <$> getPOSIXTime
    weep _ state _  = do
        alive <- subtract (tStamp state) <$> getPOSIXTime
        putStrLn $
            ";_; a user state has been deleted: " ++ show state ++
            "\n    session was alive: " ++ show alive


exampleApp :: (UserSessionHandle () UserState, Middleware) -> ScottyM ()
exampleApp (UserSessionHandle{..}, mWare) = do
    middleware mWare
      
    get "/show" $ do
        sess <- getSession
        text $ T.pack $ "current user session: " ++ show (sessionUser sess)

    get "/inc" $ do
        sess <- getSession >>= modifySession (\s -> return s {value = value s + 1})
        text $ T.pack $ "updated user session: " ++ show (sessionUser sess)
    
    get "/new" $ do
        sess <- createSession
        text $ T.pack $ "created user session: " ++ show (sessionUser sess)
    
    get "/kill" $ do
        killSession
        text "user session closed"

  where
    -- helper definitions for scotty
    -- adjust for other wai-based frameworks and/or own routing abstractions
    modifySession f = liftIO . hModifyLocal f
    killSession     = request >>= liftIO . hCloseSession
    createSession   = request >>= liftIO . hCreateSession
    getSession      = request >>= liftIO . hUnwrapSession . Right
    

