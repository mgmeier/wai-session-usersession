{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import  Network.Wai.Session.UserSession

import  Web.Scotty
import  Network.Wai                     (Middleware)
import  Data.Text.Lazy                  as T (pack)

import  Control.Monad.IO.Class
import  Data.Maybe                      (fromJust)


newtype UserState = UserState { value :: Maybe Int } deriving Show


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
    options = UserSessionOptions 1 "WAIEXAMPLESESSION"

    factory :: UserSessionFactory () UserState
    factory = UserSessionFactory (return ()) initState weep
    
    initState _ _   = return $ Just $ UserState Nothing
    weep _ state _  = putStrLn $ ";_; a user state has been deleted: " ++ show state


exampleApp :: (UserSessionHandle () UserState, Middleware) -> ScottyM ()
exampleApp (UserSessionHandle{..}, mWare) = do
    middleware mWare
      
    get "/show" $ do
        sess <- getSession
        text $ T.pack $ "current user session: " ++ show (sessionUser sess)

    get "/inc" $ do
        sess <- getSession
        sess' <- modifySession (\s -> return s {value = Just (maybe 1 (+1) (value s))}) sess
        text $ T.pack $ "updated user session: " ++ show (sessionUser sess')
    
    get "/new" $ do
        sess <- createSession
        text $ T.pack $ "updated user session: " ++ show (sessionUser sess)
    
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
    

