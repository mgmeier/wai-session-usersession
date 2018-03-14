{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module  Network.Wai.Session.UserSession.Types where

import  Network.Wai             (Request)
import  Data.Default.Class      (Default(..))
import  Data.ByteString         (ByteString, empty)
import  Data.Vault.Lazy         (Key)
import  Data.Maybe              (isJust)
import  Data.Time.Clock.POSIX   (POSIXTime)
import  Data.Hashable           (Hashable)


newtype SessionID = SessionID {sessionID :: ByteString} deriving (Eq, Ord, Show, Hashable)

instance Default SessionID where
    def = SessionID empty

data UserSession shared user = UserSession
    { sessionShared :: shared
    , sessionUser   :: Maybe user
    , sessionId     :: Maybe SessionID
    }


type VaultKey = Key (Maybe SessionID, SessionID -> IO ())

hasUserSession :: UserSession shared user -> Bool
hasUserSession = isJust . sessionUser


data UserSessionHandle shared user = UserSessionHandle
    { hUnwrapSession    :: Either SessionID Request -> IO (UserSession shared user)
    , hCloseSession     :: Request -> IO ()
    , hCreateSession    :: Request -> IO (UserSession shared user)

    -- calling hModifyShared during a traversal will *not* affect the value of shared for that traversal
    , hModifyLocals     :: (shared -> user -> IO user) -> IO ()
    , hModifyShared     :: (shared -> IO shared) -> IO shared
    , hModifyLocal      :: (user -> IO user) -> UserSession shared user -> IO (UserSession shared user)
    
    -- finalize and remove all user sessions. this should only be called upon shutdown or reset
    , hCloseAll         :: IO () 

    , hDumpLocals       :: IO [(SessionID, POSIXTime, user)]
    }

data UserSessionOptions = UserSessionOptions
    { usValidMinutes    :: Int
    , usCookieName      :: String
    -- TODO expose SetCookie options?
    }

data UserSessionFactory shared user = UserSessionFactory
    { usConstructShared :: IO shared 
    , usConstructLocal  :: shared -> SessionID -> IO (Maybe user)
    , usDestructLocal   :: shared -> user -> SessionID -> IO ()
    }
