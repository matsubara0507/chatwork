{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE  FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module MockServer
    ( mockServer
    ) where

import           ChatWork.Test.Values
import           ChatWork.Types
import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS (length)
import           Data.Int                 (Int64)
import           Network.Wai.Handler.Warp
import           Servant

type API = "me" :> Get '[JSON] (Headers '[Header "Content-Length" Int64] Me)
      :<|> "my" :> "status" :> Get '[JSON] (Headers '[Header "Content-Length" Int64] MyStatus)
      :<|> "my" :> "tasks" :> Get '[JSON] (Headers '[Header "Content-Length" Int64] MyTasks)
      :<|> "contacts" :> Get '[JSON] (Headers '[Header "Content-Length" Int64] Contacts)
      :<|> "incoming_requests" :> Get '[JSON] (Headers '[Header "Content-Length" Int64] IncomingRequests)
      :<|> "incoming_requests" :> Capture "request_id" Int :> Put '[JSON] (Headers '[Header "Content-Length" Int64] AcceptedIncomingRequest)
      :<|> "incoming_requests" :> Capture "request_id" Int :> Delete '[JSON] (Headers '[Header "Content-Length" Int64] ())

api :: Proxy API
api = Proxy

server :: Server API
server = getMe :<|> getMyStatus :<|> getMyTasks :<|> getContacts
    :<|> getIncomingRequests :<|> acceptIncomingRequest :<|> rejectIncomingRequest
  where
    getMe = return $ addHeader (LBS.length $ encode me) me
    getMyStatus = return $ addHeader (LBS.length $ encode myStatus) myStatus
    getMyTasks = return $ addHeader (LBS.length $ encode myTasks) myTasks
    getContacts = return $ addHeader (LBS.length $ encode contacts) contacts
    getIncomingRequests = return $ addHeader (LBS.length $ encode incomingRequests) incomingRequests
    acceptIncomingRequest _ = return $ addHeader (LBS.length $ encode acceptedIncomingRequest) acceptedIncomingRequest
    rejectIncomingRequest _ = return $ addHeader (LBS.length $ encode ()) ()

mockServer :: IO ()
mockServer = run 8000 (serve api server)
