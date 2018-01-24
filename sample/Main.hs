{-# LANGUAGE OverloadedStrings #-}

import           ChatWork
import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class
import           Network.HTTP.Req       (responseBody, runReq)

main :: IO ()
main = runReq def $ do
  let
    token  = "xxx"
    client = ChatWorkClient token
  (Right myId) <- (fmap meToAccountId . responseBody) <$> getMe client
  (Right ids) <-
      (fmap (fmap contactToAccountId) . responseBody) <$> getContacts client
  (Right roomId) <- (fmap getRoomId . responseBody) <$>
      createRoom client (CreateRoomParams Nothing Nothing [myId] (Just ids) Nothing "test")
  _ <- postMessage client roomId "Wellcome to Test Room !!"
  _ <- createTask client roomId
      (CreateTaskParams "Self-Introduction" Nothing ids)

  liftIO . putStrLn $
    "I Created Test Room and Tasks that Self-Introductions to my contact members."
