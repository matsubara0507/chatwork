{-# LANGUAGE OverloadedStrings #-}

import           ChatWork
import           Network.HTTP.Req (responseBody)

main :: IO ()
main = do
  let
    token  = "xxx"
    client = ChatWorkClient token
  (Right myId) <- (fmap meToAccountId . responseBody) <$> getMe client
  (Right ids) <-
      (fmap (fmap contactToAccountId) . responseBody) <$> getContacts client
  (Right roomId) <- (fmap getRoomId . responseBody) <$> createRoom client
      (CreateRoomParams Nothing Nothing [myId] (Just ids) Nothing "test")
  _ <- postMessage client roomId "Wellcome to Test Room !!"
  _ <- createTask client roomId
      (CreateTaskParams "Self-Introduction" Nothing ids)

  putStrLn $
    "I Created Test Room and Tasks that Self-Introductions to my contact members."
