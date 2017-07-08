{-# LANGUAGE OverloadedStrings #-}

import           ChatWork
import           ChatWork.Test.Client
import           ChatWork.Test.Values
import           Control.Concurrent
import           Control.Exception
import           MockServer           (mockServer)
import           Network.HTTP.Req
import           Test.Hspec

main :: IO ()
main = hspec . around_ runMockServer $ do
    describe "endpoint GET /me" $ do
      it "correct responce" $ do
        (responseBody <$> getMe TestClient) `shouldReturn` Right me
    describe "endpoint GET /my/status" $ do
      it "correct responce" $ do
        (responseBody <$> getMyStatus TestClient) `shouldReturn` Right myStatus
    describe "endpoint GET /my/tasks" $ do
      it "correct responce" $ do
        (responseBody <$> getMyTasks TestClient) `shouldReturn` Right myTasks
    describe "endpoint GET /contacts" $ do
      it "correct responce" $ do
        (responseBody <$> getContacts TestClient) `shouldReturn` Right contacts
    describe "endpoint GET /incoming_requests" $ do
      it "correct responce" $ do
        (responseBody <$> getIncomingRequests TestClient) `shouldReturn` Right incomingRequests
    describe "endpoint GET /incoming_requests/{request_id}" $ do
      it "correct responce" $ do
        (responseBody <$> acceptIncomingRequest TestClient 123) `shouldReturn` Right acceptedIncomingRequest
    describe "endpoint DELETE /incoming_requests/{request_id}" $ do
      it "correct responce" $ do
        (responseBody <$> rejectIncomingRequest TestClient 123) `shouldReturn` Right ()
    describe "endpoint GET /rooms" $ do
      it "correct responce" $ do
        (responseBody <$> getRooms TestClient) `shouldReturn` Right rooms
    describe "endpoint POST /rooms" $ do
      it "correct responce" $ do
        (responseBody <$> createRoom TestClient createRoomParams) `shouldReturn` Right roomId
    describe "endpoint GET /rooms/{room_id}" $ do
      it "correct responce" $ do
        (responseBody <$> getRoom TestClient 123) `shouldReturn` Right roomDetail
    describe "endpoint PUT /rooms/{room_id}" $ do
      it "correct responce" $ do
        (responseBody <$> updateRoom TestClient 123 updateRoomParams) `shouldReturn` Right roomId
    describe "endpoint DELETE /rooms/{room_id}" $ do
      it "correct responce: delete" $ do
        (responseBody <$> deleteRoom TestClient 123) `shouldReturn` Right ()
      it "correct responce: leave" $ do
        (responseBody <$> leaveRoom TestClient 123) `shouldReturn` Right ()
    describe "endpoint GET /rooms/{room_id}/members" $ do
      it "correct responce" $ do
        (responseBody <$> getMembers TestClient 123) `shouldReturn` Right members
    describe "endpoint PUT /rooms/{room_id}/members" $ do
      it "correct responce" $ do
        (responseBody <$> updateMembersPermission TestClient 123 roomMembersParams) `shouldReturn` Right membersPermission
    describe "endpoint GET /rooms/{room_id}/messages?force=0" $ do
      it "correct responce" $ do
        (responseBody <$> getMessages TestClient 123 (Just False)) `shouldReturn` Right messages
    describe "endpoint POST /rooms/{room_id}/messages" $ do
      it "correct responce" $ do
        (responseBody <$> postMessage TestClient 123 "Hello ChatWork!") `shouldReturn` Right messageId
    describe "endpoint GET /rooms/{room_id}/messages/{message_id}" $ do
      it "correct responce" $ do
        (responseBody <$> getMessage TestClient 123 "5") `shouldReturn` Right message
    describe "endpoint GET /rooms/{room_id}/tasks?..." $ do
      it "correct responce" $ do
        (responseBody <$> getRoomTasks TestClient 123 getTasksParams) `shouldReturn` Right roomTasks
    describe "endpoint POST /rooms/{room_id}/tasks" $ do
      it "correct responce" $ do
        (responseBody <$> createTask TestClient 123 createTaskParams) `shouldReturn` Right taskIds
    describe "endpoint GET /rooms/{room_id}/tasks/{tasks_id}" $ do
      it "correct responce" $ do
        (responseBody <$> getRoomTask TestClient 123 3) `shouldReturn` Right roomTask
    describe "endpoint GET /rooms/{room_id}/files?..." $ do
      it "correct responce" $ do
        (responseBody <$> getFiles TestClient 123 (Just 101)) `shouldReturn` Right files
    describe "endpoint GET /rooms/{room_id}/files/{files_id}?..." $ do
      it "correct responce" $ do
        (responseBody <$> getFile TestClient 123 3 (Just True)) `shouldReturn` Right file
  where
    runMockServer action = do
      tid <- forkIO mockServer
      action `finally` killThread tid
