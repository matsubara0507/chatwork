{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.RoomsSpec
    ( main
    , spec
    ) where

import           ChatWork.Endpoints.Rooms   (createRoom, createTask, deleteRoom,
                                             getFile, getFiles, getMembers,
                                             getMessage, getMessages, getRoom,
                                             getRoomTask, getRoomTasks,
                                             getRooms, leaveRoom, postMessage,
                                             updateMembersPermission,
                                             updateRoom)
import           ChatWork.MonadHttpIO       ()
import           ChatWork.Test.Client       (TestClient (..))
import           ChatWork.Test.MockServer   (runMockServer)
import           ChatWork.Test.Values.Rooms (file, files, members,
                                             membersPermission, message,
                                             messageId, messages, roomDetail,
                                             roomId, roomTask, roomTasks, rooms,
                                             taskIds, createRoomParams, updateRoomParams, createTaskParams, roomMembersParams, getTasksParams)
import           Network.HTTP.Req           (responseBody)
import           Test.Hspec                 (Spec, around_, context, describe,
                                             hspec, it, shouldReturn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ runMockServer $ do
  describe "getRooms: endpoint GET /rooms" $ do
    context "correct responce" $ do
      it "should return Right rooms response body" $ do
        (responseBody <$> getRooms TestClient) `shouldReturn` Right rooms
  describe "createRoom: endpoint POST /rooms" $ do
    context "correct responce" $ do
      it "should return Right room response body" $ do
        (responseBody <$> createRoom TestClient createRoomParams) `shouldReturn` Right roomId
  describe "getRoom: endpoint GET /rooms/{room_id}" $ do
    context "correct responce" $ do
      it "should return Right roomDetail response body" $ do
        (responseBody <$> getRoom TestClient 123) `shouldReturn` Right roomDetail
  describe "updateRoom: endpoint PUT /rooms/{room_id}" $ do
    context "correct responce" $ do
      it "should return Right roomId response body" $ do
        (responseBody <$> updateRoom TestClient 123 updateRoomParams) `shouldReturn` Right roomId
  describe "deleteRoom: endpoint DELETE /rooms/{room_id}" $ do
    context "correct responce" $ do
      it "should return Right () response body" $ do
        (responseBody <$> deleteRoom TestClient 123) `shouldReturn` Right ()
  describe "leaveRoom: endpoint DELETE /rooms/{room_id}" $ do
    context "correct responce" $ do
      it "should return Right () response body" $ do
        (responseBody <$> leaveRoom TestClient 123) `shouldReturn` Right ()
  describe "getMembers: endpoint GET /rooms/{room_id}/members" $ do
    context "correct responce" $ do
      it "should return Right members response body" $ do
        (responseBody <$> getMembers TestClient 123) `shouldReturn` Right members
  describe "updateMembersPermission: endpoint PUT /rooms/{room_id}/members" $ do
    context "correct responce" $ do
      it "should return Right membersPermission response body" $ do
        (responseBody <$> updateMembersPermission TestClient 123 roomMembersParams) `shouldReturn` Right membersPermission
  describe "getMessages: endpoint GET /rooms/{room_id}/messages?force=0" $ do
    context "correct responce" $ do
      it "should return Right messages response body" $ do
        (responseBody <$> getMessages TestClient 123 (Just False)) `shouldReturn` Right messages
  describe "postMessage: ndpoint POST /rooms/{room_id}/messages" $ do
    context "correct responce" $ do
      it "should return Right messageId response body" $ do
        (responseBody <$> postMessage TestClient 123 "Hello ChatWork!") `shouldReturn` Right messageId
  describe "getMessage: ndpoint GET /rooms/{room_id}/messages/{message_id}" $ do
    context "correct responce" $ do
      it "should return Right message response body" $ do
        (responseBody <$> getMessage TestClient 123 "5") `shouldReturn` Right message
  describe "getRoomTasks: endpoint GET /rooms/{room_id}/tasks?..." $ do
    context "correct responce" $ do
      it "should return Right roomTasks response body" $ do
        (responseBody <$> getRoomTasks TestClient 123 getTasksParams) `shouldReturn` Right roomTasks
  describe "createTask: endpoint POST /rooms/{room_id}/tasks" $ do
    context "correct responce" $ do
      it "should return Right taskIds response body" $ do
        (responseBody <$> createTask TestClient 123 createTaskParams) `shouldReturn` Right taskIds
  describe "getRoomTask: endpoint GET /rooms/{room_id}/tasks/{tasks_id}" $ do
    context "correct responce" $ do
      it "should return Right roomTask response body" $ do
        (responseBody <$> getRoomTask TestClient 123 3) `shouldReturn` Right roomTask
  describe "getFiles: endpoint GET /rooms/{room_id}/files?..." $ do
    context "correct responce" $ do
      it "should return Right files response body" $ do
        (responseBody <$> getFiles TestClient 123 (Just 101)) `shouldReturn` Right files
  describe "getfile: endpoint GET /rooms/{room_id}/files/{files_id}?..." $ do
    context "correct responce" $ do
      it "should return Right file response body" $ do
        (responseBody <$> getFile TestClient 123 3 (Just True)) `shouldReturn` Right file
