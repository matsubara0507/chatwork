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
        (responseBody <$> acceptIncomingRequests TestClient 123) `shouldReturn` Right acceptedIncomingRequest
    describe "endpoint DELETE /incoming_requests/{request_id}" $ do
      it "correct responce" $ do
        (responseBody <$> rejectIncomingRequests TestClient 123) `shouldReturn` Right ()
  where
    runMockServer action = do
      tid <- forkIO mockServer
      action `finally` killThread tid
