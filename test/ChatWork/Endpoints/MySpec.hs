module ChatWork.Endpoints.MySpec
    ( main
    , spec
    ) where

import           ChatWork.Endpoints.My    (getMyStatus, getMyTasks)
import           ChatWork.MonadHttpIO     ()
import           ChatWork.Test.Client     (TestClient (..))
import           ChatWork.Test.MockServer (runMockServer)
import           ChatWork.Test.Values.My  (myStatus, myTasks)
import           Network.HTTP.Req         (responseBody)
import           Test.Hspec               (Spec, around_, context, describe,
                                           hspec, it, shouldReturn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ runMockServer $ do
  describe "getMyStatus: endpoint GET /my/status" $ do
    context "correct responce" $ do
      it "should return Right myStatus response body" $ do
        (responseBody <$> getMyStatus TestClient) `shouldReturn` Right myStatus
  describe "getMyTasks: endpoint GET /my/tasks" $ do
    context "correct responce" $ do
      it "should return Right myTasks response body" $ do
        (responseBody <$> getMyTasks TestClient) `shouldReturn` Right myTasks
