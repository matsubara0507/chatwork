module ChatWork.Endpoints.MySpec
    ( spec
    ) where

import           ChatWork.Endpoints.My     (getMyStatus, getMyTasks)
import           ChatWork.Test.Client      (TestClient (..))
import           ChatWork.Test.MonadHttpIO ()
import           ChatWork.Test.Values.My   (getMyTasksParams, myStatus, myTasks)
import           Network.HTTP.Req          (responseBody)
import           Test.Hspec                (Spec, context, describe, it,
                                            shouldReturn)

spec :: Spec
spec = do
  describe "getMyStatus: endpoint GET /my/status" $ do
    context "correct responce" $ do
      it "should return Right myStatus response body" $ do
        (responseBody <$> getMyStatus TestClient) `shouldReturn` Right myStatus
  describe "getMyTasks: endpoint GET /my/tasks" $ do
    context "correct responce" $ do
      it "should return Right myTasks response body" $ do
        (responseBody <$> getMyTasks TestClient getMyTasksParams) `shouldReturn` Right myTasks
