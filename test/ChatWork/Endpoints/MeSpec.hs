module ChatWork.Endpoints.MeSpec
    ( spec
    ) where

import           ChatWork.Endpoints.Me     (getMe)
import           ChatWork.Test.Client      (TestClient (..))
import           ChatWork.Test.MonadHttpIO ()
import           ChatWork.Test.Values.Me   (me)
import           Network.HTTP.Req          (responseBody)
import           Test.Hspec                (Spec, context, describe, it,
                                            shouldReturn)

spec :: Spec
spec = do
    describe "getMe: endpoint GET /me" $ do
      context "correct responce" $ do
        it "should return Right me response body" $ do
          (responseBody <$> getMe TestClient) `shouldReturn` Right me
