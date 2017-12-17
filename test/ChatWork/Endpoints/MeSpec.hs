module ChatWork.Endpoints.MeSpec
    ( main
    , spec
    ) where

import           ChatWork.Endpoints.Me     (getMe)
import           ChatWork.Test.Client      (TestClient (..))
import           ChatWork.Test.MockServer  (runMockServer)
import           ChatWork.Test.MonadHttpIO ()
import           ChatWork.Test.Values.Me   (me)
import           Network.HTTP.Req          (responseBody)
import           Test.Hspec                (Spec, around_, context, describe,
                                            hspec, it, shouldReturn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ runMockServer $ do
    describe "getMe: endpoint GET /me" $ do
      context "correct responce" $ do
        it "should return Right me response body" $ do
          (responseBody <$> getMe TestClient) `shouldReturn` Right me
