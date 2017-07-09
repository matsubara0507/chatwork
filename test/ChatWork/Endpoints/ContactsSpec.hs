module ChatWork.Endpoints.ContactsSpec
    ( main
    , spec
    ) where

import           ChatWork.Endpoints.Contacts   (getContacts)
import           ChatWork.MonadHttpIO          ()
import           ChatWork.Test.Client          (TestClient (..))
import           ChatWork.Test.MockServer      (runMockServer)
import           ChatWork.Test.Values.Contacts (contacts)
import           Network.HTTP.Req              (responseBody)
import           Test.Hspec                    (Spec, around_, context,
                                                describe, hspec, it,
                                                shouldReturn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ runMockServer $ do
  describe "getContacts: endpoint GET /contacts" $ do
    context "correct responce" $ do
      it "should return Right contacts response body" $ do
        (responseBody <$> getContacts TestClient) `shouldReturn` Right contacts
