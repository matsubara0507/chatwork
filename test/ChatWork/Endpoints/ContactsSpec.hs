module ChatWork.Endpoints.ContactsSpec
    ( spec
    ) where

import           ChatWork.Endpoints.Contacts   (getContacts)
import           ChatWork.Test.Client          (TestClient (..))
import           ChatWork.Test.MonadHttpIO     ()
import           ChatWork.Test.Values.Contacts (contacts)
import           Network.HTTP.Req              (responseBody)
import           Test.Hspec                    (Spec, context, describe, it,
                                                shouldReturn)

spec :: Spec
spec = do
  describe "getContacts: endpoint GET /contacts" $ do
    context "correct responce" $ do
      it "should return Right contacts response body" $ do
        (responseBody <$> getContacts TestClient) `shouldReturn` Right contacts
