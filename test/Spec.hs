module Main where

import           Test.Hspec

import qualified ChatWork.Endpoints.ContactsSpec
import qualified ChatWork.Endpoints.IncomingRequestsSpec
import qualified ChatWork.Endpoints.MeSpec
import qualified ChatWork.Endpoints.MySpec
import qualified ChatWork.Endpoints.RoomsSpec
import           ChatWork.Test.MockServer                (runMockServer)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ runMockServer $ do
  describe "ChatWork.Endpoints.Contacts" ChatWork.Endpoints.ContactsSpec.spec
  describe "ChatWork.Endpoints.IncomingRequests" ChatWork.Endpoints.IncomingRequestsSpec.spec
  describe "ChatWork.Endpoints.Me" ChatWork.Endpoints.MeSpec.spec
  describe "ChatWork.Endpoints.My" ChatWork.Endpoints.MySpec.spec
  describe "ChatWork.Endpoints.Rooms" ChatWork.Endpoints.RoomsSpec.spec
