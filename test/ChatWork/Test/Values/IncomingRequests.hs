{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Test.Values.IncomingRequests
    ( incomingRequests
    , acceptedIncomingRequest
    ) where

import           ChatWork.Types (AcceptedIncomingRequest (..),
                                 IncomingRequest (..), IncomingRequests)

incomingRequests :: IncomingRequests
incomingRequests = [ IncomingRequest 123
                                     363
                                     "hogehoge"
                                     "John Smith"
                                     "tarochatworkid"
                                     101
                                     "Hello Company"
                                     "Marketing"
                                     "https://example.com/abc.png"
                   ]

acceptedIncomingRequest :: AcceptedIncomingRequest
acceptedIncomingRequest = AcceptedIncomingRequest 363
                                                  1234
                                                  "John Smith"
                                                  "tarochatworkid"
                                                  101
                                                  "Hello Company"
                                                  "Marketing"
                                                  "https://example.com/abc.png"
