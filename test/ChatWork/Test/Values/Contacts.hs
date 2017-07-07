{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Test.Values.Contacts
    ( contacts
    ) where

import           ChatWork.Types (Contact (..), Contacts)

contacts :: Contacts
contacts = [ Contact 123
                     322
                     "John Smith"
                     "tarochatworkid"
                     101
                     "Hello Company"
                     "Marketing"
                     "https://example.com/abc.png"
           ]
