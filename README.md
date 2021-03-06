# chatwork

[![Hackage](https://img.shields.io/hackage/v/chatwork.svg?style=flat)](https://hackage.haskell.org/package/chatwork)
[![Build Status](https://travis-ci.org/matsubara0507/chatwork.svg?branch=master)](https://travis-ci.org/matsubara0507/chatwork)
[![Stackage LTS](http://stackage.org/package/chatwork/badge/lts)](http://stackage.org/lts/package/chatwork)
[![Stackage Nightly](http://stackage.org/package/chatwork/badge/nightly)](http://stackage.org/nightly/package/chatwork)

The ChatWork API for Haskell.

## example

```haskell
>> :module Network.HTTP.Req ChatWork
>> token = ChatWorkClient "xxx"
>> responseBody <$> runReq def (getMe token)
Right (Me {meToAccountId = 1234567, meToRoomId = 9876543, meToName = "\26494\21407\20449\24544", meToChatworkId = "", meToOrganizationId = 13579, meToOrganizationName = "", meToDepartment = "", meToTitle = "", meToUrl = "", meToIntroduction = "", meToMail = "", meToTelOrganization = "", meToTelExtension = "", meToTelMobile = "", meToSkype = "", meToFacebook = "", meToTwitter = "", meToAvatarImageUrl = "https://appdata.chatwork.com/avatar/1234/12345678.rsz.png"})
>> responseBody <$> runReq def (createRoom token $ CreateRoomParams Nothing Nothing [1234567] Nothing Nothing "test")
Right (RoomIdWrap {getRoomId = 78381630})
>> responseBody <$> runReq def (postMessage token 78381630 "hello")
Right (MessageIdWrap {getMessageId = "930765966558646272"})
```
