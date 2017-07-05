# chatwork

The ChatWork API for Haskell.

## example

```haskell
>> :module Network.HTTP.Req ChatWork
>> token = ChatWorkClient "xxx"
>> print =<< (responseBody <$> getMe  token)
Right (Me {meToAccountId = 1234567, meToRoomId = 9876543, meToName = "\26494\21407\20449\24544", meToChatworkId = "", meToOrganizationId = 13579, meToOrganizationName = "", meToDepartment = "", meToTitle = "", meToUrl = "", meToIntroduction = "", meToMail = "", meToTelOrganization = "", meToTelExtension = "", meToTelMobile = "", meToSkype = "", meToFacebook = "", meToTwitter = "", meToAvatarImageUrl = "https://appdata.chatwork.com/avatar/1234/12345678.rsz.png"})
>> print =<< (responseBody <$> createRoom token (CreateRoomParams Nothing Nothing [1234567] Nothing Nothing "test"))
Right (RoomIdWrap {getRoomId = 78381630})
>> print =<< (responseBody <$> postMessage token 78381630 "hello")
Right (MessageIdWrap {getMessageId = "930765966558646272"})
```
