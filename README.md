# chatwork

The ChatWork API for Haskell.

## example

```haskell
>> :module Network.HTTP.Req ChatWork.Endpoints ChatWork.MonadHttpIO ChatWork.Types
>> token = "xxx"
>> print =<< (responseBody <$> getMe  token)
Right (GetMeResponse {meAccountId = 1234567, meRoomId = 9876543, meName = "\26494\21407\20449\24544", meChatworkId = "", meOrganizationId = 13579, meOrganizationName = "", meDepartment = "", meTitle = "", meUrl = "", meIntroduction = "", meMail = "", meTelOrganization = "", meTelExtension = "", meTelMobile = "", meSkype = "", meFacebook = "", meTwitter = "", meAvatarImageUrl = "https://appdata.chatwork.com/avatar/1234/12345678.rsz.png"})
>> print =<< (responseBody <$> postRoom token (CreateRoomParams Nothing Nothing [1234567] Nothing Nothing "test"))
Right (RoomIdWrap {getRoomId = 78381630})
>> print =<< (responseBody <$> postRoomMessage token 78381630 "hello")
Right (MessageIdWrap {getMessageId = "930765966558646272"})
```
