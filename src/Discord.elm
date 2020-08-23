module Discord exposing
    ( Authentication, botToken, bearerToken
    , getChannel, deleteChannel, getMessages, getMessage, MessagesRelativeTo(..), createMessage, getReactions, createReaction, deleteOwnReaction, deleteUserReaction, deleteAllReactions, deleteAllReactionsForEmoji, deleteMessage, bulkDeleteMessage, Channel, PartialChannel, ChannelId, Message, MessageId, Reaction, Attachment, AttachmentId
    , Emoji, EmojiId
    , Guild, GuildId, GuildMember, RoleId, PartialGuild
    , Invite, InviteWithMetadata, InviteCode(..)
    , username, nickname, Username, Nickname, NameError(..), getCurrentUser, getCurrentUserGuilds, User, PartialUser, UserId, Permissions
    , WebhookId
    , ImageCdnConfig, Png(..), Jpg(..), WebP(..), Gif(..), Choices(..)
    , Bits, ChannelInviteConfig, CreateGuildCategoryChannel, CreateGuildTextChannel, CreateGuildVoiceChannel, DataUri(..), GuildModifications, GuildPreview, Id(..), ImageHash, ImageSize(..), Modify(..), OptionalData(..), Roles(..), UserDiscriminator(..), achievementIconUrl, addPinnedChannelMessage, applicationAssetUrl, applicationIconUrl, code, codeBlock, createChannelInvite, createGuildCategoryChannel, createGuildEmoji, createGuildTextChannel, createGuildVoiceChannel, customEmojiUrl, defaultChannelInviteConfig, defaultUserAvatarUrl, deleteChannelPermission, deleteGuild, deleteGuildEmoji, deleteInvite, deletePinnedChannelMessage, editMessage, getChannelInvites, getGuild, getGuildChannel, getGuildEmojis, getGuildMember, getGuildPreview, getInvite, getPinnedMessages, getUser, guildBannerUrl, guildDiscoverySplashUrl, guildIconUrl, guildSplashUrl, imageIsAnimated, leaveGuild, listGuildEmojis, listGuildMembers, mentionUser, modifyCurrentUser, modifyGuild, modifyGuildEmoji, nicknameErrorToString, nicknameToString, noGuildModifications, strikethrough, teamIconUrl, triggerTypingIndicator, userAvatarUrl, usernameErrorToString, usernameToString
    )

{-| Useful Discord links:

  - API documentation: <https://discord.com/developers/docs/intro>
    (A lot of their documentation has been reused here. Thanks Discord!)
  - Create bot invites: <https://discordapi.com/permissions.html>

Before starting, note that this package requires user credentials and creates tasks.
If I were evil (or my account got hacked) I could try to sneak in code that sends your Discord credentials to some other server.
For that reason it's probably a good idea to have a look at the source code and double check that it doesn't try anything sneaky!


# Authentication

@docs Authentication, botToken, bearerToken


# Audit Log


# Channel

@docs getChannel, deleteChannel, getMessages, getMessage, MessagesRelativeTo, createMessage, getReactions, createReaction, deleteOwnReaction, deleteUserReaction, deleteAllReactions, deleteAllReactionsForEmoji, deleteMessage, bulkDeleteMessage, Channel, PartialChannel, ChannelId, Message, MessageId, Reaction, Attachment, AttachmentId


# Emoji

@docs Emoji, EmojiId


# Guild

@docs getUsers, Guild, GuildId, GuildMember, RoleId, PartialGuild


# Invite

@docs Invite, InviteWithMetadata, InviteCode


# User

@docs username, nickname, Username, Nickname, NameError, getCurrentUser, getCurrentUserGuilds, User, PartialUser, UserId, Permissions


# Voice


# Webhook

@docs WebhookId


# CDN

These are functions that return a url pointing to a particular image.

@docs ImageCdnConfig, Png, Jpg, WebP, Gif, Choices, customEmoji, guildIcon, guildSplash, guildDiscoverySplash, guildBanner, defaultUserAvatar, userAvatar, applicationIcon, applicationAsset, achievementIcon, teamIcon

-}

import Binary
import Duration exposing (Seconds)
import Http
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Json.Encode.Extra as JE
import Quantity exposing (Quantity(..), Rate)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Posix(..))
import UInt64 exposing (UInt64)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)



--- CHANNEL ENDPOINTS ---


{-| Get a channel by ID.
-}
getChannel : Authentication -> Id ChannelId -> Task String Channel
getChannel authentication channelId =
    httpGet authentication decodeChannel [ "channels", rawIdAsString channelId ] []



-- Modify channel excluded


{-| Delete a channel, or close a private message.
Requires the `MANAGE_CHANNELS` permission for the guild.
Deleting a category does not delete its child channels; they will have their `parent_id` removed and a Channel Update Gateway event will fire for each of them.
Returns a channel object on success.
Fires a Channel Delete Gateway event.

Deleting a guild channel cannot be undone.
Use this with caution, as it is impossible to undo this action when performed on a guild channel.
In contrast, when used with a private message, it is possible to undo the action by opening a private message with the recipient again.

For Public servers, the set Rules or Guidelines channel and the Moderators-only (Public Server Updates) channel cannot be deleted.

-}
deleteChannel : Authentication -> Id ChannelId -> Task String Channel
deleteChannel authentication channelId =
    httpDelete authentication decodeChannel [ "channels", rawIdAsString channelId ] [] (JE.string "")


{-| Returns the messages for a channel.
If operating on a guild channel, this endpoint requires the `VIEW_CHANNEL` permission to be present on the current user.
If the current user is missing the `READ_MESSAGE_HISTORY` permission in the channel then this will return no messages (since they cannot read the message history).

  - channelId: The channel to get messages from
  - limit: Max number of messages to return (1-100)
  - relativeTo: Relative to which message should we retrieve messages?
    Or should we get the most recent messages?

-}
getMessages : Authentication -> { channelId : Id ChannelId, limit : Int, relativeTo : MessagesRelativeTo } -> Task String (List Message)
getMessages authentication { channelId, limit, relativeTo } =
    httpGet
        authentication
        (JD.list decodeMessage)
        [ "channels", rawIdAsString channelId, "messages" ]
        (Url.Builder.int "limit" limit
            :: (case relativeTo of
                    Around messageId ->
                        [ Url.Builder.string "around" (rawIdAsString messageId) ]

                    Before messageId ->
                        [ Url.Builder.string "before" (rawIdAsString messageId) ]

                    After messageId ->
                        [ Url.Builder.string "after" (rawIdAsString messageId) ]

                    MostRecent ->
                        []
               )
        )


{-| Returns a specific message in the channel.
If operating on a guild channel, this endpoint requires the `READ_MESSAGE_HISTORY` permission to be present on the current user.
-}
getMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String (List Message)
getMessage authentication { channelId, messageId } =
    httpGet
        authentication
        (JD.list decodeMessage)
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId ]
        []


{-| Before using this endpoint, you must connect to and identify with a gateway at least once.

Discord may strip certain characters from message content, like invalid unicode characters or characters which cause unexpected message formatting.
If you are passing user-generated strings into message content, consider sanitizing the data to prevent unexpected behavior and utilizing `allowed_mentions` to prevent unexpected mentions.

Post a message to a guild text or DM channel.
If operating on a guild channel, this endpoint requires the `SEND_MESSAGES` permission to be present on the current user.
If the tts field is set to true, the `SEND_TTS_MESSAGES` permission is required for the message to be spoken.
Returns a message object. Fires a Message Create Gateway event.
See message formatting for more information on how to properly format messages.

The maximum request size when sending a message is 8MB.

-}
createMessage : Authentication -> { channelId : Id ChannelId, content : String } -> Task String ()
createMessage authentication { channelId, content } =
    httpPost
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages" ]
        []
        (JE.object [ ( "content", JE.string content ) ])


{-| Create a reaction for the message.
This endpoint requires the `READ_MESSAGE_HISTORY` permission to be present on the current user.
Additionally, if nobody else has reacted to the message using this emoji, this endpoint requires the `ADD_REACTIONS` permission to be present on the current user.
-}
createReaction : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
createReaction authentication { channelId, messageId, emoji } =
    httpPut
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions", emoji, "@me" ]
        []
        (JE.object [])


{-| Delete a reaction the current user has made for the message.
-}
deleteOwnReaction : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
deleteOwnReaction authentication { channelId, messageId, emoji } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions", emoji, "@me" ]
        []
        (JE.object [])


{-| Deletes another user's reaction.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteUserReaction :
    Authentication
    -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String, userId : Id UserId }
    -> Task String ()
deleteUserReaction authentication { channelId, messageId, emoji, userId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions", emoji, rawIdAsString userId ]
        []
        (JE.object [])


{-| Get a list of users that reacted with this emoji.
-}
getReactions : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
getReactions authentication { channelId, messageId, emoji } =
    httpGet
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions", emoji ]
        [ Url.Builder.int "limit" 100 ]


{-| Deletes all reactions on a message.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteAllReactions : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
deleteAllReactions authentication { channelId, messageId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions" ]
        []
        (JE.object [])


{-| Deletes all the reactions for a given emoji on a message.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteAllReactionsForEmoji :
    Authentication
    -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String }
    -> Task String ()
deleteAllReactionsForEmoji authentication { channelId, messageId, emoji } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId, "reactions", emoji ]
        []
        (JE.object [])


{-| Edit a previously sent message. The fields content can only be edited by the original message author.
The content field can have a maximum of 2000 characters.
-}
editMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, content : String } -> Task String ()
editMessage authentication { channelId, messageId, content } =
    httpPatch
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId ]
        [ Url.Builder.string "content" content ]
        (JE.object [])


{-| Delete a message.
If operating on a guild channel and trying to delete a message that was not sent by the current user, this endpoint requires the `MANAGE_MESSAGES` permission.
-}
deleteMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
deleteMessage authentication { channelId, messageId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", rawIdAsString messageId ]
        []
        (JE.object [])


{-| Delete multiple messages in a single request.
This endpoint can only be used on guild channels and requires the `MANAGE_MESSAGES` permission.
Any message IDs given that do not exist or are invalid will count towards the minimum and maximum message count (currently 2 and 100 respectively).

This endpoint will not delete messages older than 2 weeks, and will fail with a 400 BAD REQUEST if any message provided is older than that or if any duplicate message IDs are provided.

-}
bulkDeleteMessage :
    Authentication
    ->
        { channelId : Id ChannelId
        , firstMessage : Id MessageId
        , secondMessage : Id MessageId
        , restOfMessages : List (Id MessageId)
        }
    -> Task String ()
bulkDeleteMessage authentication { channelId, firstMessage, secondMessage, restOfMessages } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "messages", "bulk-delete" ]
        []
        (JE.list JE.string (rawIdAsString firstMessage :: rawIdAsString secondMessage :: List.map rawIdAsString restOfMessages))



-- Edit Channel Permissions excluded


{-| Returns a list of invites for the channel.
Only usable for guild channels. Requires the `MANAGE_CHANNELS` permission.
-}
getChannelInvites : Authentication -> Id ChannelId -> Task String (List InviteWithMetadata)
getChannelInvites authentication channelId =
    httpGet
        authentication
        (JD.list decodeInviteWithMetadata)
        [ "channels", rawIdAsString channelId, "invites" ]
        []


{-| Default invite settings. Can be used an unlimited number of times but expires after 1 day.
-}
defaultChannelInviteConfig : ChannelInviteConfig
defaultChannelInviteConfig =
    { maxAge = Just (Quantity.round Duration.day)
    , maxUses = Nothing
    , temporaryMembership = False
    , unique = False
    , targetUser = Nothing
    }


{-| Create a new invite object for the channel. Only usable for guild channels.
Requires the `CREATE_INSTANT_INVITE` permission.
-}
createChannelInvite :
    Authentication
    -> Id ChannelId
    -> ChannelInviteConfig
    -> Task String Invite
createChannelInvite authentication channelId { maxAge, maxUses, temporaryMembership, unique, targetUser } =
    httpPost
        authentication
        decodeInvite
        [ "channels", rawIdAsString channelId, "invites" ]
        []
        (JE.object
            (( "max_age"
             , case maxAge of
                Just (Quantity maxAge_) ->
                    max 1 maxAge_ |> JE.int

                Nothing ->
                    JE.int 0
             )
                :: ( "max_uses"
                   , case maxUses of
                        Just maxUses_ ->
                            max 1 maxUses_ |> JE.int

                        Nothing ->
                            JE.int 0
                   )
                :: ( "temporary", JE.bool temporaryMembership )
                :: ( "unique", JE.bool unique )
                :: (case targetUser of
                        Just targetUserId ->
                            [ ( "target_user", JE.string (rawIdAsString targetUserId) ) ]

                        Nothing ->
                            []
                   )
            )
        )


{-| Delete a channel permission overwrite for a user or role in a channel.
Only usable for guild channels.
Requires the `MANAGE_ROLES` permission. For more information about permissions, see [permissions](https://discord.com/developers/docs/topics/permissions#permissions).
-}
deleteChannelPermission :
    Authentication
    -> { channelId : Id ChannelId, overwriteId : Id OverwriteId }
    -> Task String (List InviteWithMetadata)
deleteChannelPermission authentication { channelId, overwriteId } =
    httpDelete
        authentication
        (JD.list decodeInviteWithMetadata)
        [ "channels", rawIdAsString channelId, "permissions", rawIdAsString overwriteId ]
        []
        (JE.object [])


{-| Post a typing indicator for the specified channel.
Generally bots should not implement this route.
However, if a bot is responding to a command and expects the computation to take a few seconds, this endpoint may be called to let the user know that the bot is processing their message.
-}
triggerTypingIndicator : Authentication -> Id ChannelId -> Task String ()
triggerTypingIndicator authentication channelId =
    httpPost
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "typing" ]
        []
        (JE.object [])


{-| Returns all pinned messages in the channel.
-}
getPinnedMessages : Authentication -> Id ChannelId -> Task String (List Message)
getPinnedMessages authentication channelId =
    httpGet
        authentication
        (JD.list decodeMessage)
        [ "channels", rawIdAsString channelId, "pins" ]
        []


{-| Pin a message in a channel. Requires the `MANAGE_MESSAGES` permission.

The max pinned messages is 50.

-}
addPinnedChannelMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
addPinnedChannelMessage authentication { channelId, messageId } =
    httpPut
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "pins", rawIdAsString messageId ]
        []
        (JE.object [])


{-| Delete a pinned message in a channel. Requires the `MANAGE_MESSAGES` permission.
-}
deletePinnedChannelMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
deletePinnedChannelMessage authentication { channelId, messageId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawIdAsString channelId, "pins", rawIdAsString messageId ]
        []
        (JE.object [])



-- Group DM Add Recipient excluded
-- Group DM Remove Recipient excluded
--
--- EMOJI ENDPOINTS ---


{-| Returns a list of emojis for the given guild.
-}
listGuildEmojis : Authentication -> Id GuildId -> Task String (List Emoji)
listGuildEmojis authentication guildId =
    httpGet
        authentication
        (JD.list decodeEmoji)
        [ "guilds", rawIdAsString guildId, "emojis" ]
        []


{-| Returns an emoji for the given guild and emoji IDs.
-}
getGuildEmojis : Authentication -> { guildId : Id GuildId, emojiId : Id EmojiId } -> Task String Emoji
getGuildEmojis authentication { guildId, emojiId } =
    httpGet
        authentication
        decodeEmoji
        [ "guilds", rawIdAsString guildId, "emojis", rawIdAsString emojiId ]
        []


{-| Create a new emoji for the guild. Requires the `MANAGE_EMOJIS` permission.

  - emojiName: Name of the emoji
  - image: A 128x128 emoji image
  - roles: A list of roles in this guild that can use this emoji

Emojis and animated emojis have a maximum file size of 256kb.

-}
createGuildEmoji :
    Authentication
    -> { guildId : Id GuildId, emojiName : String, image : DataUri, roles : Roles }
    -> Task String Emoji
createGuildEmoji authentication { guildId, emojiName, image, roles } =
    httpPost
        authentication
        decodeEmoji
        [ "guilds", rawIdAsString guildId, "emojis" ]
        []
        (JE.object
            [ ( "name", JE.string emojiName )
            , ( "image", JE.string (rawDataUri image) )
            , ( "roles", encodeRoles roles )
            ]
        )


{-| Modify the given emoji. Requires the MANAGE\_EMOJIS permission.
-}
modifyGuildEmoji :
    Authentication
    ->
        { guildId : Id GuildId
        , emojiId : Id EmojiId
        , emojiName : Modify String
        , roles : Modify Roles
        }
    -> Task String Emoji
modifyGuildEmoji authentication { guildId, emojiId, emojiName, roles } =
    httpPost
        authentication
        decodeEmoji
        [ "guilds", rawIdAsString guildId, "emojis" ]
        []
        (JE.object
            ((case emojiName of
                Replace emojiName_ ->
                    [ ( "name", JE.string emojiName_ ) ]

                Unchanged ->
                    []
             )
                ++ (case roles of
                        Replace roles_ ->
                            [ ( "roles", encodeRoles roles_ ) ]

                        Unchanged ->
                            []
                   )
            )
        )


{-| Delete the given emoji. Requires the `MANAGE_EMOJIS` permission.
-}
deleteGuildEmoji : Authentication -> { guildId : Id GuildId, emojiId : Id EmojiId } -> Task String ()
deleteGuildEmoji authentication { guildId, emojiId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "guilds", rawIdAsString guildId, "emojis", rawIdAsString emojiId ]
        []
        (JE.object [])



--- GUILD ENDPOINTS ---


{-| Returns the guild for the given id.
-}
getGuild : Authentication -> Id GuildId -> Task String Guild
getGuild authentication guildId =
    httpGet
        authentication
        decodeGuild
        [ "guilds", rawIdAsString guildId ]
        []


{-| Returns a preview of a guild for the given id.

This endpoint is only for Public guilds

-}
getGuildPreview : Authentication -> Id GuildId -> Task String GuildPreview
getGuildPreview authentication guildId =
    httpGet
        authentication
        decodeGuildPreview
        [ "guilds", rawIdAsString guildId, "preview" ]
        []


noGuildModifications : GuildModifications
noGuildModifications =
    { name = Unchanged
    , region = Unchanged
    , verificationLevel = Unchanged
    , defaultMessageNotifications = Unchanged
    , explicitContentFilter = Unchanged
    , afkChannelId = Unchanged
    , afkTimeout = Unchanged
    , icon = Unchanged
    , ownerId = Unchanged
    , splash = Unchanged
    , banner = Unchanged
    , systemChannelId = Unchanged
    , rulesChannelId = Unchanged
    , publicUpdatesChannelId = Unchanged
    , preferredLocale = Unchanged
    }


{-| Modify a guild's settings. Requires the `MANAGE_GUILD` permission.

If you only plan on changing one or two things then I recommend this approach:

    import Discord exposing (Modify(..))

    noChanges =
        Discord.noGuildModifications

    changeGuildName =
        Discord.modifyGuild
            myAuth
            myGuildId
            { noChange | name = Replace "New Guild Name" }

-}
modifyGuild :
    Authentication
    -> Id GuildId
    -> GuildModifications
    -> Task String Guild
modifyGuild authentication guildId modifications =
    httpPatch
        authentication
        decodeGuild
        [ "guilds", rawIdAsString guildId ]
        []
        (JE.object
            (encodeModify "name" JE.string modifications.name
                ++ encodeModify "region" (JE.maybe JE.string) modifications.region
                ++ encodeModify "verification_level" (JE.maybe JE.int) modifications.verificationLevel
                ++ encodeModify "default_message_notifications" (JE.maybe JE.int) modifications.defaultMessageNotifications
                ++ encodeModify "explicit_content_filter" (JE.maybe JE.int) modifications.explicitContentFilter
                ++ encodeModify "afk_channel_id" (JE.maybe encodeId) modifications.afkChannelId
                ++ encodeModify "afk_timeout" encodeQuantityInt modifications.afkTimeout
                ++ encodeModify "icon" (JE.maybe encodeDataUri) modifications.icon
                ++ encodeModify "owner_id" encodeId modifications.ownerId
                ++ encodeModify "splash" (JE.maybe encodeDataUri) modifications.splash
                ++ encodeModify "banner" (JE.maybe encodeDataUri) modifications.banner
                ++ encodeModify "system_channel_id" (JE.maybe encodeId) modifications.systemChannelId
                ++ encodeModify "rules_channel_id" (JE.maybe encodeId) modifications.rulesChannelId
                ++ encodeModify "public_updates_channel_id" (JE.maybe encodeId) modifications.publicUpdatesChannelId
                ++ encodeModify "preferred_locale" (JE.maybe JE.string) modifications.preferredLocale
            )
        )


{-| Delete a guild permanently. User must be owner.
-}
deleteGuild : Authentication -> Id GuildId -> Task String ()
deleteGuild authentication guildId =
    httpDelete authentication (JD.succeed ()) [ "guilds", rawIdAsString guildId ] [] (JE.object [])


{-| Returns a list of guild channels.
-}
getGuildChannel : Authentication -> Id GuildId -> Task String (List Channel)
getGuildChannel authentication guildId =
    httpGet authentication (JD.list decodeChannel) [ "guilds", rawIdAsString guildId, "channels" ] []


{-| Create a new text channel for the guild. Requires the `MANAGE_CHANNELS` permission.
-}
createGuildTextChannel : Authentication -> CreateGuildTextChannel -> Task String Channel
createGuildTextChannel authentication config =
    httpPost
        authentication
        decodeChannel
        [ "guilds", rawIdAsString config.guildId, "channels" ]
        []
        (JE.object
            (( "name", JE.string config.name )
                :: ( "type", JE.int 0 )
                :: ( "topic", JE.string config.topic )
                :: ( "nsfw", JE.bool config.nsfw )
                :: encodeOptionalData "parent_id" encodeId config.parentId
                ++ encodeOptionalData "position" JE.int config.position
                ++ encodeOptionalData "rate_limit_per_user" encodeQuantityInt config.rateLimitPerUser
            )
        )


{-| Create a new voice channel for the guild. Requires the `MANAGE_CHANNELS` permission.
-}
createGuildVoiceChannel : Authentication -> CreateGuildVoiceChannel -> Task String Channel
createGuildVoiceChannel authentication config =
    httpPost
        authentication
        decodeChannel
        [ "guilds", rawIdAsString config.guildId, "channels" ]
        []
        (JE.object
            (( "name", JE.string config.name )
                :: ( "type", JE.int 2 )
                :: ( "topic", JE.string config.topic )
                :: ( "nsfw", JE.bool config.nsfw )
                :: encodeOptionalData "parent_id" encodeId config.parentId
                ++ encodeOptionalData "position" JE.int config.position
                ++ encodeOptionalData "bitrate" encodeQuantityInt config.bitrate
                ++ encodeOptionalData "user_limit" JE.int config.userLimit
            )
        )


{-| Create a new category for the guild that you can place other channels in.
Requires the `MANAGE_CHANNELS` permission.
-}
createGuildCategoryChannel : Authentication -> CreateGuildCategoryChannel -> Task String Channel
createGuildCategoryChannel authentication config =
    httpPost
        authentication
        decodeChannel
        [ "guilds", rawIdAsString config.guildId, "channels" ]
        []
        (JE.object
            (( "name", JE.string config.name )
                :: ( "type", JE.int 4 )
                :: encodeOptionalData "position" JE.int config.position
            )
        )



--Modify Guild Channel Positions excluded


{-| Returns a guild member for the specified user.
-}
getGuildMember : Authentication -> Id GuildId -> Id UserId -> Task String GuildMember
getGuildMember authentication guildId userId =
    httpGet
        authentication
        decodeGuildMember
        [ "guilds", rawIdAsString guildId, "members", rawIdAsString userId ]
        []


{-| Returns a list of guild members that are members of the guild.

  - limit: Max number of members to return (1-1000)
  - after: The highest user id in the previous page

-}
listGuildMembers :
    Authentication
    -> { guildId : Id GuildId, limit : Int, after : OptionalData (Id UserId) }
    -> Task String (List GuildMember)
listGuildMembers authentication { guildId, limit, after } =
    httpGet
        authentication
        (JD.list decodeGuildMember)
        [ "guilds", rawIdAsString guildId, "members" ]
        (Url.Builder.int "limit" limit
            :: (case after of
                    Included after_ ->
                        [ Url.Builder.string "after" (rawIdAsString after_) ]

                    Missing ->
                        []
               )
        )



--- INVITE ENDPOINTS ---


{-| Returns an invite for the given code.
-}
getInvite : Authentication -> InviteCode -> Task String Invite
getInvite authentication (InviteCode inviteCode) =
    httpGet
        authentication
        decodeInvite
        [ "invites", inviteCode ]
        [ Url.Builder.string "with_counts" "true" ]


{-| Delete an invite.
Requires the `MANAGE_CHANNELS` permission on the channel this invite belongs to, or `MANAGE_GUILD` to remove any invite across the guild.
-}
deleteInvite : Authentication -> InviteCode -> Task String Invite
deleteInvite authentication (InviteCode inviteCode) =
    httpDelete
        authentication
        decodeInvite
        [ "invites", inviteCode ]
        []
        (JE.object [])



--- USER ENDPOINTS ---


username : String -> Result NameError Username
username usernameText =
    if String.length usernameText < 2 then
        Err NameTooShort

    else if String.length usernameText > 32 then
        Err NameTooLong

    else if List.any (\substring -> String.contains substring usernameText) invalidNameSubstrings then
        Err NameContainsInvalidSubstring

    else if String.any (\char -> Set.member char invalidNameCharacters) usernameText then
        Err NameContainsInvalidCharacters

    else
        String.trim usernameText |> Username |> Ok


usernameToString : Username -> String
usernameToString (Username username_) =
    username_


mentionUser : Id UserId -> String
mentionUser id =
    "<@!" ++ rawIdAsString id ++ ">"


strikethrough : String -> String
strikethrough text =
    "~~" ++ text ++ "~~"


code : String -> String
code text =
    "`" ++ text ++ "`"


codeBlock : String -> String
codeBlock text =
    "```" ++ text ++ "```"


nickname : String -> Result NameError Nickname
nickname nicknameText =
    if String.length nicknameText < 1 then
        Err NameTooShort

    else if String.length nicknameText > 32 then
        Err NameTooLong

    else if List.any (\substring -> String.contains substring nicknameText) invalidNameSubstrings then
        Err NameContainsInvalidSubstring

    else if String.any (\char -> Set.member char invalidNameCharacters) nicknameText then
        Err NameContainsInvalidCharacters

    else
        String.trim nicknameText |> Nickname |> Ok


nicknameToString : Nickname -> String
nicknameToString (Nickname nickname_) =
    nickname_


usernameErrorToString : NameError -> String
usernameErrorToString usernameError =
    case usernameError of
        NameTooShort ->
            "Username is too short. Must be at least 2 characters long."

        NameTooLong ->
            "Username is too long. Must be 32 characters or shorter."

        NameContainsInvalidCharacters ->
            "Username contains invalid characters."

        NameContainsInvalidSubstring ->
            "Username contains an invalid substring."


nicknameErrorToString : NameError -> String
nicknameErrorToString nicknameError =
    case nicknameError of
        NameTooShort ->
            "Nickname is too short. Must be at least 1 character long."

        NameTooLong ->
            "Nickname is too long. Must be 32 characters or shorter."

        NameContainsInvalidCharacters ->
            "Nickname contains invalid characters."

        NameContainsInvalidSubstring ->
            "Nickname contains an invalid substring."


{-| Returns the user object of the requester's account.
For OAuth2, this requires the identify scope, which will return the object without an email, and optionally the email scope, which returns the object with an email.
-}
getCurrentUser : Authentication -> Task String User
getCurrentUser authentication =
    httpGet
        authentication
        decodeUser
        [ "users", "@me" ]
        []


{-| Returns a user object for a given user ID.
-}
getUser : Authentication -> Id UserId -> Task String User
getUser authentication userId =
    httpGet authentication decodeUser [ "users", rawIdAsString userId ] []


{-| Modify the requester's user account settings.

  - username: The user's username. If changed, may cause the [`user's discriminator`](#UserDiscriminator) to be randomized.
  - avatar: Modifies the user's avatar (aka profile picture)

-}
modifyCurrentUser :
    Authentication
    -> { username : Modify Username, avatar : Modify (Maybe DataUri) }
    -> Task String User
modifyCurrentUser authentication modifications =
    httpPatch
        authentication
        decodeUser
        [ "users", "@me" ]
        []
        (JE.object
            (encodeModify "username" encodeUsername modifications.username
                ++ encodeModify "avatar" (JE.maybe encodeDataUri) modifications.avatar
            )
        )


{-| Returns a list of partial guilds the current user is a member of. Requires the guilds OAuth2 scope.
-}
getCurrentUserGuilds : Authentication -> Task String (List PartialGuild)
getCurrentUserGuilds authentication =
    httpGet
        authentication
        (JD.list decodePartialGuild)
        [ "users", "@me", "guilds" ]
        []


{-| Leave a guild.
-}
leaveGuild : Authentication -> Id GuildId -> Task String ()
leaveGuild authentication guildId =
    httpDelete
        authentication
        (JD.succeed ())
        [ "users", "@me", "guilds", rawIdAsString guildId ]
        []
        (JE.object [])



-- Get User DMs excluded
-- Create DM excluded
-- Create Group DM excluded
-- Get User Connections excluded
--- VOICE ENDPOINTS ---
--- WEBHOOK ENDPOINTS ---
--- CDN ENDPOINTS ---


imageIsAnimated : ImageHash hashType -> Bool
imageIsAnimated (ImageHash hash) =
    String.startsWith "a_" hash


customEmojiUrl : ImageCdnConfig (Choices Png Gif Never Never) -> Id EmojiId -> String
customEmojiUrl { size, imageType } emojiId =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "emojis", rawIdAsString emojiId ++ imageExtensionPngGif imageType ]
        (imageSizeQuery size)


guildIconUrl : ImageCdnConfig (Choices Png Jpg WebP Gif) -> Id GuildId -> ImageHash IconHash -> String
guildIconUrl { size, imageType } guildId iconHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "icons", rawIdAsString guildId, rawHash iconHash ++ imageExtensionPngJpgWebpGif imageType ]
        (imageSizeQuery size)


guildSplashUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id GuildId -> ImageHash SplashHash -> String
guildSplashUrl { size, imageType } guildId splashHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "splashes", rawIdAsString guildId, rawHash splashHash ++ imageExtensionPngJpgWebp imageType ]
        (imageSizeQuery size)


guildDiscoverySplashUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id GuildId -> ImageHash DiscoverySplashHash -> String
guildDiscoverySplashUrl { size, imageType } guildId discoverySplashHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "discovery-splashes", rawIdAsString guildId, rawHash discoverySplashHash ++ imageExtensionPngJpgWebp imageType ]
        (imageSizeQuery size)


guildBannerUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id GuildId -> ImageHash BannerHash -> String
guildBannerUrl { size, imageType } guildId splashHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "banners", rawIdAsString guildId, rawHash splashHash ++ imageExtensionPngJpgWebp imageType ]
        (imageSizeQuery size)


defaultUserAvatarUrl : ImageSize -> Id UserId -> UserDiscriminator -> String
defaultUserAvatarUrl size guildId (UserDiscriminator discriminator) =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "embed", "avatars", rawIdAsString guildId, String.fromInt (modBy 5 discriminator) ++ ".png" ]
        (imageSizeQuery size)


userAvatarUrl : ImageCdnConfig (Choices Png Jpg WebP Gif) -> Id UserId -> ImageHash AvatarHash -> String
userAvatarUrl { size, imageType } guildId avatarHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "avatars", rawIdAsString guildId, rawHash avatarHash ++ imageExtensionPngJpgWebpGif imageType ]
        (imageSizeQuery size)


applicationIconUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id ApplicationId -> ImageHash ApplicationIconHash -> String
applicationIconUrl { size, imageType } applicationId applicationIconHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "app-icons", rawIdAsString applicationId, rawHash applicationIconHash ++ imageExtensionPngJpgWebp imageType ]
        (imageSizeQuery size)


applicationAssetUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id ApplicationId -> ImageHash ApplicationAssetHash -> String
applicationAssetUrl { size, imageType } applicationId applicationAssetHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "app-assets", rawIdAsString applicationId, rawHash applicationAssetHash ++ imageExtensionPngJpgWebp imageType ]
        (imageSizeQuery size)


achievementIconUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id ApplicationId -> Id AchievementId -> ImageHash AchievementIconHash -> String
achievementIconUrl { size, imageType } applicationId achievementId achievementIconHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "app-assets"
        , rawIdAsString applicationId
        , "achievements"
        , rawIdAsString achievementId
        , "icons"
        , rawHash achievementIconHash ++ imageExtensionPngJpgWebp imageType
        ]
        (imageSizeQuery size)


teamIconUrl : ImageCdnConfig (Choices Png Jpg WebP Never) -> Id TeamId -> ImageHash TeamIconHash -> String
teamIconUrl { size, imageType } teamId teamIconHash =
    Url.Builder.crossOrigin
        discordCdnUrl
        [ "team-icons", rawIdAsString teamId, rawHash teamIconHash ++ ".png" ]
        (imageSizeQuery size)



--- MISCELLANEOUS ---


discordApiUrl : String
discordApiUrl =
    "https://discord.com/api"


discordCdnUrl : String
discordCdnUrl =
    "https://cdn.discordapp.com"


{-| Looks something like this `MTk4NjIyNDgzNDcxOTI1MjQ4.Cl2FMQ.ZnCjm1XVW7vRze4b7Cq4se7kKWs`.
See the [Discord documentation](https://discord.com/developers/docs/reference#authentication) for more info.
-}
botToken : String -> Authentication
botToken =
    BotToken


{-| Looks something like this `CZhtkLDpNYXgPH9Ml6shqh2OwykChw`.
See the [Discord documentation](https://discord.com/developers/docs/reference#authentication) for more info.
-}
bearerToken : String -> Authentication
bearerToken =
    BearerToken


rawIdAsString : Id idType -> String
rawIdAsString (Id id) =
    UInt64.toString id


rawHash : ImageHash hashType -> String
rawHash (ImageHash hash) =
    hash


httpPost : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPost authentication decoder path queryParameters body =
    http authentication "POST" decoder path queryParameters (Http.jsonBody body)


httpPut : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPut authentication decoder path queryParameters body =
    http authentication "PUT" decoder path queryParameters (Http.jsonBody body)


httpPatch : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPatch authentication decoder path queryParameters body =
    http authentication "DELETE" decoder path queryParameters (Http.jsonBody body)


httpDelete : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpDelete authentication decoder path queryParameters body =
    http authentication "DELETE" decoder path queryParameters (Http.jsonBody body)


httpGet : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> Task String a
httpGet authentication decoder path queryParameters =
    http authentication "GET" decoder path queryParameters Http.emptyBody


http : Authentication -> String -> JD.Decoder a -> List String -> List QueryParameter -> Http.Body -> Task String a
http authentication requestType decoder path queryParameters body =
    Http.task
        { method = requestType
        , headers =
            [ Http.header "Authorization"
                (case authentication of
                    BotToken token ->
                        "Bot " ++ token

                    BearerToken token ->
                        "Bearer " ++ token
                )
            ]
        , url =
            Url.Builder.crossOrigin
                discordApiUrl
                (List.map Url.percentEncode path)
                queryParameters
        , resolver = Http.stringResolver (resolver decoder)
        , body = body
        , timeout = Nothing
        }


resolver : JD.Decoder a -> Http.Response String -> Result String a
resolver decoder response =
    case response of
        Http.BadUrl_ badUrl ->
            Err ("Bad url " ++ badUrl)

        Http.Timeout_ ->
            Err "Timeout"

        Http.NetworkError_ ->
            Err "Network error"

        Http.BadStatus_ metadata body ->
            "Bad status " ++ String.fromInt metadata.statusCode ++ "      " ++ body |> Err

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Ok data ->
                    Ok data

                Err error ->
                    JD.errorToString error |> Err


rawDataUri : DataUri -> String
rawDataUri (DataUri dataUri) =
    dataUri


imageExtensionPngGif : Choices Png Gif Never Never -> String
imageExtensionPngGif choice =
    case choice of
        Choice1 _ ->
            ".png"

        _ ->
            ".gif"


imageExtensionPngJpgWebpGif : Choices Png Jpg WebP Gif -> String
imageExtensionPngJpgWebpGif choice =
    case choice of
        Choice1 _ ->
            ".png"

        Choice2 _ ->
            ".jpg"

        Choice3 _ ->
            ".webp"

        Choice4 _ ->
            ".gif"


imageExtensionPngJpgWebp : Choices Png Jpg WebP Never -> String
imageExtensionPngJpgWebp choice =
    case choice of
        Choice1 _ ->
            ".png"

        Choice2 _ ->
            ".jpg"

        _ ->
            ".webp"


imageSizeQuery : ImageSize -> List QueryParameter
imageSizeQuery size =
    case size of
        TwoToNthPower size_ ->
            2 ^ size_ |> clamp 16 4096 |> Url.Builder.int "size" |> List.singleton

        DefaultImageSize ->
            []


invalidNameSubstrings : List String
invalidNameSubstrings =
    [ "discordtag", "everyone", "here", "```" ]


invalidNameCharacters : Set Char
invalidNameCharacters =
    Set.fromList [ '@', '#', ':' ]



--- TYPES ---


type Authentication
    = BotToken String
    | BearerToken String


type OptionalData a
    = Included a
    | Missing


type alias Guild =
    { id : Id GuildId
    , name : String
    , icon : Maybe (ImageHash IconHash)
    , splash : Maybe (ImageHash SplashHash)
    , discoverySplash : Maybe (ImageHash DiscoverySplashHash)
    , owner : OptionalData Bool
    , ownerId : Id UserId
    , permissions : OptionalData Permissions
    , region : String
    , afkChannelId : Maybe (Id ChannelId)
    , afkTimeout : Quantity Int Seconds
    , embedEnabled : OptionalData Bool
    , embedChannelId : OptionalData (Maybe (Id ChannelId))
    , verificationLevel : Int
    , defaultMessageNotifications : Int
    , explicitContentFilter : Int

    -- roles field excluded
    , emojis : List Emoji
    , features : List String
    , mfaLevel : Int
    , applicationId : Maybe (Id ApplicationId)
    , widgetEnabled : OptionalData Bool
    , widgetChannelId : OptionalData (Maybe (Id ChannelId))
    , systemChannelId : Maybe (Id ChannelId)
    , systemChannelFlags : Int
    , rulesChannelId : Maybe (Id ChannelId)
    , joinedAt : OptionalData Time.Posix
    , large : OptionalData Bool
    , unavailable : OptionalData Bool
    , memberCount : OptionalData Int

    -- voiceStates field excluded
    , members : OptionalData (List GuildMember)
    , channels : OptionalData (List Channel)

    -- presences field excluded
    , maxPresences : OptionalData (Maybe Int)
    , maxMembers : OptionalData Int
    , vanityUrlCode : Maybe String
    , description : Maybe String
    , banner : Maybe (ImageHash BannerHash)
    , premiumTier : Int
    , premiumSubscriptionCount : OptionalData Int
    , preferredLocale : String
    , publicUpdatesChannelId : Maybe (Id ChannelId)
    , approximateMemberCount : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    }


type Nickname
    = Nickname String


type alias GuildMember =
    { user : OptionalData User
    , nickname : Maybe Nickname
    , roles : List (Id RoleId)
    , joinedAt : Time.Posix
    , premiumSince : OptionalData (Maybe Time.Posix)
    , deaf : Bool
    , mute : Bool
    }


type alias PartialGuild =
    { id : Id GuildId
    , name : String
    , icon : Maybe (ImageHash IconHash)
    , owner : Bool
    , permissions : Permissions
    }


type alias GuildPreview =
    { id : Id GuildId
    , name : String
    , icon : Maybe (ImageHash IconHash)
    , splash : Maybe (ImageHash SplashHash)
    , discoverySplash : Maybe (ImageHash DiscoverySplashHash)
    , emojis : List Emoji
    , features : List String
    , approximateMemberCount : Int
    , approximatePresenceCount : Int
    , description : Maybe String
    }


type alias Reaction =
    { count : Int
    , me : Bool
    , emoji : Emoji
    }


type alias Emoji =
    { id : Id EmojiId
    , name : Maybe String
    , roles : List (Id RoleId)
    , user : OptionalData User
    , requireColons : OptionalData Bool
    , managed : OptionalData Bool
    , animated : OptionalData Bool
    , available : OptionalData Bool
    }


type Bits
    = Bits Never


type alias Channel =
    { id : Id ChannelId
    , type_ : ChannelType
    , guildId : OptionalData (Id GuildId)
    , position : OptionalData Int

    -- premission overwrites field excluded
    , name : OptionalData String
    , topic : OptionalData (Maybe String)
    , nsfw : OptionalData Bool
    , lastMessageId : OptionalData (Maybe (Id MessageId))
    , bitrate : OptionalData (Quantity Int (Rate Bits Seconds))
    , userLimit : OptionalData Int
    , rateLimitPerUser : OptionalData (Quantity Int Seconds)
    , recipients : OptionalData (List User)
    , icon : OptionalData (Maybe String)
    , ownerId : OptionalData (Id UserId)
    , applicationId : OptionalData (Id ApplicationId)
    , parentId : OptionalData (Maybe (Id ChannelId))
    , lastPinTimestamp : OptionalData Time.Posix
    }


type alias PartialChannel =
    { id : Id ChannelId
    , name : String
    , type_ : ChannelType
    }


type ChannelType
    = GuildText
    | DirectMessage
    | GuildVoice
    | GroupDirectMessage
    | GuildCategory
    | GuildNews
    | GuildStore


type alias Invite =
    { code : InviteCode
    , guild : OptionalData PartialGuild
    , channel : PartialChannel
    , inviter : OptionalData User
    , targetUser : OptionalData PartialUser
    , targetUserType : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    , approximateMemberCount : OptionalData Int
    }


type alias InviteWithMetadata =
    { code : InviteCode
    , guild : OptionalData PartialGuild
    , channel : PartialChannel
    , inviter : OptionalData User
    , targetUser : OptionalData PartialUser
    , targetUserType : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    , approximateMemberCount : OptionalData Int
    , uses : Int
    , maxUses : Int
    , maxAge : Maybe (Quantity Int Seconds)
    , temporaryMembership : Bool
    , createdAt : Time.Posix
    }


{-| -maxAge: Duration of invite in before it expires. `Nothing` means it never expires.
-maxUsers: Max number of uses. `Nothing` means it has unlimited uses.
-temporaryMembership: Whether this invite only grants temporary membership.
-unique: If true, don't try to reuse a similar invite (useful for creating many unique one time use invites).
-targetUser: The target user id for this invite.
-}
type alias ChannelInviteConfig =
    { maxAge : Maybe (Quantity Int Seconds)
    , maxUses : Maybe Int
    , temporaryMembership : Bool
    , unique : Bool
    , targetUser : Maybe (Id UserId)
    }


type alias Permissions =
    { createInstantInvite : Bool
    , kickMembers : Bool
    , banMembers : Bool
    , administrator : Bool
    , manageChannels : Bool
    , manageGuild : Bool
    , addReaction : Bool
    , viewAuditLog : Bool
    , prioritySpeaker : Bool
    , stream : Bool
    , viewChannel : Bool
    , sendMessages : Bool
    , sentTextToSpeechMessages : Bool
    , manageMessages : Bool
    , embedLinks : Bool
    , attachFiles : Bool
    , readMessageHistory : Bool
    , mentionEveryone : Bool
    , useExternalEmojis : Bool
    , viewGuildInsights : Bool
    , connect : Bool
    , speak : Bool
    , muteMembers : Bool
    , deafenMembers : Bool
    , moveMembers : Bool
    , useVoiceActivityDetection : Bool
    , changeNickname : Bool
    , manageNicknames : Bool
    , manageRoles : Bool
    , manageWebhooks : Bool
    , manageEmojis : Bool
    }


type alias Attachment =
    { id : Id AttachmentId
    , filename : String
    , size : Int
    , url : String
    , proxyUrl : String
    , height : Maybe Int
    , width : Maybe Int
    }


type Username
    = Username String


type NameError
    = NameTooShort
    | NameTooLong
    | NameContainsInvalidCharacters
    | NameContainsInvalidSubstring


type alias User =
    { id : Id UserId
    , username : Username
    , discriminator : UserDiscriminator
    , avatar : Maybe (ImageHash AvatarHash)
    , bot : OptionalData Bool
    , system : OptionalData Bool
    , mfaEnabled : OptionalData Bool
    , locale : OptionalData String
    , verified : OptionalData Bool
    , email : OptionalData (Maybe String)
    , flags : OptionalData Int
    , premiumType : OptionalData Int
    , publicFlags : OptionalData Int
    }


type alias PartialUser =
    { id : Id UserId
    , username : Username
    , avatar : Maybe (ImageHash AvatarHash)
    , discriminator : UserDiscriminator
    }


type ImageHash hashType
    = ImageHash String


type AvatarHash
    = AvatarHash Never


type BannerHash
    = BannerHash Never


type IconHash
    = IconHash Never


type SplashHash
    = SplashHash Never


type DiscoverySplashHash
    = DiscoverSplashHash Never


type AchievementIconHash
    = AchievementIconHash Never


type ApplicationAssetHash
    = ApplicationAssetHash Never


type TeamIconHash
    = TeamIconHash Never


type ApplicationIconHash
    = ApplicationIconHash Never


{-| In Discord's documentation these are called snowflakes. They are always 64bit positive integers.
-}
type Id idType
    = Id UInt64


type MessageId
    = MessageId Never


type UserId
    = UserId Never


type RoleId
    = RoleId Never


type ChannelId
    = ChannelId Never


type GuildId
    = GuildId Never


type WebhookId
    = WebhookId Never


type AttachmentId
    = AttachmentId Never


type EmojiId
    = EmojiId Never


type ApplicationId
    = ApplicationId Never


type OverwriteId
    = OverwriteId Never


type TeamId
    = TeamId Never


type AchievementId
    = AchievementId Never


type InviteCode
    = InviteCode String


type alias Message =
    { id : Id MessageId
    , channelId : Id ChannelId
    , guildId : OptionalData (Id GuildId)
    , author : User

    -- member field is excluded
    , content : String
    , timestamp : Time.Posix
    , editedTimestamp : Maybe Time.Posix
    , textToSpeech : Bool
    , mentionEveryone : Bool

    -- mentions field is excluded
    , mentionRoles : List (Id RoleId)

    -- mention_channels field is excluded
    , attachments : List Attachment

    -- embeds field is excluded
    , reactions : OptionalData (List Reaction)

    -- nonce field is excluded
    , pinned : Bool
    , webhookId : OptionalData (Id WebhookId)
    , type_ : Int

    -- activity field is excluded
    -- application field is excluded
    -- message_reference field is excluded
    , flags : OptionalData Int
    }


{-| -}
type MessagesRelativeTo
    = Around (Id MessageId)
    | Before (Id MessageId)
    | After (Id MessageId)
    | MostRecent


type Modify a
    = Replace a
    | Unchanged


type Roles
    = RoleList (List (Id RoleId))
    | AllRoles


{-| A [data URI](https://en.wikipedia.org/wiki/Data_URI_scheme) (they look like this `data:image/jpeg;base64,BASE64_ENCODED_JPEG_IMAGE_DATA`)
-}
type DataUri
    = DataUri String


type UserDiscriminator
    = UserDiscriminator Int


type alias GuildModifications =
    { name : Modify String
    , region : Modify (Maybe String)
    , verificationLevel : Modify (Maybe Int)
    , defaultMessageNotifications : Modify (Maybe Int)
    , explicitContentFilter : Modify (Maybe Int)
    , afkChannelId : Modify (Maybe (Id ChannelId))
    , afkTimeout : Modify (Quantity Int Seconds)
    , icon : Modify (Maybe DataUri)
    , ownerId : Modify (Id UserId)
    , splash : Modify (Maybe DataUri)
    , banner : Modify (Maybe DataUri)
    , systemChannelId : Modify (Maybe (Id ChannelId))
    , rulesChannelId : Modify (Maybe (Id ChannelId))
    , publicUpdatesChannelId : Modify (Maybe (Id ChannelId))
    , preferredLocale : Modify (Maybe String)
    }


{-| Specify the size of an image you want to get a link to.
It can either be the default size of the image or a size in the form of `n ^ 2` (the resulting image size will get clamped between 16 and 4096)
-}
type ImageSize
    = DefaultImageSize
    | TwoToNthPower Int


{-| Choose the image size and image file type.
The available image types is shown in a function's type signature.

    import Discord exposing (Choices(..), Gif(..), ImageSize)


    -- Returns a url that points to a 32px (2^5) large, gif file of our custom emoji.
    myEmoji =
        Discord.customEmojiUrl { size = TwoToNthPower 5, imageType = Choice2 Gif }

-}
type alias ImageCdnConfig imageTypeChoices =
    { size : ImageSize
    , imageType : imageTypeChoices
    }


type Choices a b c d
    = Choice1 a
    | Choice2 b
    | Choice3 c
    | Choice4 d


type Png
    = Png


type Gif
    = Gif


type Jpg
    = Jpg


type WebP
    = WebP


type alias CreateGuildTextChannel =
    { guildId : Id GuildId
    , name : String
    , topic : String
    , nsfw : Bool
    , position : OptionalData Int
    , parentId : OptionalData (Id ChannelId)
    , rateLimitPerUser : OptionalData (Quantity Int Seconds)
    }


type alias CreateGuildVoiceChannel =
    { guildId : Id GuildId
    , name : String
    , topic : String
    , nsfw : Bool
    , position : OptionalData Int
    , parentId : OptionalData (Id ChannelId)
    , bitrate : OptionalData (Quantity Int (Rate Bits Seconds))
    , userLimit : OptionalData Int
    }


type alias CreateGuildCategoryChannel =
    { guildId : Id GuildId
    , name : String
    , position : OptionalData Int
    }



--- DECODERS ---


decodeGuildMember : JD.Decoder GuildMember
decodeGuildMember =
    JD.succeed GuildMember
        |> JD.andMap (decodeOptionalData "user" decodeUser)
        |> JD.andMap (JD.field "nick" (JD.nullable decodeNickname))
        |> JD.andMap (JD.field "roles" (JD.list decodeSnowflake))
        |> JD.andMap (JD.field "joined_at" Iso8601.decoder)
        |> JD.andMap (decodeOptionalData "premium_since" (JD.nullable Iso8601.decoder))
        |> JD.andMap (JD.field "deaf" JD.bool)
        |> JD.andMap (JD.field "mute" JD.bool)


decodeOptionalData : String -> JD.Decoder a -> JD.Decoder (OptionalData a)
decodeOptionalData field decoder =
    JD.oneOf
        [ JD.field field decoder |> JD.map Included
        , JD.field field (JD.fail ("Incorrect data for field: " ++ field))
        , JD.succeed Missing
        ]


decodeSnowflake : JD.Decoder (Id idType)
decodeSnowflake =
    JD.andThen
        (UInt64.fromString
            >> Maybe.map (Id >> JD.succeed)
            >> Maybe.withDefault (JD.fail "Invalid snowflake ID.")
        )
        JD.string


decodeHash : JD.Decoder (ImageHash hashType)
decodeHash =
    JD.map ImageHash JD.string


decodeMessage : JD.Decoder Message
decodeMessage =
    JD.succeed Message
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "channel_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "guild_id" decodeSnowflake)
        |> JD.andMap (JD.field "author" decodeUser)
        |> JD.andMap (JD.field "content" JD.string)
        |> JD.andMap (JD.field "timestamp" Iso8601.decoder)
        |> JD.andMap (JD.field "edited_timestamp" (JD.nullable Iso8601.decoder))
        |> JD.andMap (JD.field "tts" JD.bool)
        |> JD.andMap (JD.field "mention_everyone" JD.bool)
        |> JD.andMap (JD.field "mention_roles" (JD.list decodeSnowflake))
        |> JD.andMap (JD.field "attachments" (JD.list decodeAttachment))
        |> JD.andMap (decodeOptionalData "reactions" (JD.list decodeReaction))
        |> JD.andMap (JD.field "pinned" JD.bool)
        |> JD.andMap (decodeOptionalData "webhook_id" decodeSnowflake)
        |> JD.andMap (JD.field "type" JD.int)
        |> JD.andMap (decodeOptionalData "flags" JD.int)


decodeUser : JD.Decoder User
decodeUser =
    JD.succeed User
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "username" decodeUsername)
        |> JD.andMap (JD.field "discriminator" decodeDiscriminator)
        |> JD.andMap (JD.field "avatar" (JD.nullable decodeHash))
        |> JD.andMap (decodeOptionalData "bot" JD.bool)
        |> JD.andMap (decodeOptionalData "system" JD.bool)
        |> JD.andMap (decodeOptionalData "mfa_enabled" JD.bool)
        |> JD.andMap (decodeOptionalData "locale" JD.string)
        |> JD.andMap (decodeOptionalData "verified" JD.bool)
        |> JD.andMap (decodeOptionalData "email" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "flags" JD.int)
        |> JD.andMap (decodeOptionalData "premium_type" JD.int)
        |> JD.andMap (decodeOptionalData "public_flags" JD.int)


decodeUsername : JD.Decoder Username
decodeUsername =
    JD.andThen
        (\name ->
            case username name of
                Ok username_ ->
                    JD.succeed username_

                Err error ->
                    JD.fail ("Invalid username. " ++ usernameErrorToString error)
        )
        JD.string


decodeNickname : JD.Decoder Nickname
decodeNickname =
    JD.andThen
        (\name ->
            case nickname name of
                Ok nickname_ ->
                    JD.succeed nickname_

                Err error ->
                    JD.fail ("Invalid username. " ++ nicknameErrorToString error)
        )
        JD.string


decodeAttachment : JD.Decoder Attachment
decodeAttachment =
    JD.succeed Attachment
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "filename" JD.string)
        |> JD.andMap (JD.field "size" JD.int)
        |> JD.andMap (JD.field "url" JD.string)
        |> JD.andMap (JD.field "proxy_url" JD.string)
        |> JD.andMap
            {- Sometimes the width and height don't get included even though Discord's documentation says they should.
               If that happens, we just pretend we did get that field and it contained a null value.
            -}
            (decodeOptionalData "height" (JD.nullable JD.int)
                |> JD.map flattenMaybeOptional
            )
        |> JD.andMap
            (decodeOptionalData "width" (JD.nullable JD.int)
                |> JD.map flattenMaybeOptional
            )


flattenMaybeOptional : OptionalData (Maybe a) -> Maybe a
flattenMaybeOptional optionalData =
    case optionalData of
        Included maybe ->
            maybe

        Missing ->
            Nothing


decodeReaction : JD.Decoder Reaction
decodeReaction =
    JD.succeed Reaction
        |> JD.andMap (JD.field "count" JD.int)
        |> JD.andMap (JD.field "me" JD.bool)
        |> JD.andMap (JD.field "emoji" decodeEmoji)


decodeEmoji : JD.Decoder Emoji
decodeEmoji =
    JD.succeed Emoji
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" (JD.nullable JD.string))
        |> JD.andMap (JD.field "roles" (JD.list decodeSnowflake))
        |> JD.andMap (decodeOptionalData "user" decodeUser)
        |> JD.andMap (decodeOptionalData "require_colons" JD.bool)
        |> JD.andMap (decodeOptionalData "managed" JD.bool)
        |> JD.andMap (decodeOptionalData "animated" JD.bool)
        |> JD.andMap (decodeOptionalData "available" JD.bool)


decodePartialGuild : JD.Decoder PartialGuild
decodePartialGuild =
    JD.succeed PartialGuild
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" JD.string)
        |> JD.andMap (JD.field "icon" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "owner" JD.bool)
        |> JD.andMap (JD.field "permissions" decodePermissions)


decodeGuild : JD.Decoder Guild
decodeGuild =
    JD.succeed Guild
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" JD.string)
        |> JD.andMap (JD.field "icon" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "splash" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "discovery_splash" (JD.nullable decodeHash))
        |> JD.andMap (decodeOptionalData "owner" JD.bool)
        |> JD.andMap (JD.field "owner_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "permissions" decodePermissions)
        |> JD.andMap (JD.field "region" JD.string)
        |> JD.andMap (JD.field "afk_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "afk_timeout" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "embed_enabled" JD.bool)
        |> JD.andMap (decodeOptionalData "embed_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "verification_level" JD.int)
        |> JD.andMap (JD.field "default_message_notifications" JD.int)
        |> JD.andMap (JD.field "explicit_content_filter" JD.int)
        |> JD.andMap (JD.field "emojis" (JD.list decodeEmoji))
        |> JD.andMap (JD.field "features" (JD.list JD.string))
        |> JD.andMap (JD.field "mfa_level" JD.int)
        |> JD.andMap (JD.field "application_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "widget_enabled" JD.bool)
        |> JD.andMap (decodeOptionalData "widget_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "system_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "system_channel_flags" JD.int)
        |> JD.andMap (JD.field "rules_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "joined_at" Iso8601.decoder)
        |> JD.andMap (decodeOptionalData "large" JD.bool)
        |> JD.andMap (decodeOptionalData "unavailable" JD.bool)
        |> JD.andMap (decodeOptionalData "member_count" JD.int)
        |> JD.andMap (decodeOptionalData "members" (JD.list decodeGuildMember))
        |> JD.andMap (decodeOptionalData "channels" (JD.list decodeChannel))
        |> JD.andMap (decodeOptionalData "max_presences" (JD.nullable JD.int))
        |> JD.andMap (decodeOptionalData "max_members" JD.int)
        |> JD.andMap (JD.field "vanity_url_code" (JD.nullable JD.string))
        |> JD.andMap (JD.field "description" (JD.nullable JD.string))
        |> JD.andMap (JD.field "banner" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "premium_tier" JD.int)
        |> JD.andMap (decodeOptionalData "premium_subscription_count" JD.int)
        |> JD.andMap (JD.field "preferred_locale" JD.string)
        |> JD.andMap (JD.field "public_updates_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "approximate_member_count" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_presence_count" JD.int)


decodePermissions : JD.Decoder Permissions
decodePermissions =
    JD.map
        (\value ->
            let
                permissions =
                    value |> List.singleton |> Binary.fromIntegers |> Binary.toBooleans

                getPermission position =
                    List.drop position permissions |> List.head |> Maybe.withDefault False
            in
            Permissions
                (getPermission 0)
                (getPermission 1)
                (getPermission 2)
                (getPermission 3)
                (getPermission 4)
                (getPermission 5)
                (getPermission 6)
                (getPermission 7)
                (getPermission 8)
                (getPermission 9)
                (getPermission 10)
                (getPermission 11)
                (getPermission 12)
                (getPermission 13)
                (getPermission 14)
                (getPermission 15)
                (getPermission 16)
                (getPermission 17)
                (getPermission 18)
                (getPermission 19)
                (getPermission 20)
                (getPermission 21)
                (getPermission 22)
                (getPermission 23)
                (getPermission 24)
                (getPermission 25)
                (getPermission 26)
                (getPermission 27)
                (getPermission 28)
                (getPermission 29)
                (getPermission 30)
        )
        JD.int


decodeChannel : JD.Decoder Channel
decodeChannel =
    JD.succeed Channel
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "type" decodeChannelType)
        |> JD.andMap (decodeOptionalData "guild_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "position" JD.int)
        |> JD.andMap (decodeOptionalData "name" JD.string)
        |> JD.andMap (decodeOptionalData "topic" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "nsfw" JD.bool)
        |> JD.andMap (decodeOptionalData "last_message_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "bitrate" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "user_limit" JD.int)
        |> JD.andMap (decodeOptionalData "rate_limit_per_user" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "recipients" (JD.list decodeUser))
        |> JD.andMap (decodeOptionalData "icon" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "owner_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "application_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "parent_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "last_pin_timestamp" Iso8601.decoder)


decodeChannelType : JD.Decoder ChannelType
decodeChannelType =
    JD.andThen
        (\value ->
            case value of
                0 ->
                    JD.succeed GuildText

                1 ->
                    JD.succeed DirectMessage

                2 ->
                    JD.succeed GuildVoice

                3 ->
                    JD.succeed GroupDirectMessage

                4 ->
                    JD.succeed GuildCategory

                5 ->
                    JD.succeed GuildNews

                6 ->
                    JD.succeed GuildStore

                _ ->
                    JD.fail "Invalid channel type."
        )
        JD.int


decodeInviteCode : JD.Decoder InviteCode
decodeInviteCode =
    JD.map InviteCode JD.string


decodeInvite : JD.Decoder Invite
decodeInvite =
    JD.map8 Invite
        (JD.field "code" decodeInviteCode)
        (decodeOptionalData "guild" decodePartialGuild)
        (JD.field "channel" decodePartialChannel)
        (decodeOptionalData "inviter" decodeUser)
        (decodeOptionalData "target_user" decodePartialUser)
        (decodeOptionalData "target_user_type" JD.int)
        (decodeOptionalData "approximate_presence_count" JD.int)
        (decodeOptionalData "approximate_member_count" JD.int)


decodeInviteWithMetadata : JD.Decoder InviteWithMetadata
decodeInviteWithMetadata =
    JD.succeed InviteWithMetadata
        |> JD.andMap (JD.field "code" decodeInviteCode)
        |> JD.andMap (decodeOptionalData "guild" decodePartialGuild)
        |> JD.andMap (JD.field "channel" decodePartialChannel)
        |> JD.andMap (decodeOptionalData "inviter" decodeUser)
        |> JD.andMap (decodeOptionalData "target_user" decodePartialUser)
        |> JD.andMap (decodeOptionalData "target_user_type" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_presence_count" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_member_count" JD.int)
        |> JD.andMap (JD.field "uses" JD.int)
        |> JD.andMap (JD.field "max_uses" JD.int)
        |> JD.andMap
            (JD.field "max_age"
                (JD.map
                    (\value ->
                        if value == 0 then
                            Nothing

                        else
                            Just (Quantity value)
                    )
                    JD.int
                )
            )
        |> JD.andMap (JD.field "temporary" JD.bool)
        |> JD.andMap (JD.field "created_at" Iso8601.decoder)


decodePartialChannel : JD.Decoder PartialChannel
decodePartialChannel =
    JD.map3 PartialChannel
        (JD.field "id" decodeSnowflake)
        (JD.field "name" JD.string)
        (JD.field "type" decodeChannelType)


decodePartialUser : JD.Decoder PartialUser
decodePartialUser =
    JD.map4 PartialUser
        (JD.field "id" decodeSnowflake)
        (JD.field "username" decodeUsername)
        (JD.field "avatar" (JD.nullable decodeHash))
        (JD.field "discriminator" decodeDiscriminator)


decodeDiscriminator : JD.Decoder UserDiscriminator
decodeDiscriminator =
    JD.andThen
        (\text ->
            case String.toInt text of
                Just value ->
                    JD.succeed (UserDiscriminator value)

                Nothing ->
                    JD.fail "Invalid discriminator"
        )
        JD.string


decodeGuildPreview : JD.Decoder GuildPreview
decodeGuildPreview =
    JD.succeed GuildPreview
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" JD.string)
        |> JD.andMap (JD.field "icon" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "splash" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "discovery_splash" (JD.nullable decodeHash))
        |> JD.andMap (JD.field "emojis" (JD.list decodeEmoji))
        |> JD.andMap (JD.field "features" (JD.list JD.string))
        |> JD.andMap (JD.field "approximate_member_count" JD.int)
        |> JD.andMap (JD.field "approximate_presence_count" JD.int)
        |> JD.andMap (JD.field "description" (JD.nullable JD.string))



--- ENCODERS ---


encodeId : Id idType -> JE.Value
encodeId id =
    JE.string (rawIdAsString id)


encodeRoles : Roles -> JE.Value
encodeRoles roles =
    case roles of
        RoleList roles_ ->
            JE.list encodeId roles_

        AllRoles ->
            JE.null


encodeModify : String -> (a -> JE.Value) -> Modify a -> List ( String, JE.Value )
encodeModify fieldName encoder modify =
    case modify of
        Replace value ->
            [ ( fieldName, encoder value ) ]

        Unchanged ->
            []


encodeUsername : Username -> JE.Value
encodeUsername =
    usernameToString >> JE.string


encodeDataUri : DataUri -> JE.Value
encodeDataUri (DataUri dataUri) =
    JE.string dataUri


encodeQuantityInt : Quantity Int units -> JE.Value
encodeQuantityInt (Quantity quantity) =
    JE.int quantity


encodeOptionalData : String -> (a -> JE.Value) -> OptionalData a -> List ( String, JE.Value )
encodeOptionalData fieldName encoder optionalData =
    case optionalData of
        Included value ->
            [ ( fieldName, encoder value ) ]

        Missing ->
            []
