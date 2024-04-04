module Youtube

open Data
open FSharp.Data
open Google.Apis.Services
open Google.Apis.Util
open Google.Apis.YouTube.v3
open Google.Apis.YouTube.v3.Data
open System
open Thoth.Json.Net

let youtubeService =
    let initializer = BaseClientService.Initializer()
    initializer.ApiKey <- config.YoutubeKey |> Option.defaultValue ""
    new YouTubeService(initializer)

let getYoutubeChannelId (channel: string) =
    task {
        let searchRequest =
            ChannelsResource.ListRequest(
                youtubeService,
                Repeatable([| "id" |]),
                Id = Repeatable([| channel |]),
                MaxResults = 1L
            )

        let! response = searchRequest.ExecuteAsync()

        let channelId =
            match response.Items with
            | null -> None
            | items when items.Count > 0 -> Some items.[0].Id
            | _ -> None

        return
            match channelId with
            | Some id -> Some id
            | None ->
                let searchRequest =
                    ChannelsResource.ListRequest(
                        youtubeService,
                        Repeatable([| "id" |]),
                        ForHandle = channel,
                        MaxResults = 1L
                    )

                match searchRequest.Execute().Items with
                | null -> None
                | items when items.Count > 0 -> Some items.[0].Id
                | _ -> None
    }

let getChannelTitleByChannelId (channelId: string) =
    task {
        let searchRequest =
            ChannelsResource.ListRequest(
                youtubeService,
                Repeatable([| "brandingSettings" |]),
                Id = Repeatable([| channelId |]),
                MaxResults = 1L
            )

        let! response = searchRequest.ExecuteAsync()

        return
            match response.Items with
            | null -> None
            | items when items.Count > 0 ->
                Some items.[0].BrandingSettings.Channel.Title
            | _ -> None
    }

let getYoutubeUpdates () : string option list =
    [ for channelId in (getDb ()).YoutubeChannels do
          let listRequest =
              ActivitiesResource.ListRequest(
                  youtubeService,
                  Repeatable([| "snippet"; "contentDetails"; "id" |]),
                  ChannelId = channelId,
                  MaxResults = 50L,
                  PublishedAfter =
                      (Map.tryFind channelId (getDb ()).LastYoutubeFetch
                       |> Option.defaultValue (DateTime.Now.AddHours(-1)))
              )

          updateDb
              { (getDb ()) with
                  LastYoutubeFetch =
                      Map.add channelId DateTime.Now (getDb ()).LastYoutubeFetch }

          let response = listRequest.ExecuteAsync().Result

          let urlByResourceIdKind (resourceId: ResourceId) =
              match resourceId.Kind with
              | "youtube#video" ->
                  $"https://www.youtube.com/watch?v=%s{resourceId.VideoId}"
              | "youtube#playlist" ->
                  $"https://www.youtube.com/playlist?list=%s{resourceId.PlaylistId}"
              | "youtube#channel" ->
                  $"https://www.youtube.com/channel/%s{resourceId.ChannelId}"
              | _ -> $"Unsupported resource type: %s{resourceId.Kind}"

          let channelTitle =
              match (getChannelTitleByChannelId channelId).Result with
              | Some title -> title
              | None -> channelId

          for item in response.Items do
              match item.Snippet.Type with
              | "channelItem" ->
                  yield
                      Some(
                          $"**{channelTitle}** has a new channel item, whatever that is: "
                          + urlByResourceIdKind
                              item.ContentDetails.ChannelItem.ResourceId
                      )
              | "favorite" ->
                  yield
                      Some(
                          "$**{channelTitle}** has a new favorite! I wonder what kind of cool thing it could be? "
                          + urlByResourceIdKind
                              item.ContentDetails.Favorite.ResourceId
                      )
              | "like" ->
                  yield
                      Some(
                          $"**{channelTitle}** liked something. I bet it's not weird at all: "
                          + urlByResourceIdKind
                              item.ContentDetails.Like.ResourceId
                      )
              | "playlistItem" ->
                  yield
                      Some(
                          $"**{channelTitle}** added a new video to their playlist: "
                          + urlByResourceIdKind
                              item.ContentDetails.PlaylistItem.ResourceId
                      )
              | "recommendation" ->
                  yield
                      Some(
                          $"**{channelTitle}** has a new recommendation for you: "
                          + urlByResourceIdKind
                              item.ContentDetails.Recommendation.ResourceId
                      )
              | "social" ->
                  yield
                      Some(
                          $"**{channelTitle}** has a new social thing, that social butterfly: "
                          + urlByResourceIdKind
                              item.ContentDetails.Social.ResourceId
                      )
              | "subscription" ->
                  yield
                      Some(
                          $"**{channelTitle}** has subscribed to a user. More quality content in their future! "
                          + urlByResourceIdKind
                              item.ContentDetails.Subscription.ResourceId
                      )
              | "upload" ->
                  yield
                      Some(
                          $"**{channelTitle}** has uploaded a new video: "
                          + $"https://www.youtube.com/watch?v=%s{item.ContentDetails.Upload.VideoId}"
                      )
              | _ -> yield None ]

type ContentText =
    { text: string }

    static member Decoder =
        Decode.map
            (fun text -> { text = text })
            (Decode.field "text" Decode.string)

type Community =
    { id: string
      contentText: ContentText list }

    static member Decoder =
        Decode.map2
            (fun id contentText -> { id = id; contentText = contentText })
            (Decode.field "id" Decode.string)
            (Decode.field "contentText" (Decode.list ContentText.Decoder))

type Channel =
    { id: string
      Community: Community list }

    static member Decoder =
        Decode.map2
            (fun id community -> { id = id; Community = community })
            (Decode.field "id" Decode.string)
            (Decode.field "community" (Decode.list Community.Decoder))

type ChannelList =
    { items: Channel list }

    static member Decoder =
        Decode.map
            (fun items -> { items = items })
            (Decode.field "items" (Decode.list Channel.Decoder))

let getCommunityUpdates () : string list =
    [ for channelId in (getDb ()).YoutubeChannels do
        let channelTitle = match (getChannelTitleByChannelId channelId).Result with
                           | Some title -> title
                           | None -> channelId
        let communityApiUrl = $"https://yt.lemnoslife.com/channels?part=community,snippet&id=%s{channelId}"
        let response = Http.RequestString(communityApiUrl)

        match response |> Decode.fromString ChannelList.Decoder with
        | Ok channelList ->
            for channel in channelList.items do
                for post in channel.Community do
                    if not (Set.contains post.id (getDb ()).SeenCommunityPosts) then
                        yield
                            $"**{channelTitle}** has a new community post: \n"
                            + (post.contentText |> List.map (fun x -> x.text) |> String.concat "\n")
                            + $"\n\nSee full post at https://www.youtube.com/post/%s{post.id}"

                let postIds = channel.Community |> List.map (fun x -> x.id)
                updateDb
                    { (getDb ()) with
                         SeenCommunityPosts = Set.union (getDb ()).SeenCommunityPosts (Set.ofList postIds )}
        | Error error ->
            yield
                $"Error decoding community post for channel {channelId}: {error}"
    ]
