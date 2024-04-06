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
        match Map.tryFind channelId (getDb ()).YoutubeChannelTitles with
        | Some title -> return Some title
        | None ->
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
                    let title = items[0].BrandingSettings.Channel.Title

                    updateDb
                        { (getDb ()) with
                            YoutubeChannelTitles =
                                Map.add
                                    channelId
                                    title
                                    (getDb ()).YoutubeChannelTitles }

                    Some title
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
                       |> Option.defaultValue (DateTime.UtcNow.AddHours(-1)))
              )

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

          let result =
              [ for item in response.Items do
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

          let latestItem =
              if response.Items.Count = 0 then
                  None
              else
                  response.Items
                  |> Seq.maxBy (fun i ->
                      match DateTime.TryParse(i.Snippet.PublishedAtRaw) with
                      | true, dt -> dt
                      | _ -> DateTime.MinValue)
                  |> Option.ofObj

          match latestItem with
          | Some item ->
              updateDb
                  { (getDb ()) with
                      LastYoutubeFetch =
                          Map.add
                              channelId
                              (DateTime
                                  .Parse(item.Snippet.PublishedAtRaw)
                                  .AddSeconds(1))
                              (getDb ()).LastYoutubeFetch }
          | None -> ()

          yield! result ]

type ContentText =
    { text: string
      url: string option }

    static member Decoder =
        Decode.map2
            (fun text url -> { text = text; url = url })
            (Decode.field "text" Decode.string)
            (Decode.optional "url" Decode.string)

type Thumbnail =
    { url: string
      width: int
      height: int }

    static member Decoder =
        Decode.map3
            (fun url width height ->
                { url = url
                  width = width
                  height = height })
            (Decode.field "url" Decode.string)
            (Decode.field "width" Decode.int)
            (Decode.field "height" Decode.int)

type Image =
    { thumbnails: Thumbnail list }

    static member Decoder =
        Decode.map
            (fun thumbnails -> { thumbnails = thumbnails })
            (Decode.field "thumbnails" (Decode.list Thumbnail.Decoder))

type Community =
    { id: string
      contentText: ContentText list
      videoId: string option
      images: Image list }

    static member Decoder =
        Decode.map4
            (fun id contentText videoId images ->
                { id = id
                  contentText = contentText
                  videoId = videoId
                  images = images })
            (Decode.field "id" Decode.string)
            (Decode.field "contentText" (Decode.list ContentText.Decoder))
            (Decode.field "videoId" (Decode.option Decode.string))
            (Decode.field "images" (Decode.list Image.Decoder))

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

let getCommunityUpdates () : Result<string, string> list =
    [ for channelId in (getDb ()).YoutubeChannels do
          let channelTitle =
              match (getChannelTitleByChannelId channelId).Result with
              | Some title -> title
              | None -> channelId

          let communityApiUrl =
              $"https://yt.lemnoslife.com/channels?part=community,snippet&id=%s{channelId}"

          let response = Http.RequestString(communityApiUrl)

          match response |> Decode.fromString ChannelList.Decoder with
          | Ok channelList ->
              for channel in channelList.items do
                  for post in channel.Community do
                      if
                          not (
                              Set.contains post.id (getDb ()).SeenCommunityPosts
                          )
                      then
                          yield
                              Ok(
                                  $"**{channelTitle}** has a new community post: \n"
                                  + (post.contentText
                                     |> List.map (fun x -> x.text)
                                     |> String.concat "")
                                  + (match post.videoId with
                                     | Some videoId ->
                                         $"\n\nhttps://www.youtube.com/watch?v=%s{videoId}"
                                     | None -> "")
                                  + (post.images
                                     |> List.map (fun i ->
                                         Seq.tryLast i.thumbnails)
                                     |> List.choose id
                                     |> List.map (fun t -> t.url)
                                     |> List.map (fun url ->
                                         $"\n[Image]({url})")
                                     |> String.concat "")
                                  + $"\n\nSee full post at https://www.youtube.com/post/%s{post.id}"
                              )

                  let postIds = channel.Community |> List.map (fun x -> x.id)

                  updateDb
                      { (getDb ()) with
                          SeenCommunityPosts =
                              Set.union
                                  (getDb ()).SeenCommunityPosts
                                  (Set.ofList postIds) }
          | Error error -> yield Error(error) ]
