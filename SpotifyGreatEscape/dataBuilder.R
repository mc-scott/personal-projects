library(httr)
library(yaml)
library(tidyverse)

BATCH_SIZE <- "100"

# post request for token
client_id <- yaml.load_file("credentials.yml")$client_id
client_secret <- yaml.load_file("credentials.yml")$client_secret

res <- httr::POST(
    "https://accounts.spotify.com/api/token",
    config = authenticate(user = client_id,
                          password = client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
)

token <- content(res)$access_token

# build endpoint
playlist_id <- "3EinPtec9V2HgG6ceAaqBf" # The Great Escape 2024



endpoint <- paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks?offset=500&limit=67")


# get data from endpoint
response <- httr::GET(
    endpoint,
    config = add_headers(Authorization = paste0("Bearer ", token))
)
# extract response to a list
res_list <- content(response)

# get the number of tracks in playlist divided by batch size for ?offset= param
num_loop_batches <- floor(res_list$total/100)

# length of items returned 
ntracks <- length(res_list$items)


track_list <- data.frame(
    AddedAt = numeric(ntracks),
    SongName = character(ntracks),
    SongID = character(ntracks),
    SongPopularity = numeric(ntracks),
    Artist = character(ntracks),
    ArtistID = character(ntracks),
    ArtistLink = character(ntracks),
    Album = character(ntracks),
    AlbumLink = character(ntracks)
)

for (i in 1:ntracks){
    track_list[i,]$AddedAt <- res_list$items[[i]]$added_at
    track_list[i,]$SongName <- res_list$items[[i]]$track$name
    track_list[i,]$SongID <- res_list$items[[i]]$track$id
    track_list[i,]$SongPopularity <- res_list$items[[i]]$track$popularity
    track_list[i,]$Artist <- res_list$items[[i]]$track$artists[[1]]$name
    track_list[i,]$ArtistID <- res_list$items[[i]]$track$artists[[1]]$id
    track_list[i,]$ArtistLink <- res_list$items[[i]]$track$artists[[1]]$external_urls$spotify
    track_list[i,]$Album <- res_list$items[[i]]$track$album$name
    track_list[i,]$AlbumLink <- res_list$items[[i]]$track$album$external_urls$spotify
}

# Get Additional Track Details
for (i in 1:ntracks) {
    
    # pause briefly between calls
    Sys.sleep(0.10)
    track_endpoint = paste0('https://api.spotify.com/v1/audio-features/', track_list$SongID[i])
    track_response = GET(track_endpoint, 
                         config = add_headers(Authorization = paste0("Bearer ", token)))
    track_content = content(track_response)
    
    track_list$key[i] <- track_content$key
    track_list$mode[i] <- track_content$mode
    track_list$time_signature[i] <- track_content$time_signature
    track_list$acousticness[i] <- track_content$acousticness
    track_list$danceability[i] <- track_content$danceability
    track_list$energy[i] <- track_content$energy
    track_list$instrumentalness[i] <- track_content$instrumentalness
    track_list$liveliness[i] <- track_content$liveness
    track_list$loudness[i] <- track_content$loudness
    track_list$speechiness[i] <- track_content$speechiness
    track_list$valence[i] <- track_content$valence
    track_list$tempo[i] <- track_content$tempo
}

# get additional artist details
for (i in 1:ntracks) {
    
    Sys.sleep(0.10)
    artist_endpoint = paste0("https://api.spotify.com/v1/artists/", track_list$ArtistID[i])
    artist_response = GET(artist_endpoint, 
                         config = add_headers(Authorization = paste0("Bearer ", token)))
    artist_content = content(artist_response)
    
    track_list$followers[i] <- artist_content$followers$total
    track_list$popularity[i] <- artist_content$popularity
    # TODO get genres
}

track_list_df = list()

# now loop through the remaining playlist in batches of 100
for (batch_i in 1:num_loop_batches){

    endpoint <- paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks?offset=", batch_i ,"00&limit=", BATCH_SIZE)
    # get data from endpoint
    response <- httr::GET(
        endpoint,
        config = add_headers(Authorization = paste0("Bearer ", token))
    )
    # extract response to a list
    res_list <- content(response)
    
    # instantiate an empty data frame with necessary cols
    track_list <- data.frame(
        AddedAt = numeric(BATCH_SIZE),
        SongName = character(BATCH_SIZE),
        SongID = character(BATCH_SIZE),
        SongPopularity = numeric(BATCH_SIZE),
        Artist = character(BATCH_SIZE),
        ArtistID = character(BATCH_SIZE),
        ArtistLink = character(BATCH_SIZE),
        Album = character(BATCH_SIZE),
        AlbumLink = character(BATCH_SIZE)
    )
    # fill df with values from nested list response
    for (i in 1:BATCH_SIZE){
        track_list[i,]$AddedAt <- res_list$items[[i]]$added_at
        track_list[i,]$SongName <- res_list$items[[i]]$track$name
        track_list[i,]$SongID <- res_list$items[[i]]$track$id
        track_list[i,]$SongPopularity <- res_list$items[[i]]$track$popularity
        track_list[i,]$Artist <- res_list$items[[i]]$track$artists[[1]]$name
        track_list[i,]$ArtistID <- res_list$items[[i]]$track$artists[[1]]$id
        track_list[i,]$ArtistLink <- res_list$items[[i]]$track$artists[[1]]$external_urls$spotify
        track_list[i,]$Album <- res_list$items[[i]]$track$album$name
        track_list[i,]$AlbumLink <- res_list$items[[i]]$track$album$external_urls$spotify
    }
    
    # Get Additional Track Details
    for (i in 1:BATCH_SIZE) {
        
        # pause briefly between calls
        Sys.sleep(0.10)
        track_endpoint = paste0('https://api.spotify.com/v1/audio-features/', track_list$SongID[i])
        track_response = GET(track_endpoint, 
                             config = add_headers(Authorization = paste0("Bearer ", token)))
        track_content = content(track_response)
        
        track_list$key[i] <- track_content$key
        track_list$mode[i] <- track_content$mode
        track_list$time_signature[i] <- track_content$time_signature
        track_list$acousticness[i] <- track_content$acousticness
        track_list$danceability[i] <- track_content$danceability
        track_list$energy[i] <- track_content$energy
        track_list$instrumentalness[i] <- track_content$instrumentalness
        track_list$liveliness[i] <- track_content$liveness
        track_list$loudness[i] <- track_content$loudness
        track_list$speechiness[i] <- track_content$speechiness
        track_list$valence[i] <- track_content$valence
        track_list$tempo[i] <- track_content$tempo
    }
    
    # get additional artist details
    for (i in 1:BATCH_SIZE) {
        
        Sys.sleep(0.10)
        artist_endpoint = paste0("https://api.spotify.com/v1/artists/", track_list$ArtistID[i])
        artist_response = GET(artist_endpoint, 
                              config = add_headers(Authorization = paste0("Bearer ", token)))
        artist_content = content(artist_response)
        
        track_list$followers[i] <- artist_content$followers$total
        track_list$popularity[i] <- artist_content$popularity
        # TODO get genres (and account for when this is null so it doesn't break)
    }
    # fill list with each df created in for loop
    track_list_df[[batch_i]] <- track_list
}
# bind rows into one df
track_list_df = do.call(rbind, track_list_df)

GE_dataframe <- rbind(data, track_list)

saveRDS(GE_dataframe, "GE_playlist.rds")
# for python clustering analysis:
write.csv(GE_dataframe, "GE_playlist.csv")

# TODO now get from track 500-556 (i think it broke last time because it expected a batch of 100)
