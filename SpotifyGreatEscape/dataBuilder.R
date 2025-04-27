library(httr)
library(yaml)
library(tidyverse)

# load creds
client_id <- yaml.load_file("credentials.yml")$client_id
client_secret <- yaml.load_file("credentials.yml")$client_secret

# get token udf ----
token <- function(){
    # get barer token
    res <- httr::POST(
        "https://accounts.spotify.com/api/token",
        config = authenticate(user = client_id,
                              password = client_secret),
        body = list(grant_type = "client_credentials"),
        encode = "form"
    )
    
    return(content(res)$access_token)
}

# get spotify details udf ----
get_spotify_details <- function(playlist_id = NA, url_string = NA){
    Sys.sleep(0.1)
    # get data from endpoint
    response <- httr::GET(
        if_else(
            is.character(playlist_id),
            str_glue("https://api.spotify.com/v1/playlists/{playlist_id}/tracks?limit=100"),
            url_string),
        config = add_headers(Authorization = paste0("Bearer ", token()))
    )
    # extract response to a list
    content(response)
}

# playlist IDs
ge_playlist_id <- "7pskSMBb1Hes8SEM01DGX4" # Great Escape 2025
saved_playlist_id <- "6txLMueNiknIvDCnJFV4np" # my Great Escape Bangers 2025

# get my saved songs ----
my_songs <- 
    get_spotify_details(playlist_id = saved_playlist_id)$items |> 
    map_df( function(i){
        tibble(
            track_name = i$track$name,
            track_id = i$track$id)
    })

# get GE playlist songs ----

# initial 100 tracks
ge_res <- get_spotify_details(playlist_id = ge_playlist_id)

# coerce into data frame
ge_playlist2 <- 
    ge_res$items |> 
    map_df( function(i){
        tibble(
            added_at = i$added_at,
            track_name = i$track$name,
            track_id = i$track$id,
            track_popularity = i$track$popularity,
            artist_names = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
            artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", "),
            album_name = i$track$album$name
            )
    })

next_res <- ge_res$`next`

while (TRUE) {
    # if no more to batches, break loop
    if (is.null(next_res)){
        break
    }
    # get the next batch of songs from playlist
    this_res <- get_spotify_details(url_string = next_res)
    # coerce into data frame
    temp_df <-
        this_res$items |> 
        map_df( function(i){
            tibble(
                added_at = i$added_at,
                track_name = i$track$name,
                track_id = i$track$id,
                track_popularity = i$track$popularity,
                artist_names = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
                artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", "),
                album_name = i$track$album$name
            )
        })
    # append this batch to existing data frame
    ge_playlist <- 
        ge_playlist |> 
        add_row(temp_df)
    # don't spam the API
    Sys.sleep(0.1)
    # get next endpoint
    next_res <- this_res$`next`
    print(next_res)

}

# remove duplicates from playlist & add ID col
ge_playlist <-
    ge_playlist |> 
    distinct(track_id, artist_ids, album_name,
             .keep_all = TRUE) |> 
    mutate(id = row_number())

fact_playlist <-
    ge_playlist |> 
    select(id, added_at)

# artist details ----

# TODO finish writing map_df call

dim_artists <-
    ge_playlist |> 
    select(id, artist_names, artist_ids) |> 
    separate_longer_delim(
        cols = c(artist_names, artist_ids),
        delim = ", ") |> 
    distinct(artist_names, artist_ids,
             .keep_all = TRUE)

# get additional artist details
dim_artists |> 
    head(10) |> 
    pull(artist_ids) |> 
    map_df(.progress = TRUE, function(artist_ids){
        artist_details <- 
            get_spotify_details(url_string = paste0("https://api.spotify.com/v1/artists/", artist_ids)) |> 
            map_df(~(tibble(
                    artist_id = .$id,
                    popularity = .$popularity,
                    followers = .$followers$total,
                    genres = if_else(length(.$genres) == 0, 
                                     "None", 
                                     str_c(.$genres, collapse = ", "))
            )))
    })


dim_artists |> 
    head(10) |> 
    pull(artist_ids) |> 
    map_df(.progress = TRUE, function(artist_ids){
        get_spotify_details(url_string = paste0("https://api.spotify.com/v1/artists/", artist_ids)) |> 
            map(~{tibble(artist_id = .$id)})
    })

# test ~~~~        
x <- get_spotify_details(url_string = paste0("https://api.spotify.com/v1/artists/", "1WNmfSqydnt1FDJKg3l6lw"))
    
y <- tibble(artist_id = x$id,
            popularity = x$popularity,
            followers = x$followers$total,
            genres = if_else(length(x$genres) == 0, 
                             "None", 
                             str_c(x$genres, collapse = ", ")))
# end test ~~~~

# TODO get track details using audio-features API:
# https://developer.spotify.com/documentation/web-api/reference/get-audio-features

# TODO save fact_playlist, dim_tracks, dim_artists, my_tracks instead of flat playlist df

saveRDS(GE_dataframe, "GE_playlist.rds")
# for python clustering analysis:
write.csv(GE_dataframe, "GE_playlist.csv")

