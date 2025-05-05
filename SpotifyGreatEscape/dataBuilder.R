library(httr)
library(yaml)
library(tidyverse)

# load creds
client_id <- yaml.load_file("credentials.yml")$client_id
client_secret <- yaml.load_file("credentials.yml")$client_secret

create_key <- function(col){
    key <- col |> 
        str_to_upper() |> 
        str_replace_all("[:space:]", "") |> 
        str_replace_all("[:punct:]", "") |> 
        str_conv("UTF8")
}

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
            track_id = i$track$id,
            artists = paste(map_chr(i$track$artists, pluck, "name"), collapse = ", "),
            artist_ids = paste(map_chr(i$track$artists, pluck, "id"), collapse = ", ")
            )
    }) |> 
    separate_longer_delim(c(artists, artist_ids), ", ") |> 
    mutate(artist_key = create_key(artists))

# get GE playlist songs ----

# initial 100 tracks
ge_res <- get_spotify_details(playlist_id = ge_playlist_id)

# coerce into data frame
ge_playlist <- 
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

    # get next endpoint
    next_res <- this_res$`next`
    print(next_res)

}

# remove duplicates from playlist
ge_playlist <-
    ge_playlist |> 
    distinct(track_id, artist_ids,
             .keep_all = TRUE)

spotify_playlist_tracks <-
    ge_playlist |> 
    select(track_id, added_at)

# artist details ----

spotify_artists <-
    ge_playlist |> 
    select(track_id, artist_ids, artist_names) |> 
    mutate(artist_key = create_key(artist_names)) |> 
    separate_longer_delim(
        cols = c(artist_names, artist_ids),
        delim = ", ") |> 
    distinct(artist_names, artist_ids,
             .keep_all = TRUE)

# get additional artist details
artist_details <- 
    spotify_artists |> 
    pull(artist_ids) |> 
    map_df(.progress = TRUE, function(artist_ids){
        x <- get_spotify_details(url_string = paste0("https://api.spotify.com/v1/artists/", artist_ids))
        x <- tibble(
                artist_id = x$id,
                popularity = x$popularity,
                followers = x$followers$total,
                genres = if_else(length(x$genres) == 0, 
                                 "None", 
                                 str_c(x$genres, collapse = ", ")))
    })

spotify_artists <- 
    spotify_artists |> 
    inner_join(artist_details,
               by = c("artist_ids" = "artist_id"))

spotify_genres <- 
    spotify_artists |> 
    select(artist_ids, genres) |> 
    separate_longer_delim(genres, ", ") |> 
    mutate(genres = genres |> 
               str_squish() |> 
               str_to_sentence())

rm(artist_details)

# search spotify by artist name

artist_details <- x <- readRDS("SpotifyGreatEscape/artists.rds") |> 
    mutate(artist_encode = utils::URLencode(ge_artist)) |> 
    pull(artist_encode) |> 
    map_df(.progress = TRUE, function(i){
        x <- get_spotify_details(url_string = str_glue("https://api.spotify.com/v1/search?q=", i, "&type=artist&limit=1"))$artists$items
        x <- tibble(artist_id = pluck(x, 1, "id"),
                    artist_name = pluck(x, 1, "name"),
                    popularity = pluck(x, 1, "popularity"),
                    followers = pluck(x, 1, "followers", "total"),
                    genres = if_else(length(pluck(x, 1, "genres")) == 0, 
                                     "None", 
                                     str_c(pluck(x, 1, "genres"), collapse = ", ")))
    })
    
# TODO get track details using audio-features API:
# https://developer.spotify.com/documentation/web-api/reference/get-audio-features

# save files ----
c("spotify_playlist_tracks",
  "spotify_artists",
  "spotify_genres",
  "my_songs") |> 
    walk(.progress = "saving files", function(x){
        saveRDS(get(x), paste0("SpotifyGreatEscape/Data/RDS/", x, ".rds"))
        write.csv(get(x), paste0("SpotifyGreatEscape/Data/CSV/", x, ".csv"), row.names = FALSE)
})
