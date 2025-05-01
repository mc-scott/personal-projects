# create a bridge table between GE site and Spotify playlist
library(dplyr)
library(stringr)

# read saved files ----
ge_artists <- readRDS("timetable.rds")
sp_artists <- readRDS("dim_artists.rds")

# helper function ----
create_key <- function(col){
    key <- col |> 
        str_to_upper() |> 
        str_replace_all("[:space:]", "") |> 
        str_replace_all("[:punct:]", "") |> 
        str_conv("UTF8")
}

# match GE artists with Spoitify artisits ---- 
x <- ge_artists |> 
    mutate(artist_key = create_key(ArtistName)) |> 
    select(artist_key, ge_artist = ArtistName) |> 
    full_join(
        sp_artists |> 
            mutate(artist_key = create_key(artist_names)) |> 
            select(artist_key, spotify_artist = artist_names, artist_ids),
        by = "artist_key") #|> 
    #writexl::write_xlsx("ge_artist_bridge.xlsx")

