# create a bridge table between GE site and Spotify playlist

ge_artists <- readRDS("timetable.rds")
sp_artists <- readRDS("dim_artists.rds")

x <- ge_artists |> 
    mutate(artist_key = ArtistName |> 
               str_to_upper() |> 
               str_replace_all("[:space:]", "") |> 
               str_replace_all("[:punct:]", "") |> 
               str_conv("UTF8")) |> 
    select(artist_key, ArtistName) |> 
    full_join(
        sp_artists |> 
            mutate(artist_key = artist_names |> 
                       str_to_upper() |> 
                       str_replace_all("[:space:]", "") |> 
                       str_replace_all("[:punct:]", "") |> 
                       str_conv("UTF8")) |> 
            select(artist_key, artist_names, artist_ids),
        by = "artist_key") |> 
    writexl::write_xlsx("ge_artist_bridge.xlsx")

