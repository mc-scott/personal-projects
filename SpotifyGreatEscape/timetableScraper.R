library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# get next artist URL from 'next' chevron on page
get_next_artist <- function(artist_url){
    read_html(artist_url) %>% 
        html_elements(".chev-right") %>% 
        html_attr("href")  
}

# get artist details from page

## name
artist_name <- function(artist_url){
    read_html(artist_url) %>% 
        html_element(".article__title--single") %>% 
        html_text() 
}

## artist location
artist_from <- function(artist_url){
    read_html(artist_url) %>%
        html_element(xpath = '/html/body/div[2]/div/div/div/article/h2') %>%
        html_text() %>% 
        as.character(.) %>% 
        str_sub(2, -2) # remove first '(' and last ')'characters
}

# get venues and times

## NOTE: an artist can have multiple gigs, need to loop through all
num_events <- function(artist_url){
    read_html(artist_url) %>%
        html_elements(css = "div.event") %>% 
        length()
}

event_venues <- function(artist_url) {
    # get number of rows in event grid
    num_events <- num_events(artist_url)    
    # clear variable
    venues = "NA"
    # loop through rows in event grid, finding artist venue
    for (x in 1:num_events) {
        venue <- read_html(artist_url) %>%
            html_element(xpath = paste0('/html/body/div[2]/div/div/div/article/div/div[', x+1, ']/div/a')) %>% 
            html_attr("title")
        venues <- paste(venues, venue, sep = ", ")
        venues <- str_remove(venues, "NA, ")
    }
    return(venues)
}

event_times <- function(artist_url) {
    # get number of rows in events grid
    num_events <- num_events(artist_url)
    # clear variable
    times = "NA"
    # loop through html elements in event grid, finding gig times
    for (x in 1:num_events) {
        event_time <- read_html(artist_url) %>%
            html_element(xpath = paste0('/html/body/div[2]/div[1]/div/div/article/div/div[', x+1, ']/div[2]')) %>%
            html_text2() %>% 
            str_remove("\r ")
        times <- paste(times, event_time, sep = ", ")
        times <- str_remove(times, "NA, ")
    }
    return(times)
}

# get blurb

artist_blurb <- function(artist_url) {
    read_html(artist_url) %>%
        html_element(xpath = '/html/body/div[2]/div/div/div/article/div[2]/p') %>% 
        html_text()       
}

# create artist key

create_key <- function(col){
    key <- col |> 
        str_squish() |> 
        str_to_upper() |> 
        str_replace_all("[:space:]", "-") |> 
        str_replace_all("[:punct:]", "") |> 
        str_conv("UTF8")
}

# create df with first artist
artist1 <- "https://greatescapefestival.com/artists/76/"

timetable_df <- tibble(
    ge_artist = artist_name(artist1),
    artist_from = artist_from(artist1),
    artist_blurb = artist_blurb(artist1),
    event_venues = event_venues(artist1),
    event_times = event_times(artist1),
    url = artist1
)

# run a loop getting artist info and then moving to the next page
# break when next artist comes back around to start, alphabetically
x <- 1

while (TRUE) {
    
    # get the next artist URL by querying last row of table url column
    artist <- slice_tail(timetable_df, n = 1) %>% 
        select(url) %>% 
        as.character()
    
    next_artist <- get_next_artist(artist)
    
    # break if we've cycled round to the first artists
    if (next_artist == artist1) {
        break
    }
    
    # append rows to pre-defined cols
    timetable_df <- timetable_df %>% add_row(
        ge_artist = artist_name(next_artist),
        artist_from = artist_from(next_artist),
        artist_blurb = artist_blurb(next_artist),
        event_venues = event_venues(next_artist),
        event_times = event_times(next_artist),
        url = next_artist
    )
    
    print(paste(x, ": ", artist))
    
    x <- x + 1
}

timetable <- timetable_df %>%
    # make sure loop didn't duplicate pages
    distinct() %>% 
    # create artist key - lower and remove spaces
    mutate(artist_key = create_key(ge_artist))

# create gigs table w/ venues and times in for given artist
# clean event timetable to get rows for each gig and format to date-time
gigs <- timetable %>% 
    select(artist_key, event_venues, event_times) %>% 
    mutate(event_venues = str_squish(event_venues)) %>%
    # simultaneously separate two columns by the same delim
    separate_longer_delim(c(event_venues, event_times), ", ") %>% 
    mutate(
        parsed_event_times = 
            case_when(
                str_detect(str_to_lower(event_times), "wednesday") ~
                    paste("14/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "thursday") ~ 
                    paste("15/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "friday") ~ 
                    paste("16/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                str_detect(str_to_lower(event_times), "saturday") ~ 
                    paste("17/05/2025", str_extract(event_times, "([0-9]|0[0-9]|1[0-9]):([0-5][0-9])([AaPp][Mm])")),
                event_times == "NA" ~ NA_character_
            ),
        parsed_event_times = lubridate::parse_date_time(parsed_event_times, orders = "dmy HMOp", tz = "Europe/London"),
        event_venues = if_else(event_venues == "NA", NA_character_, event_venues)
    )

# create artists table w/ artist info in
artists <- timetable %>% 
    select(artist_key, ge_artist, artist_from, artist_blurb)

venues <- gigs |> 
    distinct(event_venues) |> 
    drop_na()
    
# save files ----
c("gigs",
  "artists",
  "timetable",
  "venues") |> 
    walk(function(x){
        saveRDS(get(x), paste0("SpotifyGreatEscape/", x, ".rds"))
        write.csv(get(x), paste0("SpotifyGreatEscape/", x, ".csv"), row.names = FALSE)
    })
