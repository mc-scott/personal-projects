library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

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
## NOTE: an artist can have multiple gigs, need to loop through all...
num_events <- function(artist_url){
    read_html(artist_url) %>%
        html_elements(css = "div.event") %>% 
        length()
}

event_venues <- function(artist_url) {
    # get number of rows in event grid
    num_events <- num_events(artist_url)    
    # clear variable
    venues = character()
    # loop through rows in event grid, finding artist venue
    for (x in 1:num_events) {
        venue <- read_html(artist_url) %>%
            html_element(xpath = paste0('/html/body/div[2]/div/div/div/article/div/div[', x+1, ']/div/a')) %>% 
            html_attr("title")
        venues <- paste(venues, venue, sep = ", ")
    }
    return(venues)
}

event_times <- function(artist_url) {
    # get number of rows in events grid
    num_events <- num_events(artist_url)
    # clear variable
    times = character()
    # loop through html elements in event grid, finding gig times
    for (x in 1:num_events) {
        event_time <- read_html(artist_url) %>%
            html_element(xpath = paste0('/html/body/div[2]/div[1]/div/div/article/div/div[', x+1, ']/div[2]')) %>%
            html_text2() %>% 
            str_remove("\r ")
        times <- paste(times, event_time, sep = ", ")
    }
    return(times)
}

# get blurb

artist_blurb <- function(artist_url) {
    read_html(artist_url) %>%
        html_element(xpath = '/html/body/div[2]/div/div/div/article/div[2]/p') %>% 
        html_text()       
}

# create df with first row using UDFs above
artist1 <- "https://greatescapefestival.com/artists/abby-sage/"

timetable_df <- tibble(
    ArtistName = artist_name(artist1),
    ArtistFrom = artist_from(artist1),
    ArtistBlurb = artist_blurb(artist1),
    EventVenues = event_venues(artist1),
    EventTimes = event_times(artist1),
    Source = artist1
)

# run a loop getting artist info and then moving to the next page
# break when next artist comes back around to start alphabetically
x <- 1

while (TRUE) {
    
    # get the next artist URL by querying last row of table Source column
    artist <- slice_tail(timetable_df, n = 1) %>% 
        select(Source) %>% 
        as.character()
    
    next_artist <- get_next_artist(artist)
    
    if (next_artist == "https://greatescapefestival.com/artists/abby-sage/") {
        break
    }
    
    # append rows to pre-defined cols
    timetable_df <- timetable_df %>% add_row(
        ArtistName = artist_name(next_artist),
        ArtistFrom = artist_from(next_artist),
        ArtistBlurb = artist_blurb(next_artist),
        EventVenues = event_venues(next_artist),
        EventTimes = event_times(next_artist),
        Source = next_artist
    )
    
    print(paste(x, ": ", artist))
    
    x <- x + 1
}

# make sure it didn't duplicate pages in loop
timetable <- timetable_df %>% 
    distinct()

# TODO tidy DF and separate longer by delim

saveRDS(timetable, "timetable.rds")

