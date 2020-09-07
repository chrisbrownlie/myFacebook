#' Get facial recognition info
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return the number of examples of the users face they have
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import xml2
#' @import rvest
#' @import stringr
#' @import lubridate
#' @import rjson
#'
#' @export
get_face_recognition_info <- function(folder = "data") {

  face_recognition <- fromJSON(file = file.path(folder, "about_you", "face_recognition.json")) %>%
    unlist(recursive = TRUE)

  return(face_recognition[["facial_data.example_count"]])
}

#' Get peer group from Facebook data file
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return a character string denoting the 'friend peer group' assigned by Facebook
#'
#' @export
get_friend_peer_group <- function(folder = "data") {
  fpg <- fromJSON(file = file.path(folder, "about_you", "friend_peer_group.json"))
  return(fpg[[1]])
}

#' Get any secret conversations
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return vector of logicals indicating whether the user has sent or received any secret conversations
#'
#' @export
get_secret_conversations <- function(folder = "data") {

  sc <- fromJSON(file = file.path(folder, "about_you", "messenger.json"))

  return(c("sent_secret_message" = sc$messenger$secret_conversations$has_sent_message,
           "received_secret_message" = sc$messenger$secret_conversations$has_received_message))
}

#' Get information about videos watched
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return list of data frames giving information about videos watched on Facebook Watch
#'
#' @export
get_videos_watched <- function(folder = "data") {

  viewed <- fromJSON(file = file.path(folder, "about_you", "viewed.json"))

  vids <- viewed[[1]][[1]][[3]]

  # The number of seconds spent watching videos from each page
  time_spent_per_page <- vids[[2]][[3]] %>%
    lapply(function(x){
      ts <- x[[1]]
      data <- unlist(x[[2]])
      data.frame(date_time = as_datetime(ts),
                 name = data[["name"]],
                 uri = data[["uri"]],
                 watch_time = data[["watch_time"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  # The names of shows watched (i.e. that are part of a page/series?)
  shows <- vids[[3]][[3]] %>%
    lapply(function(x){
      ts <- x[[1]]
      data <- unlist(x[[2]])
      data.frame(date_time = as_datetime(ts),
                 name = data[["name"]],
                 uri = data[["uri"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  # The number of seconds spent watching each video
  time_viewed <- vids[[4]][[3]] %>%
    lapply(function(x){
      ts <- x[[1]]
      data <- unlist(x[[2]])
      data.frame(date_time = as_datetime(ts),
                 name = data[["name"]],
                 uri = data[["uri"]],
                 watch_position_seconds = data[["watch_position_seconds"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  return(list(time_per_page = time_spent_per_page,
              shows_watched = shows,
              time_viewed = time_viewed))
}


#' Get Marketplace interactions
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return list of data frames of marketplace interactions
#'
#' @export
get_marketplace_interactions <- function(folder = "data") {

  viewed <- fromJSON(file = file.path(folder, "about_you", "viewed.json"))

  marketplace <- viewed[[1]][[4]][[3]]

  # Dates user posted items to marketplace
  dates_items_posted <- marketplace[[1]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)

  # Dates user searched in marketplace
  dates_searches_made <- marketplace[[3]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)

  # Dates user contacted a seller on marketplace
  dates_sellers_contacted <- marketplace[[4]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)

  # Dates the details of an item were viewed on marketplace
  dates_item_viewed <- marketplace[[6]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)

  # Dates when marketplace was shown to user
  dates_marketplace_shown <- marketplace[[7]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)

  # Details of items viewed in marketplace
  items_viewed <- marketplace[[8]][[3]] %>%
    lapply(function(x) {
      ts <- as_datetime(x[[1]])
      data <- x[[2]]
      data.frame(date_time = ts,
                 name = data[["name"]],
                 uri = data[["uri"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)


  # Use visited to get dates the marketplace was visited
  visited <- fromJSON(file = file.path(folder, "about_you", "visited.json"))
  dates_marketplace_visited <- visited[[1]][[5]][[3]] %>%
    unlist(recursive = TRUE, use.names = FALSE)


  return(list(dates_items_posted = dates_items_posted,
              dates_searches_made = dates_searches_made,
              dates_sellers_contacted = dates_sellers_contacted,
              dates_item_viewed = dates_item_viewed,
              dates_marketplace_shown = dates_marketplace_shown,
              dates_marketplace_visited = dates_marketplace_visited,
              items_viewed = items_viewed))
}


#' Get Ads viewed in the last week
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of ads recently viewed on Facebook
#'
#' @export
get_recent_ads <- function(folder = "data") {

  viewed <- fromJSON(file = file.path(folder, "about_you", "viewed.json"))

  ads <- viewed[[1]][[5]][[3]] %>%
    lapply(function(x) {
      ts <- as_datetime(x[[1]])
      data <- x[[2]]
      data.frame(date_time = ts,
                 name = data[["name"]],
                 uri = data[["uri"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

}

#' Get Events visited
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of events visited with date/time and name
#'
#' @export
get_events_visited <- function(folder = "data") {

  visited <- fromJSON(file = file.path(folder, "about_you", "visited.json"))

  events <- visited[[1]][[3]][[3]] %>%
    lapply(function(x) {
      ts <- as_datetime(x[[1]])
      data <- x[[2]]
      data.frame(date_time = ts,
                 name = data[["name"]],
                 uri = data[["uri"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  return(events)
}

#'  Get groups visited
#'
#'  @param folder the name of the data folder (in the project root directory)
#'
#'  @return data frame of groups visited by the user and date/time of last visit
#'
#' @export
get_groups_visited <- function(folder = "data") {

  visited <- fromJSON(file = file.path(folder, "about_you", "visited.json"))

  groups <- visited[[1]][[4]][[3]] %>%
    lapply(function(x) {
      ts <- as_datetime(x[[1]])
      data <- x[[2]]
      data.frame(date_time = ts,
                 name = data[["name"]],
                 uri = data[["uri"]],
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  return(groups)
}


#' Get address book
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of the users mobile phone contacts
#'
#' @export
get_address_book <- function(folder = "data") {

  address_book <- fromJSON(file = file.path(folder, "about_you", "your_address_books.json"))[[1]][[1]] %>%
    lapply(function(x) {
      contact <- x[[1]]
      created <- as_datetime(x[[3]])
      last_updated <- as_datetime(x[[4]])
      contact_details <- ifelse(length(x[[2]]) == 0,
                                NA,
                                x[[2]][[1]][[1]])
      type <- ifelse(str_detect(contact_details, pattern = "@"),
                     "Email",
                     "Number")
      data.frame(contact = contact,
                 type = type,
                 details = contact_details,
                 created = created,
                 last_updated = last_updated,
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)

  return(address_book)
}
