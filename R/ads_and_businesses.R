#' Get ads interests
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return character vector of all the users ad interests
#'
#' @export
get_ad_interests <- function(folder = "data") {

  interests <- fromJSON(file = file.path(folder, "ads_and_businesses", "ads_interests.json")) %>%
    unlist(recursive = T, use.names = FALSE)

  return(interests)
}

#' Get advertisers who have your contact info
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return character vector of advertisers who have uploaded a contact list containing
#' the users information
#'
#' @export
get_advertisers_with_contact_info <- function(folder = "data") {

  advertisers_with_contact_info <- fromJSON(file = file.path(folder, "ads_and_businesses", "advertisers_who_uploaded_a_contact_list_with_your_information.json")) %>%
    unlist(recursive = TRUE, use.names = FALSE)

  return(advertisers_with_contact_info)
}

#' Get advertisers the user has interacted with (in the last month?)
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of advertisers and the users interaction with their advert
#'
#' @export
get_advert_interactions <- function(folder = "data") {

  advertisers_interacted_with <- fromJSON(file = file.path(folder, "ads_and_businesses", "advertisers_you've_interacted_with.json"))[[1]] %>%
    reduce(bind_rows) %>%
    transmute(date_time = as_datetime(timestamp),
              advert_title = title,
              interaction = action)

  return(advertisers_interacted_with)
}

#' Get off-Facebook activity
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of off-facebook activity
#'
#' @export
get_off_facebook_activity <- function(folder = "data") {

  off_fb_activity <- fromJSON(file = file.path(folder, "ads_and_businesses", "your_off-facebook_activity.json"))[[1]] %>%
    lapply(function(x) {
      company <- x[[1]]
      events <- x[[2]] %>%
        bind_rows() %>%
        mutate(location = company,
               date_time = as_datetime(timestamp)) %>%
        select(date_time,
               location,
               type,
               id)
      return(events)
    }) %>%
    reduce(bind_rows)
  return(off_fb_activity)
}

