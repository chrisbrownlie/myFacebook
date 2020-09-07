#' Get profile information
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return a list of data frames containing profile information
#'
#' @export
get_profile_information <- function(folder = "data") {

  profile_info <- fromJSON(file = file.path(folder, "profile_information", "profile_information.json"))[[1]]

  profile_info_df <- data.frame(first_name = profile_info$name$first_name,
                                middle_name = profile_info$name$middle_name,
                                last_name = profile_info$name$last_name,
                                joined_facebook = as_datetime(profile_info$registration_timestamp),
                                emails = paste(profile_info$emails$emails, collapse = "; "),
                                previous_email = profile_info$emails$previous_emails,
                                birthday = ymd(paste(profile_info$birthday$year,
                                                     profile_info$birthday$month,
                                                     profile_info$birthday$day)),
                                hometown = profile_info$hometown$name,
                                current_city = profile_info$current_city$name,
                                interested_in = profile_info$interested_in,
                                relationship_status = profile_info$relationship$status,
                                relationship_partner = profile_info$relationship$partner,
                                relationship_anniversary = ymd(paste(profile_info$relationship$anniversary$year,
                                                                     profile_info$relationship$anniversary$month,
                                                                     profile_info$relationship$anniversary$day)))
  return(profile_info_df)
}


#' Get profile update history
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return a list of data frames containing the profile update history
get_profile_update_history <- function(folder = "data") {

  profile_updates <- fromJSON(file = file.path(folder, "profile_information", "profile_update_history.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              title = title,
              life_event = attachments.data.life_event.title,
              start_date_year = attachments.data.life_event.start_date.year,
              start_date_month = attachments.data.life_event.start_date.month,
              start_date_day = attachments.data.life_event.start_date.day,
              place_name = attachments.data.place.name,
              place_lat = attachments.data.place.coordinate.latitude,
              place_lon = attachments.data.place.coordinate.longitude,
              place_url = attachments.data.place.url)

  return(profile_updates)
}
