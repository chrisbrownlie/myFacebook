#' Get event invitations
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of event invitations
#'
#' @export
get_event_invitations <- function(folder = "data") {

  event_invites <- fromJSON(file = file.path(folder, "events", "event_invitations.json"))[[1]] %>%
    bind_rows() %>%
    mutate(event_name = name,
           start_time = as_datetime(start_timestamp),
           end_time = as_datetime(end_timestamp))
  return(event_invites)
}


#' Get event responses
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of event responses
#'
#' @export
get_event_responses <- function(folder = "data") {

  event_responses <- fromJSON(file = file.path(folder, "events", "your_event_responses.json"))[[1]]

  event_response_df <- map_dfr(1:3,
                               function(i) {
                                 data <- event_responses[[i]] %>%
                                   lapply(function(x) {
                                     data.frame(event_name = x[[1]],
                                                start_time = as_datetime(x[[2]]),
                                                end_time = as_datetime(x[[3]]),
                                                stringsAsFactors = FALSE)
                                   }) %>%
                                   bind_rows() %>%
                                   mutate(response = str_replace(names(event_responses)[[i]], pattern = "events_", replacement = ""))
                               })

  return(event_response_df)
}


#' Get events created by user
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of users created events
#'
#' @export
get_my_events <- function(folder = "data") {

  my_events <- fromJSON(file = file.path(folder, "events", "your_events.json"))[[1]] %>%
    lapply(function(x) {
      if ("place" %in% names(x)) {
        location_name <- ifelse("name" %in% names(x[["place"]]),
                                x[["place"]][["name"]],
                                NA)
        location_address <- ifelse("address" %in% names(x[["place"]]),
                                   x[["place"]][["address"]],
                                   NA)
        location_coordinates <- ifelse("coordinate" %in% names(x[["place"]]),
                                       paste(x[["place"]][["coordinate"]], collapse = ", "),
                                       NA)
      } else {
        location_name <- NA
        location_address <- NA
        location_coordinates <- NA
      }
      data.frame(event_name = x[["name"]],
                 created_time = as_datetime(x[["create_timestamp"]]),
                 start_time = as_datetime(x[["start_timestamp"]]),
                 end_time = as_datetime(x[["end_timestamp"]]),
                 description = x[["description"]],
                 location_name = location_name,
                 location_address = location_address,
                 location_coordinates = location_coordinates,
                 stringsAsFactors = FALSE)
    }) %>%
    bind_rows()

  return(my_events)
}

