#' Get last location
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of time and coordinates of last known location
#'
#' @export
get_last_location <- function(folder = "data") {

  last_location <- fromJSON(file = file.path(folder, "location", "last_location.json"))[[1]] %>%
    as.data.frame() %>%
    transmute(date_time = as_datetime(time),
              latitude = coordinate.latitude,
              longitude = coordinate.longitude)
  return(last_location)
}

#' Get location history
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return a data frame of user location history
#'
#' @export
get_location_history <- function(folder = "data") {

  location_history <- fromJSON(file = file.path(folder, "location", "location_history.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(creation_date_time = as_datetime(creation_timestamp),
              location = name,
              latitude = coordinate.latitude,
              longitude = coordinate.longitude)
  return(location_history)
}

#' Get primary location
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame with one observation: the users primary known location
#'
#' @export
get_primary_location <- function(folder = "data") {

  primary_location <- fromJSON(file = file.path(folder, "location", "primary_location.json"))[[1]]

  data.frame(location = primary_location[[1]][[1]][[1]],
             country = primary_location[[1]][[1]][[2]],
             postcode = primary_location[[2]],
             stringsAsFactors = FALSE)
}


#' Get timezone
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return the users timezone
#'
#' @export
get_timezone <- function(folder = "data") {

  timezone <- fromJSON(file = file.path(folder, "location", "timezone.json"))[[1]]

  return(timezone)
}
