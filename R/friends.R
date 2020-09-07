#' Get friends list
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of the users friends on facebook
#'
#' @export
get_friends <- function(folder = "data") {

  friends <- fromJSON(file = file.path(folder, "friends", "friends.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(friend_name = name,
              added_time = as_datetime(timestamp),
              contact_info = contact_info)

  return(friends)
}
