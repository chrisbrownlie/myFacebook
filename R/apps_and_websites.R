#' Get installed apps (apps linked to FB account)
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of all installed apps and when they were installed
#'
#' @export
get_installed_apps <- function(folder = "data") {

  installed_apps <- fromJSON(file = file.path(folder, "apps_and_websites", "apps_and_websites.json"))[[1]] %>%
    bind_rows() %>%
    mutate(added_timestamp = as_datetime(added_timestamp))

  return(installed_apps)
}

#' Get facebook posts from external websites
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of posts made to users profile by external apps/websites
#'
#' @export
get_posts_from_external <- function(folder = "data") {

  external_posts <- fromJSON(file = file.path(folder, "apps_and_websites", "posts_from_apps_and_websites.json"))[[1]] %>%
    lapply(function(x){
      ts <- as_datetime(x[[1]])
      if ("attachments" %in% names(x)) {
        atts <- unlist(x[[2]], recursive = TRUE)
        att_name <- ifelse("data.external_context.name" %in% names(atts),
                           atts[["data.external_context.name"]],
                           NA)
        att_url <- ifelse("data.external_context.url" %in% names(atts),
                          atts[["data.external_context.url"]],
                          NA)
        post_title <- x[[3]]
      } else {
        att_name <- NA
        att_url <- NA
        post_title <- x[[2]]
      }
      data.frame(date_time = ts,
                 post_title = post_title,
                 attachment = att_name,
                 attachment_url = att_url,
                 stringsAsFactors = FALSE)
    }) %>%
    reduce(bind_rows)
}
