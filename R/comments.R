#' Get comments
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of comments
#'
#' @export
get_comments <- function(folder = "data") {

  comms <- fromJSON(file = file.path(folder, "comments", "comments.json"))[[1]] %>%
    lapply(function(x) {
      ts <- as_datetime(x[["timestamp"]])
      title <- x[["title"]]
      if ("data" %in% names(x)){
        comment_ts <- as_datetime(x[["data"]][[1]][["comment"]][["timestamp"]])
        comment_author <- x[["data"]][[1]][["comment"]][["author"]]
        comment_content <- x[["data"]][[1]][["comment"]][["comment"]]
      } else {
        comment_ts <- NA
        comment_author <- NA
        comment_content <- NA
      }
      data.frame(date_time = ts,
                 author = comment_author,
                 comment_date_time = comment_ts,
                 event_description = title,
                 comment = comment_content,
                 stringsAsFactors = FALSE) %>%
        mutate(commented_on = str_extract(event_description, pattern = "[[:alpha:]]+(?=\\.$)"))
    }) %>%
    reduce(bind_rows)

  return(comms)
}
