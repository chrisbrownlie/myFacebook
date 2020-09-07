#' Get fundraisers donated to
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of fundraisers donated to
#'
#' @export
get_fundraisers_donated_to <- function(folder = "data") {

  fundraisers <- fromJSON(file = file.path(folder, "other_activity", "fundraisers_donated_to.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows %>%
    transmute(date_time = as_datetime(timestamp),
              title = str_replace(attachments.data.fundraiser.title, pattern = "[^[:alnum:][:space:][:punct:]]", replacement = ""),
              amount_donated = paste0("£", str_extract(attachments.data.fundraiser.donated_amount, "[[:digit:]]+")))
  return(fundraisers)
}

