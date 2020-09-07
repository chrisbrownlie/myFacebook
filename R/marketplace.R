#' Get items sold
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return dataframe of items sold
#'
#' @export
get_items_sold <- function(folder = "data") {

  items_sold <- fromJSON(file = file.path(folder, "marketplace", "items_sold.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(creation_date_time = as_datetime(created_timestamp),
              updated_date_time = as_datetime(updated_timestamp),
              item = title,
              price = str_replace(price, pattern = "[^[:digit:]\\.]", replacement = ""),
              category = category,
              description = description,
              marketplace = marketplace,
              location_latitude = location.coordinate.latitude,
              location_longitude = location.coordinate.longitude)
  return(items_sold)
}
