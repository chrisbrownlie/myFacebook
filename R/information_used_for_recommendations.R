#' Get information used for recommendations
#'
#' @param type any of 'watch', 'news' or 'feed'. Will return recommended topics for: Facebook Watch;
#' news stories; and the users newsfeed, respectively.
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return a character vector of topics used for recommendations
#'
#' @export
get_recommended_topics <- function(type,
                                   folder = "data") {


  if (type == "watch") {
    f_name <- file.path(folder, "information_used_for_recommendations", "facebook_watch_topics_for_recommendations.json")
  } else if (type == "news") {
    f_name <- file.path(folder, "information_used_for_recommendations", "news_topics_for_recommendations.json")
  } else if (type == "feed") {
    f_name <- file.path(folder, "information_used_for_recommendations", "news_feed_topics_for_recommendations.json")
  } else {
    stop("Argument 'type' must be either 'watch', 'news' or 'feed'.")
  }

  topics <- fromJSON(file = f_name)[[1]]
  return(topics)
}
