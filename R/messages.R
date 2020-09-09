#' Get messages with an individual
#'
#' @param folder the name of the data folder (in the project root directory)
#' @param participant the name of someone who you have sent or received messages from
#' (will search for this name in the data/messages/inbox folder)
#'
#' @return a character string denoting the 'friend peer group' assigned by Facebook
#'
#' @export
get_messages <- function(folder = "data",
                         participant) {

  p <- tolower(gsub(" ", "", participant))

  files <- list.files(file.path(folder, "messages", "inbox"))

  if (!any(grepl(paste0("^", p, "_"), files))) {
    close <- str_extract(files[agrep(p, files)], "^[[:print:]]+?(?=_)")
    stop(paste0("Could not find exact match, did you mean one of the following? If so, enter this as the 'participant' argument:\n-",
                paste(close, collapse = "\n-")))
  }
  match <- files[grep(paste0("^", p, "_"), files, ignore.case = T)]
  if (length(match)!=1) stop("Multiple matches, please be more specific")

  messages <- fromJSON(file = file.path(folder, "messages", "inbox", match, "message_1.json"))[["messages"]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    mutate_at(vars(matches("photos.uri")), function(x) ifelse(is.na(x), 0, 1)) %>%
    tidyr::unite("photos", matches("photos.uri"), sep = " ") %>%
    rowwise() %>%
    transmute(date_time = as_datetime(timestamp_ms/1000),
              sender = sender_name,
              content = content,
              content_type = type,
              reaction = ifelse(is.na(reactions.reaction),
                                "None",
                                paste0(reactions.reaction, " from ", reactions.actor)),
              link = link,
              photos = str_count(photos, "1"),
              call_duration = call_duration,
              missed_call = missed,
              ip_address = ip,
              videos = !is.na(videos.uri)+!is.na(videos.uri.1))
  return(messages)
}
