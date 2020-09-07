#' Get users posts
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of the user's posts on facebook
#'
#' @export
get_posts <- function(folder = "data") {

  i <- 0
  posts <- fromJSON(file = file.path(folder, "posts", "your_posts_1.json")) %>%
    lapply(function(x) {
      xvec <- unlist(x, recursive = T, use.names = TRUE)
      namevec <- names(xvec)
      xvec <- xvec %>%
        as.character()
      names(xvec) <- namevec
      as.data.frame(as.list(xvec))
    }) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(as.numeric(timestamp)),
              date_time_updated = as_datetime(as.numeric(data.update_timestamp)),
              title = title,
              post = data.post,
              tag1 = tags1,
              tag2 = tags2,
              tag3 = tags3,
              tag4 = tags4,
              tag5 = tags5,
              attached_url = attachments.data.external_context.url,
              attached_context_name = attachments.data.external_context.name,
              attached_context_source = attachments.data.external_context.source,
              location_name = attachments.data.place.name,
              location_address = attachments.data.place.address,
              location_latitude = attachments.data.place.coordinate.latitude,
              location_longitude = attachments.data.place.coordinate.longitude,
              event_name = attachments.data.event.name,
              event_start_time = as_datetime(as.numeric(attachments.data.event.start_timestamp)),
              event_end_time = as_datetime(as.numeric(attachments.data.event.end_timestamp)),
              attached_media_uri = attachments.data.media.uri,
              attached_media_creation_date_time = as_datetime(as.numeric(attachments.data.media.creation_timestamp)),
              attached_media_upload_date_time = as_datetime(as.numeric(attachments.data.media.media_metadata.video_metadata.upload_timestamp)),
              attached_media_title = attachments.data.media.title,
              attached_media_description = attachments.data.media.description,
              attached_media_upload_ip_address = coalesce(attachments.data.media.media_metadata.photo_metadata.upload_ip,
                                                          attachments.data.media.media_metadata.video_metadata.upload_ip),
              poll_question = attachments.data.poll.question,
              poll_voted_for = case_when(
                attachments.data.poll.options.voted == "TRUE" ~ attachments.data.poll.options.option,
                attachments.data.poll.options.voted.1 == "TRUE" ~ attachments.data.poll.options.option.1,
                attachments.data.poll.options.voted.2 == "TRUE" ~ attachments.data.poll.options.option.2,
                attachments.data.poll.options.voted.3 == "TRUE" ~ attachments.data.poll.options.option.3,
                attachments.data.poll.options.voted.4 == "TRUE" ~ attachments.data.poll.options.option.4,
                attachments.data.poll.options.voted.5 == "TRUE" ~ attachments.data.poll.options.option.5,
                TRUE ~ ""
              ),
              poll_alt_option_1 = ifelse(attachments.data.poll.options.voted.1 == "TRUE",
                                         attachments.data.poll.options.option,
                                         attachments.data.poll.options.option.1),
              poll_alt_option_2 = ifelse(attachments.data.poll.options.voted.2 == "TRUE",
                                         attachments.data.poll.options.option,
                                         attachments.data.poll.options.option.2),
              poll_alt_option_3 = ifelse(attachments.data.poll.options.voted.3 == "TRUE",
                                         attachments.data.poll.options.option,
                                         attachments.data.poll.options.option.3),
              poll_alt_option_4 = ifelse(attachments.data.poll.options.voted.4 == "TRUE",
                                         attachments.data.poll.options.option,
                                         attachments.data.poll.options.option.4),
              poll_alt_option_5 = ifelse(attachments.data.poll.options.voted.5 == "TRUE",
                                         attachments.data.poll.options.option,
                                         attachments.data.poll.options.option.5))

  return(posts)
}
