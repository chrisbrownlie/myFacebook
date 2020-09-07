#' Custom GG theme for medium blog posts
#'
#' @return gg theme
#'
#' @export
theme_brownlie <- function() {
  theme(rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
        text = element_text(size = 14, family = "sans"),
        title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(colour = "#001570"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#FFFFFF00"))
}

#' Plot temporal chart with periods of life
#'
#' @param data ggplot-compatible dataframe
#' @param aes ggplot aesthetics
#' @param label_height the value of y at which to put the period of life annotations
#' @param type either "date" or "numeric" depending on the class of the x axis
#'
#' @return a base ggplot object
#'
#' @export
gg_temporal <- function(data, aes, label_height = 60, type = "date") {

  if (type == "date") {
    timespans <- data.frame(x = c(as.POSIXct("2009-01-02"),
                                  as.POSIXct("2012-09-02"),
                                  as.POSIXct("2014-09-02")),
                            xend = c(as.POSIXct("2012-05-30"),
                                     as.POSIXct("2014-06-30"),
                                     as.POSIXct("2017-08-30")),
                            posts_y = 65,
                            posts_yend = 65,
                            comms_y = label_height,
                            comms_yend = label_height,
                            label = c("Secondary School\n(Age 12-16)",
                                      "Sixth Form\n(Age 16-18)",
                                      "University\n(Age 18-20)"),
                            color = c("red",
                                      "blue",
                                      "dark green"))

    ggplot(data = data, aes = aes) +
      geom_vline(xintercept = as.POSIXct("2009-01-01"),
                 color = "red", lty = 2) +
      geom_vline(xintercept = as.POSIXct("2012-06-01"),
                 color = "red", lty = 2) +
      geom_vline(xintercept = as.POSIXct("2012-09-01"),
                 color = "blue", lty = 2) +
      geom_vline(xintercept = as.POSIXct("2014-07-01"),
                 color = "blue", lty = 2) +
      geom_vline(xintercept = as.POSIXct("2014-09-01"),
                 color = "dark green", lty = 2) +
      geom_vline(xintercept = as.POSIXct("2017-09-01"),
                 color = "dark green", lty = 2) +
      scale_x_datetime(breaks = seq(from = as.POSIXct("2009-01-01"),
                                    to = as.POSIXct("2020-07-01"),
                                    by = "6 month"),
                       labels = function(x) ifelse(month(x)==1, "", year(x))) +
      geom_segment(data = timespans,
                   aes(x = x,
                       xend = xend,
                       y = comms_y,
                       yend = comms_yend,
                       color = label),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 ends = "both"),
                   show.legend = FALSE) +
      geom_text(data = timespans,
                aes(y = comms_y,
                    x = as.POSIXct((as.numeric(x)+as.numeric(xend))/2, origin = "1970-01-01"),
                    label = label,
                    color = label),
                cex = 4,
                vjust = -0.5,
                show.legend = FALSE) +
      scale_color_manual(values = c("Secondary School\n(Age 12-16)" = "red",
                                    "Sixth Form\n(Age 16-18)" = "blue",
                                    "University\n(Age 18-20)" = "dark green")) +
      theme(axis.ticks.x = element_line(color = rep(c("black", NA), 11),
                                        size = rep(c(1.2, NA), 11)))
  } else {
    timespans <- data.frame(x = c(1,
                                  45,
                                  69),
                            xend = c(42,
                                     66,
                                     104),
                            posts_y = 65,
                            posts_yend = 65,
                            comms_y = label_height,
                            comms_yend = label_height,
                            label = c("Secondary School\n(Age 12-16)",
                                      "Sixth Form\n(Age 16-18)",
                                      "University\n(Age 18-20)"),
                            color = c("red",
                                      "blue",
                                      "dark green"))

    ggplot(data = data, aes = aes) +
      geom_vline(xintercept = 1,
                 color = "red", lty = 2) +
      geom_vline(xintercept = 42,
                 color = "red", lty = 2) +
      geom_vline(xintercept = 45,
                 color = "blue", lty = 2) +
      geom_vline(xintercept = 66,
                 color = "blue", lty = 2) +
      geom_vline(xintercept = 69,
                 color = "dark green", lty = 2) +
      geom_vline(xintercept = 104,
                 color = "dark green", lty = 2) +
      scale_x_continuous(breaks = seq(from = 0,
                                      to = 138,
                                      by = 6),
                         labels = function(x) ifelse(x%%12==0, "", floor(x/12)+2009))  +
      geom_segment(data = timespans,
                   aes(x = x,
                       xend = xend,
                       y = comms_y,
                       yend = comms_yend,
                       color = label),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 ends = "both"),
                   show.legend = FALSE) +
      geom_text(data = timespans,
                aes(y = comms_y,
                    x = (x+xend)/2,
                    label = label,
                    color = label),
                cex = 4,
                vjust = -0.5,
                show.legend = FALSE) +
      scale_color_manual(values = c("Secondary School\n(Age 12-16)" = "red",
                                    "Sixth Form\n(Age 16-18)" = "blue",
                                    "University\n(Age 18-20)" = "dark green")) +
      theme(axis.ticks.x = element_line(color = rep(c("black", NA), 11),
                                        size = rep(c(1.2, NA), 11)))
  }
}


#' Custom save function
#'
#' Save ggplot objects as png with set height and width
#'
#' @param gg a ggplot object/chart to save
#' @param name the name of the png
#' @param w width of the saved picture
#' @param h height of the saved picture
#' @param location folder in the root directory in which to save plot
#'
#' @return nothing
#'
#' @export
cb_save <- function(gg,
                    name,
                    w = 10,
                    h = 4,
                    location = "plots") {
  ggsave(filename = file.path(location, paste0(name, ".png")),
         plot = gg,
         device = "png",
         bg = "transparent", width = w, height = h)
}


#' Convert city and region into coordinates
#'
#' @param city name of a city
#' @param region name of the region e.g. england, northern ireland
#'
#' @return coordinates
#'
#' @export
convert_to_coordinates <- function(city, region = "") {

  check <- str_detect(city,
                      pattern = "[^[:alpha:][:punct:][:space:]]")
  if (is.na(check)) return(NA)
  if (check) return(NA)

  check2 <- str_detect(region,
                       pattern = "[^[:alpha:][:punct:][:space:]]")
  if (check2) region <- ""

  url <- paste0("https://nominatim.openstreetmap.org/search?q=",
                str_replace_all(city,
                                pattern = "[[:space:]]",
                                replacement = "+"),
                "+",
                str_replace_all(region,
                                pattern = "[[:space:]]",
                                replacement = "+"),
                "&format=json")

  js <- jsonlite::fromJSON(url) %>%
    select(lat, lon) %>%
    slice(1)

  return(as.numeric(c(js$lat, js$lon)))
}
