#' Stock Calculations
#'

#CORONA dataset for selcted countries


#' @export
#' @rdname corona_calculations
CORONA_xgb <- function(country){
  filename <- "Corona/owid.csv"
  help <- filter(read.csv(filename),location %in% c(country))
  help$date <- as.Date(help$date)
  #help <- filter(help,date >= datestart & date <= dateend)
  help
}

#' @export
#' @rdname corona_calculations
create_hover_info_corona <- function(hovercorona,coronadata,selectedmeasure){
  hover <- hovercorona
  point <- nearPoints(coronadata, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.6); ",
                  "left:", left_px + 2, "px; top:", top_px + 30, "px;")

  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Company: </b>", point$location, "<br/>",
                  "<b> Date: </b>", point$date, "<br/>",
                  "<b>" ,selectedmeasure,": </b>", point[selectedmeasure], "<br/>")))
  )
}
