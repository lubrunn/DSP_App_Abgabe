#' Stock Calculations
#'



#' @export
#' @rdname stock_calculations
create_hover_info_DE <- function(hoverinput,stockdata){
  hover <- hoverinput
  point <- nearPoints(stockdata, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
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
                  "left:", left_px + 2, "px; top:", top_px + 60, "px;")

  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Company: </b>", point$name, "<br/>",
                  "<b> Date: </b>", point$Date, "<br/>",
                  "<b> Price: </b>", point$Close., "<br/>")))
  )
}

#' @export
#' @rdname stock_calculations
missing_date_imputer <- function(df,df_column){

  df = df[!(df[df_column] == "-"),]

  df[df_column] = as.numeric(gsub(",","",df[,5]))

  df = df[c("Date",df_column)]

  date_vector <- as.data.frame(seq(min(df$Date), max(df$Date), by="days"))
  colnames(date_vector)[1]<-"Date"

  final <- left_join(date_vector,df, by = "Date")

  final[glue("{df_column}")] <- sapply(final[glue("{df_column}")],na_kalman)
  return(final)
}



