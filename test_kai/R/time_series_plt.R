##### this function creates the time series plot on the first desciptive tab
#'@export
#'@rdname time_series_plot
time_series_plotter2 <- function(df, filter_type, selected_metrics, num_tweets, input_dates1,
                                 input_dates2, dates = NA, date_range =T,
                                 input_title, group, input_roll = F, ribbon = T){


  #### convert to date
  df$created_at <- as.Date(df$created_at)
  ### filter dates
  df <- df %>% filter(between(created_at, as.Date(input_dates1), as.Date(input_dates2)))

  ### in case there is not data for the selected dates
  if(dim(df)[1] ==0){
    return(NULL)
  }

  # replace tweet length with length
  selected_metrics_new <-   stringr::str_replace(selected_metrics, "tweet_length", "length")
  if(!is.null(selected_metrics)){
  selected_metrics_new <- paste(filter_type, selected_metrics_new, sep = "_")
  }
  #### add number of tweets to metrics
  if (num_tweets == T){
    selected_metrics_new <- c(selected_metrics_new, "N")
    selected_metrics <- c(selected_metrics, "N")
  }
  #### when multiple metrics selected scale data
  if (length(selected_metrics_new) > 1){


    ### convert to nicer names for display when hovering
  selected_metrics <- selected_metrics_converter(selected_metrics)



    #### scale when mutliple selected
    df_values <- df %>% select(selected_metrics_new)%>%
      scale()

    colnames(df_values) <- selected_metrics

    ### convert to time series
    don <- xts::xts(x = df_values, order.by = df$created_at)



    ### plot the data
    dygraphs::dygraph(don,
                      ylab = "Scaled Values",
                      main = input_title,
                      group = group) %>%
    dygraphs::dyOptions(axisLineWidth = 2) %>%
    dygraphs::dyLegend() %>%

     # {if(date_range == T)  dygraphs::dyRangeSelector(.,dates + 1) else .} %>%
    dygraphs::dyShading(from = min(df$created_at), to = max(df$created_at), color = "white") %>%
      {if (input_roll == T) dygraphs::dyRoller(., rollPeriod = 7, showRoller = F) else .}


  } else{

    #### in case of single time series add ribbon

    ### set up ribbon info, green when higehr then average, red when smaller
    decreasing <- which(df[[selected_metrics_new]] < mean(df[[selected_metrics_new]]))
    increasing <- which(df[[selected_metrics_new]] >= mean(df[[selected_metrics_new]]))


    #### change selected metrics name into nice name
    df <- df %>% select(created_at, selected_metrics_new)

    if (selected_metrics != "N"){
    selected_metrics_new <- regmatches(selected_metrics_new, regexpr("_", selected_metrics_new), invert = TRUE)[[1]][2]
    }

    ##### convert selected metrics into nicer names for display
    selected_metrics_new <- selected_metrics_converter(selected_metrics_new)

    ### convert names to nicer names
    names(df) <- c("Date", selected_metrics_new)

    ### convert to time series
    dyData <- xts::xts(df[,2], order.by = df$Date)


    ##### add ribon at bottom when only onöy metric seleted that is green when higher than average and red when smaller than average
    ribbonData <- rep(0, nrow(dyData))
    ribbonData[decreasing] <- 0.5
    ribbonData[increasing] <- 1


    ### create  plot
    dygraphs::dygraph(dyData,
                      ylab = selected_metrics_new,
                      main = input_title,
                      group = group) %>%
      dygraphs::dySeries(label = selected_metrics_new) %>%
     {if (ribbon == T) dygraphs::dyRibbon(.,data = ribbonData, top = 0.05, bottom = 0) else . } %>%
      dygraphs::dyOptions(axisLineWidth = 2) %>%
      dygraphs::dyLegend() %>%

      #{if(date_range == T)  dygraphs::dyRangeSelector(.,dates + 1) else .} %>%
      dygraphs::dyShading(from = min(df$Date), to = max(df$Date), color = "white") %>%
      {if (input_roll == T) dygraphs::dyRoller(., rollPeriod = 7, showRoller = F) else .}

  }
}

#### this function converts the selceted metrics into nicer names for display
#'@rdname time_series_plot
selected_metrics_converter <- function(selected_metrics){
  selected_metrics <- stringr::str_replace(selected_metrics, "sentiment_rt", "Retweets weighted Sentiment")
  selected_metrics <- stringr::str_replace(selected_metrics, "sentiment_likes", "Likes weighted Sentiment")
  selected_metrics <- stringr::str_replace(selected_metrics, "sentiment_tweet_length", "Tweet Length weighted Sentiment")
  selected_metrics <- stringr::str_replace(selected_metrics, "likes", "Likes")
  selected_metrics <- stringr::str_replace(selected_metrics, "rt", "Retweets")
  selected_metrics <- stringr::str_replace(selected_metrics, "tweet_length", "Tweet Length")
  selected_metrics <- stringr::str_replace(selected_metrics, "sentiment", "Sentiment")

  return(selected_metrics)
}


### yesteraday as last available date
date_avail <- as.character(Sys.Date() - lubridate::days(1))

