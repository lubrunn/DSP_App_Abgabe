
#'@export
#'@rdname time_series_plot
time_series_plotter <- function(df, filter_type, selected_metrics, num_tweets){

if (num_tweets == T){
  selected_metrics <- c(selected_metrics, "N")
}

df <-     df %>%
     select(created_at, contains("mean"), -language) %>%
     pivot_longer(
        !created_at, names_to = "metric", values_to = "value"
        #,names_prefix = "mean_"
      ) %>%
    bind_rows(
      df %>%
        select(created_at, contains("std"), -language) %>%
        pivot_longer(
          !created_at, names_to = "metric", values_to = "value"
          #,names_prefix = "mean_"
        )
    ) %>%
    bind_rows(
      df %>%
        select(created_at, contains("median"), -language) %>%
        pivot_longer(
          !created_at, names_to = "metric", values_to = "value"
          #,names_prefix = "mean_"
        )
    )  %>%
    separate(col = metric, into = c("type", "metric"), sep = "_", remove = F, extra = "merge") %>%
  # { if (!grepl("tweet_length", selected_metrics)) mutate(.,metric = stringr::str_replace(metric, "length",
  #                                                                                      "tweet_length")) else .} %>%

  bind_rows(
    df %>% select(created_at, N) %>% pivot_longer(N,
                                                  names_to = "metric", values_to = "value"
    ) %>% mutate(type = "mean")
  ) %>%

    filter(type == filter_type & metric %in% selected_metrics)


##### plot
# if only one selected just plot
if (length(selected_metrics) == 1){
    df %>%
    ggplot() +
    geom_line(aes(x = as.Date(created_at), y = value, color = metric))

  } else { # if mutliple selected set up loop and append all needed parts then plot and use metric as grouping aes

    df_all <- NULL
    for (metric_value in selected_metrics){

       df_filt <- df %>% filter(metric == metric_value)
      df_filt$value <-  scale(df_filt$value)

      if (is.null(df_all)){
        df_all <- df_filt
      } else{
        df_all <- rbind(df_all, df_filt)
      }



    }
    df_all %>%
      ggplot() +
      geom_line(aes(x = as.Date(created_at), y = value, color = metric))

  }


}

# num_tweets <- T
# selected_metrics <-  c("sentiment",
# "sentiment_rt",
# "sentiment_likes",
# "sentiment_tweet_length",
# "rt",
# "likes",
# "tweet_length")
#
#
# filter_type = "mean"

time_series_plotter2 <- function(df, filter_type, selected_metrics, num_tweets, input_dates1, input_dates2, r){



  df$created_at <- as.Date(df$created_at)

  #df <- df %>% filter(between(created_at, as.Date(input_dates1), as.Date(input_dates2)))

  # replace tweet length with length
  selected_metrics <-   stringr::str_replace(selected_metrics, "tweet_length", "length")
  selected_metrics <- paste(filter_type, selected_metrics, sep = "_")
  if (num_tweets == T){
    selected_metrics <- c(selected_metrics, "N")
  }

  if (length(selected_metrics) > 1){

    df_values <- df %>% select(selected_metrics)%>%
     {if (length(selected_metrics) > 1) scale(.) else .}


    don <- xts::xts(x = df_values, order.by = df$created_at)




    dygraphs::dygraph(don) %>%
    dygraphs::dyOptions(axisLineWidth = 2, drawGrid = FALSE) %>%
    dygraphs::dyLegend(width = 600) %>%

    dygraphs::dyRangeSelector(r$dates + 1) %>%
    dygraphs::dyShading(from = min(df$created_at), to = max(df$created_at), color = "white")


  } else{

    #### in case of single time series add ribbon

    decreasing <- which(df[[selected_metrics]] < mean(df[[selected_metrics]]))
    increasing <- which(df[[selected_metrics]] >= mean(df[[selected_metrics]]))

    dyData <- xts::xts(df[[selected_metrics]], order.by = df$created_at)

    ribbonData <- rep(0, nrow(dyData))
    ribbonData[decreasing] <- 0.5
    ribbonData[increasing] <- 1

    dygraphs::dygraph(dyData) %>%
      dygraphs::dyRibbon(data = ribbonData, top = 0.1, bottom = 0.02) %>%
      dygraphs::dyOptions(axisLineWidth = 2, drawGrid = FALSE) %>%
      dygraphs::dyLegend(width = 600) %>%

      dygraphs::dyRangeSelector(r$dates + 1) %>%
      dygraphs::dyShading(from = min(df$created_at), to = max(df$created_at), color = "white")

  }
}




