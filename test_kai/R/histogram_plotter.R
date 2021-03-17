
### here is the function that computes the histograms

#'@export
#'@rdname histogram_plotter
#'
#

histogram_plotter <- function(df, date_input1, date_input2, input_bins, input_log){

  ## second column is always column of interest
  input_metric <- names(df)[2]
 ### tunr of scientifc notation
  options(scipen=999)

  # count number of tweets per metric bin
  df <- df[created_at >= as.Date(date_input1) &
             created_at <= as.Date(date_input2),
           list(sum_n = sum(N)),
           by = c(input_metric)]

  ## if emtpy after filtering return
  if (dim(df)[1] == 0){
    return()
  }

  # take log if asked
  if (input_log == T){
  df[, metric := log(get(input_metric) + 1)]
  } else{
    setnames(df, input_metric, "metric")
  }

  # cut into intervalls according to bin input
  df[, bins := cut_interval(metric, n = input_bins)]
  #### count bins
  df <- df[, .(sum_n = sum(sum_n)), by = bins]


  #### create tick marks series
  df <- df %>% separate(bins, c("bin1", "bin2"), ",")
  ## remove brackets
  df$bin1 <- gsub("[(]", "", df$bin1)
  df$bin1 <- gsub("\\[|\\]", "", df$bin1)
  df$bin2 <- gsub("[)]", "", df$bin2)
  df$bin2 <- gsub("\\[|\\]", "", df$bin2)


  # convert to numeric
  df$bin1 <- as.numeric(df$bin1)
  df$bin2 <- as.numeric(df$bin2)

  ### take mean
  #df <- df %>% mutate(mean_bin = rowMeans(select(df, bin1, bin2), na.rm = T))


  # replace names of metrics with nicer names
  input_metric <- stringr::str_replace(input_metric, "sentiment_rt_rd", "Retweets weighted Sentiment")
  input_metric <- stringr::str_replace(input_metric, "sentiment_likes_rd", "Likes weighted Sentiment")
  input_metric <- stringr::str_replace(input_metric, "sentiment_length_rd", "Tweet Length weighted Sentiment")
  input_metric <- stringr::str_replace(input_metric, "likes_count", "Likes")
  input_metric <- stringr::str_replace(input_metric, "retweets_count", "Retweets")
  input_metric <- stringr::str_replace(input_metric, "tweet_length", "Tweet Length")
  input_metric <- stringr::str_replace(input_metric, "sentiment_rd", "Sentiment")

  names(df) <- stringr::str_replace(names(df), "sum_n", "N")
  names(df) <- stringr::str_replace(names(df), "bin1", input_metric)


  ## show N in thousands




### find distance between first two bins for setting width in geom_col
 # df <- df %>% arrange(.data[[input_metric]])
 # min_dist <- df[[input_metric]][2] - df[[input_metric]][1]

  # plot

  p <- df %>%
    select(1,3) %>%


    ggplot(aes(.data[[input_metric]], N)) +
    geom_col(color = "black", fill = "grey") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme_classic() +
    geom_hline(yintercept = 0) +
    labs(y = "N")


  plotly::ggplotly(p)

}
