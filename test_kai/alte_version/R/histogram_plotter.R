#'@export
#'@rdname histogram_plotter
#'
#

histogram_plotter <- function(df, date_input1, date_input2, input_bins, input_log){

  ## second column is always column of interest
  input_metric <- names(df)[2]



  # count number of tweets per metric bin
  df <- df[created_at >= as.Date(date_input1) &
             created_at <= as.Date(date_input1),
           list(sum_n = sum(N)),
           by = c(input_metric)]

  # take log if asked
  if (input_log == T){
  df[, metric := log(get(input_metric) + 0.0001)]
  } else{
    setnames(df, input_metric, "metric")
  }

  # cut into intervalls according to bin input
  df[, bins := cut_interval(metric, n = input_bins)]
  # plot
  df %>%
    ggplot(aes(bins, sum_n)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

}
