#'@export
#'@rdname violin_plotter
#'
violin_plotter <- function(df, selected_metrics, filter_type){


  selected_metrics_new <-   stringr::str_replace(selected_metrics, "tweet_length", "length")
  selected_metrics_new <- paste(filter_type, selected_metrics_new, sep = "_")

  df %>%
    ggplot() +
    geom_violin(aes_string(1, selected_metrics_new[1]))



}
