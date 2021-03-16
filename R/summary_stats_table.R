
### this funtion creates the summar statistics table in tabs descriptives
#'@export
#'@rdname sum_stats_table_creator

## setup dataframe for average summary statistics
sum_stats_table_creator <- function(df_need, input_date1, input_date2){


df_need <-   df_need %>%
  filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) ### filter for date range

### account for empty df after filtering
if(dim(df_need)[1] == 0){
  return()
}


df_need <- df_need %>%
  select(!contains("sentiment_")) %>% #### get correct names that are needed, get everyhing without sentiment
  select(starts_with(c("mean","median", "std"))) %>% ### get means, median, stf and take there averages
  summarise_all(mean) %>%
  cbind(
    ### now add qunatile info
   df_need %>%
         filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
         select(!contains("sentiment_")) %>%
         select(starts_with(c("q"))) %>%
         summarise_all(mean) %>% ### take mean of qunatiles, approximation using aggregated data
        summarise_all(as.integer) ### convert to integer
    ) %>%
  cbind(
    ##### add maximum info
    df_need%>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(!contains("sentiment_"))  %>% summarise_at(vars(starts_with("max")), max) ## here take max
  ) %>%
  cbind(
    ### add minimum info
    df_need %>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(!contains("sentiment_")) %>% summarise_at(vars(starts_with("min")), min) ### her take min
  ) %>%
  cbind(
    ### add number of tweets info, here take acutal sum stats because data is not aggreagted but summed on a daily level
    df_need %>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(N) %>%

      summarise(std_N = sd(N),
                mean_N = mean(N),
                median_N = median(N),
                q25_N = quantile(N, 0.25),
                q75_N = quantile(N, 0.75),
                min_N = min(N),
                max_N = max(N))
  ) %>%
  round(2) %>%
  ### create long dataframe
  pivot_longer(everything(),
               names_to = c(".value", "variable"),
               #prefix = "mean",
               names_pattern = "(.+)_(.+)") ### cnvert names to nicer names for display



  ### convert column names
  names(df_need) <- names(df_need) %>% toupper()

  # convert variable names
  df_need[,1] <- c("Retweets", "Likes", "Tweet Length", "Sentiment", "N")
  ### return kable table
  return(knitr::kable(df_need, "html") %>%
           column_spec(1:8, color = "lightgrey") %>%
           column_spec(1, bold = T, color = "white") %>%
           row_spec(1, bold = T) %>%
           kableExtra::kable_styling(c("striped","hover"), full_width = T,
                                     position = "center",
                                     font_size = 16))


}





