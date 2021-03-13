
# con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/test.db")
# time1 <- Sys.time()
# df_need <- DBI::dbGetQuery(con, "SELECT * FROM sum_stats_de WHERE created_at >= '2018-11-30' and created_at <= '2021-02-19'
#                            and likes_count = 200 and retweets_count = 200 and tweet_length = 81")
# print(Sys.time() -  time1)
#
# df_need %>%
#   ggplot() +
#   geom_histogram(aes(retweets_count))
# Sys.time() - time1





#input = "rt"
#'@export
#'@rdname sum_stats_table_creator

## setup dataframe for average summary statistics
sum_stats_table_creator <- function(df_need, input_date1, input_date2){
df_need <-   df_need %>%
  filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
  select(!contains("sentiment_")) %>%
  select(starts_with(c("mean", "std", "q"))) %>%
  summarise_all(mean) %>%

  cbind(
    df_need%>%
      select(!contains("sentiment_"))  %>% summarise_at(vars(starts_with("max")), max)
  ) %>%
  cbind(
    df_need %>%
      select(!contains("sentiment_")) %>% summarise_at(vars(starts_with("min")), min)
  ) %>%
  cbind(
    df_need %>%
      select(N) %>%

      summarise(std_N = sd(N),
                mean_N = mean(N),
                min_N = min(N),
                max_N = max(N))
  ) %>%
  round(2) %>%
  pivot_longer(everything(),
               names_to = c(".value", "variable"),
               #prefix = "mean",
               names_pattern = "(.+)_(.+)")



  ### convert column names
  names(df_need) <- names(df_need) %>% toupper()

  # convert variable names
  df_need[,1] <- c("Retweets", "Likes", "Tweet Length", "Sentiment", "N")

  return(knitr::kable(df_need, caption = glue("Summary Statistics for the selected Metric")) %>%
           kableExtra::kable_styling(c("striped","hover"), full_width = T,
                                     position = "center",
                                     font_size = 16))

}






# a = data.frame(mean_rt = c(5,11), mean_senti = c(10,19), sd_rt = c(3,4), sd_senti= c(2,1), max_rt = c(10, 11), max_senti = c(100, 200),
#                min_rt = c(2,3), min_senti = c(2,3))
#
#
# a %>%
#   select(starts_with(c("mean", "sd", "q"))) %>%
#   summarise_all(mean) %>%
#
#   cbind(
#     a %>% summarise_at(vars(starts_with("max")), max)
#   ) %>%
#   cbind(
#     a %>% summarise_at(vars(starts_with("min")), min)
#   ) %>%
#   pivot_longer(everything(),
#                names_to = c(".value", "variable"),
#                #prefix = "mean",
#                names_pattern = "(.+)_(.+)")
#
#
#
#
# b <- df_need %>%
#   select(!contains("sentiment_")) %>%
#   select(starts_with(c("mean", "std", "q"))) %>%
#   summarise_all(mean) %>%
#
#   cbind(
#     df_need%>%
#       select(!contains("sentiment_"))  %>% summarise_at(vars(starts_with("max")), max)
#   ) %>%
#   cbind(
#     df_need %>%
#       select(!contains("sentiment_")) %>% summarise_at(vars(starts_with("min")), min)
#   ) %>%
#   cbind(
#     df_need %>%
#       select(N) %>%
#
#       summarise(std_N = sd(N),
#                 mean_N = mean(N),
#                 min_N = min(N),
#                 max_N = max(N))
#   ) %>%
#   pivot_longer(everything(),
#                names_to = c(".value", "variable"),
#                #prefix = "mean",
#                names_pattern = "(.+)_(.+)")
#
#
