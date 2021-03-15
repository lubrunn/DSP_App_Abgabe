

#' @export
#' @rdname multiple_plotting
Multiple_input <- function(filtered_df,aggregation,listi,key){
  
  if(length(aggregation) == 1){
    aggregation <- key[[aggregation]]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation)
    
  }else if(length(aggregation) == 2){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]])
    
  }else if(length(aggregation) == 3){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]])
  }else{
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]],aggregation[[4]])}
  
  
  
  
  
}

#' @export
#' @rdname multiple_plotting
ticker_dict <- function(stock){
  
  listi <- list("adidas " = "ADS.DE","Allianz " = "ALV.DE",
                "Deutsche Bank " = "DBK.DE","Delivery Hero " = "DHER.DE")
  
  res <- listi[[stock]]
  
}

#' @export
#' @rdname multiple_plotting
key <- function(str){
  
  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")
  res <- key[str]
  res[[1]]
}


#' @param test_data data object
#'
#' @export
#' @rdname dataPreps
aggregate_sentiment <- function(test_data) {
  
  test_data1 <- test_data %>%
    group_by(date,language) %>%
    summarise(sentiment_weight_retweet = weighted.mean(sentiment,
                                                       retweets_count/
                                                         sum(retweets_count)),
              sentiment_weight_likes = weighted.mean(sentiment,
                                                     likes_count/
                                                       sum(likes_count)),
              sentiment_weight_length = weighted.mean(sentiment,
                                                      tweet_length/
                                                        sum(tweet_length)),
              sentiment_mean = mean(sentiment))
  
  
  
}


#' @export
#' @rdname industry_sentiment

get_industry_sentiment <- function(de,industry,retweets_min,tweet_length){
  
  components_de <- de %>%  filter(Symbol == "ADS.DE"|Symbol == "ALV.DE" |
                                    Symbol == "DBK.DE" | Symbol == "DHER.DE")
  components_de <- components_de %>% filter(sector == industry)
  symbols <- c(components_de[["Symbol"]])
  df_total = data.frame()
  for (s in symbols) {
    
    load_data <- eval(parse(text = paste(s,'()', sep='')))
    if(tweet_length == "yes"){
      load_data <- load_data %>% filter(retweets_count > as.numeric(retweets_min)&
                                        (tweet_length > 81))
    }else{
      load_data <- load_data %>% filter(retweets_count > as.numeric(retweets_min))
    }
    senti_stock <- aggregate_sentiment(load_data)
    
    df_total <- rbind(df_total,senti_stock)
  }
  filtered_df <- df_total %>% group_by(date,language) %>%
    summarise_at(vars("sentiment_weight_retweet", "sentiment_weight_likes",
                      "sentiment_weight_length","sentiment_mean"), mean)
}

