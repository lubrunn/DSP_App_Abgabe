#' Plot Output Functions
#' @export
#' @rdname density

density_plot_reg <- function(filtered_df) {

  aa <- filtered_df
  #res$date <- as.Date(res$date)
  a <- aa %>%  pull(Close)
  returns <- diff(a)/a[-length(a)]
  bb <- as.data.frame(returns)
  q <- quantile(returns,probs = c(0.1,0.9))

 ggplot(bb, aes_string("returns")) +
    geom_density(color="black",size=1) + labs(x = "returns", y = "density") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(colour = "transparent", fill = "white")) +
    stat_function(fun = dnorm, args = list(mean = mean(bb$returns), sd = sd(bb$returns)),
                  aes(linetype = "Normal distribution"),size = 0.7,colour = "red") +
    geom_vline(xintercept = q, linetype = "longdash")


}

#' @export
#' @rdname correlation
acf_plot_xgb <- function(df,variable){
  
  var <- df[,variable]
  ggAcf(var) + ggtitle(paste("Autocorrelation Function for",variable))  
  
}

#' @export
#' @rdname correlation

pacf_plot_xgb <- function(df,variable){
  
  var <- df[,variable]
  ggPacf(var) + ggtitle(paste("Partial autocorrelation Function for",variable))  
  
}


#' @export
#' @rdname ts

forcast_plot_xgb <- function(res,preds,full_df,plot_type,variable,stock){
  
preds <- preds %>%
  zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))
names(preds)[1] <- "predicted"
if(plot_type == "Full"){
  
  ts <- full_df %>% pull(variable) %>%
    zoo(seq(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), by = "day"))
  
  {cbind(actual=ts, predicted=preds)} %>% dygraph(main = glue("Forecast for {stock}"), 
                                                   ylab = variable) %>%  
    dygraphs::dyOptions(axisLineWidth = 2) %>%
    dygraphs::dyLegend() %>%
    dygraphs::dyShading(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), color = "white") %>% 
  dyEvent(as.Date(max(res$df_train$date)), "Start forecast", labelLoc = "bottom",color = "red")
  

  
}else if(plot_type == "Forecasted"){
  ts <- full_df %>% pull(variable) %>%
    zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))
  
  {cbind(actual=ts, predicted=preds)} %>% dygraph(main = glue("Forecast for {stock}"), 
                                                  ylab = variable) %>%  
    dygraphs::dyOptions(axisLineWidth = 2) %>%
    dygraphs::dyLegend() %>%
    dygraphs::dyShading(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), color = "white") %>% 
    dyEvent(as.Date(max(res$df_train$date)), "Start forecast", labelLoc = "bottom",color = "red")
  
}
  
# }else{
#   colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
#   
#   model_xgboost <-  model_xgbi()[[1]] %>%
#     fit(formula = y ~ .,data = res$df_train[,c(-1)])
#   plot <- xgb.importance(model=model_xgboost$fit) %>% xgb.ggplot.importance(
#     top_n=20, measure=NULL, rel_to_first = F)
#   plot
# }

}


