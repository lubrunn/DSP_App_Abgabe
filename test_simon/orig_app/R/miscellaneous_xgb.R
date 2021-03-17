#' @export
#' @rdname kable_graph

kable_tab <- function(res,text){

  knitr::kable(res, caption = glue(text),colnames = NULL) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)


}

#' @export
#' @rdname prep_custom
prep_custom <- function(list_dfs){
df <- data.frame((sapply(list_dfs,c)))
df <- df[!duplicated(as.list(df))]
df <- df %>% dplyr::select(-contains("."))
df$Dates <- as.Date(df$Dates)
cols <- setdiff(colnames(df), "date")
df <- df[,cols]

return(df)
}

#' @export
#' @rdname train_default

default_prep <- function(res,input_var,ahead,ftype,part){

res <- lag_cols(res,input_var)

help_df <- res %>% select_if(~ !any(is.na(.)))

help_df <- make_ts_stationary(help_df)

missing_names <- colnames(res)[!complete.cases(t(res))]

res <- left_join(help_df,res[,c("Dates",missing_names)])

if(part == 1){
  list_dfs <- split_data_for(res,ahead,ftype,input_var)
}else{
  list_dfs <- split_data_for_ahead(res,ahead,ftype)
}

res <- ARMA_creator(res,input_var)

cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
res <- res[,c("date",cols)]
list_dfs$df_train <- left_join(list_dfs$df_train,res)
return(list_dfs)
}
#
# res <- lag_cols(res,input$regression_outcome_xgb)
# res <- make_ts_stationary(res)
# list_dfs <- split_data_for_ahead(res,input$n_ahead2,input$ftpye2)
# res <- ARMA_creator(res,input$regression_outcome_xgb)
#
# cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
# res <- res[,c("date",cols)]
# list_dfs$df_train <- left_join(list_dfs$df_train,res)


#' @export
#' @rdname train_default
pred_output <- function(res,input_var,model){

colnames(res$df_forecast)[which(names(res$df_forecast) == input_var)] <- "y"

colnames(res$df_train)[which(names(res$df_train) == input_var)] <- "y"

preds <- model %>%
  fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
  predict(new_data = res$df_forecast[,c(-1)])
return(preds)
}


#
# colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
#
# colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
# model <- model_xgbi()[[1]]
#
# preds <- model %>%
#   fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
#   predict(new_data = res$df_forecast[,c(-1)])
#



#' @export
#' @rdname train_default
metrics_out <- function(res,fits){

out <- NULL

diff <- (fits[,1]-res$df_train[,"y"])
mean_diff <- mean(diff[,1]^2)
out$rsme <- sqrt(mean_diff)
out$mean_abs <- mean(abs(diff[,1]))
diff_per <- ((res$df_train[,"y"]-fits[,1])/res$df_train[,"y"])
diff_per <- diff_per[!is.infinite(rowSums(diff_per)),]
out$mape <- mean(abs(diff_per)*100)

return(out)

}

#' @export
#' @rdname train_default
metrics_out_final <- function(preds,y){
out <- NULL
diff <- (preds[,1]-y[,2])
mean_diff <- mean(diff[,1]^2)
out$rsme <- sqrt(mean_diff)
out$mean_abs <- mean(abs(diff[,1]))
diff_per <- ((y[,2]-preds[,1])/y[,2])
out$mape <- mean(abs(diff_per[,1])*100)
return(out)

}
