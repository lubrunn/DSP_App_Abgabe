#' @export
#' @rdname prep_custom
prep_custom <- function(list_dfs){
  ##### combine in list
  df <- data.frame((sapply(list_dfs,c)))
  ###### delete duplicates based on row values: This duplicated is created when putting 1 as text input
  df <- df[!duplicated(as.list(df))]
  ##### delete duplicates detected by pattern: "."
  #if(any(names(df) == 'Adj.Close') | any(names(df) == 'OFR.FSI'))
  df <- df %>% dplyr::select(-contains("."))
  ##### create date column
  df$Dates <- as.Date(df$Dates,origin = "1970-01-01")
  ##### delete wrong date columns
  cols <- setdiff(colnames(df), "date")
  df <- df[,cols]

  return(df)
}


# ###### combine the single dataframes from each variable in on large dataframe
# ##### start combination based on action button
# custom_df <- eventReactive(input$finish, {
#   ##### combine in list
#   list_dfs <- c(xchange$df_full,xchange$df_full2,xchange$df_full3,xchange$df_full4)
#   ##### create dataframe from list of dataframes
#   df <- data.frame((sapply(list_dfs,c)))
#   ###### delete duplicates based on row values: This duplicated is created when putting 1 as text input
#   df <- df[!duplicated(as.list(df))]
#   ##### delete duplicates detected by pattern: "."
#   df <- df %>% dplyr::select(-contains("."))
#   ##### create date column
#   df$Dates <- as.Date(df$Dates,origin = "1970-01-01")
#   ##### delete wrong date columns
#   cols <- setdiff(colnames(df), "date")
#   df <- df[,cols]
#   ##### add corona dummy if user selects "yes"
#   if(input$corona_dummy == "yes"){
#     df <- corona_dummy(df)
#   }else{
#     df
#   }
#   df
# })

#' @export
#' @rdname train_default
default_prep <- function(res,input_var,ahead,ftype,part){
  #### lag features by one day
  res <- lag_cols(res,input_var)
  ###### delete duplicates based on row values: This duplicated is created when putting 1 as text input
  help_df <- res %>% select_if(~ !any(is.na(.)))
  ##### call function to check for stationarity
  ##### returns differenced dataframe for columns which did not pass the test
  help_df <- make_ts_stationary(help_df)
  #### store names of columns with NAs in vector for reference in the next step
  missing_names <- colnames(res)[!complete.cases(t(res))]

  if(!is.null(missing_names)){
    res
  }else{#### join columns back to dataframe
    res <- left_join(help_df,res[,c("Dates",missing_names)])
  }

  if(part == 1){ ##### split data into training and forecast dataset
    ##### lists are created for the "Validity" tab
    list_dfs <- split_data_for(res,ahead,ftype,input_var)
  }else{##### lists are created for the "Actual" forecast tab because the full time period can be exploited
    list_dfs <- split_data_for_ahead(res,ahead,ftype)
  }
  ##### create the AR and MA features
  res <- ARMA_creator(res,input_var)
  ##### create dataframe with unique columns
  cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
  res <- res[,c("date",cols)]
  list_dfs$df_train <- left_join(list_dfs$df_train,res)
  return(list_dfs)
}



#' @export
#' @rdname train_default
pred_output <- function(res,input_var,model){

  colnames(res$df_forecast)[which(names(res$df_forecast) == input_var)] <- "y"

  colnames(res$df_train)[which(names(res$df_train) == input_var)] <- "y"


  preds <- model %>%
  parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
    predict(new_data = res$df_forecast[,c(-1)])
  return(preds)
}

#
# ##### load dataframe
# res <- df_xgb_train()
# ##### rename dependent variable as y in train and forecast dataset
# colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
# colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
#
# ##### load model
# model <- model_xgbi()[[1]]
# ##### predict
# preds <- model %>%
#   parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
#   predict(new_data = res$df_forecast[,c(-1)])
#
#
#
#' @export
#' @rdname kable_graph

kable_tab <- function(res,text){

  knitr::kable(res, caption = glue(text),colnames = NULL) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)


}

