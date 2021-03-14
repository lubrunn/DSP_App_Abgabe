
#' @export
#' @rdname xgboost_prep

split_data <- function(sample,spliti){
  names(sample)[1] <- "date"
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date),
                  quarter = lubridate::quarter(date,fiscal_start = 1),
                  semester = lubridate::semester(date))

  n_sample <- round(nrow(sample)*spliti)
  split.at = sample[n_sample,"date"]
  split = which(sample$date<split.at)
  out <- NULL
  out$df.train <-  sample[split,] # Predictor training data set
  out$y.train <- sample[split,c(2)] # Outcome for training data set
  out$date.train <- sample[split,c(1)] # Date, not a predictor but useful for plotting

  out$df.test <- sample[-split,] # Predictors for testing/evaluation data set
  out$y.test <- sample[-split,c(2)] # Outcome for testing data set
  out$date.test <- sample[-split,c(1)] # date for test data set

  return(out)
}
#' @export
#' @rdname xgboost_prep
AR_creator <- function(df,variable,lag){
  names(df)[1] <- "date"
  df$date <- as.Date(df$date)
  
  if(lag > 0){
  df <- df[,c("date",variable)]
  df <- df %>%  tk_augment_lags(contains(variable), .lags = 1:lag)
  df <- df[,c(-1,-2)]
  return(df)
  }else{
    return(df)
  }
}

#' @export
#' @rdname xgboost_prep
MA_creator <- function(df,variable,avg_len){
  
  if(avg_len > 0){
  avg_len <- as.numeric(avg_len)
  x <- zoo(df[,variable])

  df <- as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))
  names(df)[1] <- paste("MA_",variable,sep = "")
  return(df)
  }else{
  return(df)
  }

}
#' @export
#' @rdname xgboost_prep
# input variable for "Close"
ARMA_creator <- function(res,variable){
  
   names(res)[1] <- "date"
   
   help_df <- res %>% dplyr::select(-date)
   
   list_var <- names(help_df)
   
   optlags <- NULL
                                  #or just rename to y
   for(i in list_var[-1]){      #insert here
     lag <- VARselect(help_df[,c(variable,i)],lag.max = 10, type = "const")$selection[["AIC(n)"]]
     optlags <- c(optlags,lag)
   }
                            # insert here
   lag <- VARselect(help_df[,variable],lag.max = 10, type = "const")$selection[["AIC(n)"]]

   optlags <- c(optlags,lag)
   
   list_ma <- rep(5,each=ncol(help_df))
   
   bb <- mapply(c,optlags, list_var, SIMPLIFY = T)
   cc <- mapply(c,list_ma, list_var, SIMPLIFY = T)

  

  for(i in 1:length(list_var)){
    cols_ar <- AR_creator(res,bb[2,i],bb[1,i])
    res <- cbind(res,cols_ar)
  }
  for(i in 1:length(list_var)){
    cols_ma <- MA_creator(res,cc[2,i],cc[1,i])
    res <- cbind(res,cols_ma)
  }
  
  return(res)
}
#' @export
#' @rdname xgboost_prep
ARMA_creator_for <- function(res,res_pre){
 
  res_pre <- res_pre[1:nrow(res),]
  list_col <- names(res_pre)[!(names(res_pre) %in% names(res))]  
  
  for(i in list_col){
    res[,i] <- rep(NA,nrow(res))
  }
  
  res[, list_col] <- lapply(list_col, function(x) as.numeric(res[[x]]))
  
  return(res)
}



#' @export
#' @rdname xgboost_prep
lag_cols <- function(res,variable){    #input variable for "Close"
                              #insert here
  b <- res %>% dplyr::select(-Dates,-variable) %>%  lag(1)
                              #insert here
  res <- res %>% dplyr::select(Dates,variable) %>%  cbind(b)
  
  res <- res[-1,]
  
  return(res)
}

#' @export
#' @rdname xgboost_prep
make_ts_stationary <- function(res){
    
 for(i in 2:ncol(res)){ 
 # optlags <- VARselect(res[,i],lag.max = 10, 
 #                      type = "const")$selection[["AIC(n)"]]
 #   
  if(adf.test(res[,i],k=2)$p.value > 0.1){
    res[,i] <- c(diff(res[,i],1),NA)
    
    }
 
  }
  res <- drop_na(res)
  
  return(res)
}
#' @export
#' @rdname xgboost_prep   
split_data_for <- function(sample,n_ahead,ftype,variable){
  names(sample)[1] <- "date"
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date),
                  quarter = lubridate::quarter(date,fiscal_start = 1),
                  semester = lubridate::semester(date))
  
  n <- dim(sample)[1]
  out <- NULL
  out$df_train <- sample[1:(n-n_ahead),]
  out$df_forecast <- sample[(n-n_ahead)+1:n,c("date","days","weeks","months","years","quarter","semester")]
  out$y_forecast <- sample[(n-n_ahead)+1:n,variable] #insert here
  out$f_dates <- sample[(n-n_ahead)+1:n,"date"]
  
  out$df_forecast <- drop_na(out$df_forecast)
  out$y_forecast <- out$y_forecast[1:n_ahead]
  out$f_dates <-  out$f_dates[1:n_ahead]
  
  n_train <- dim(out$df_train)[1]
  past_features <- out$df_train[(n_train-n_ahead)+1:n_train,]
  past_features <- drop_na(past_features)
  
  #out$y_forecast <- past_features %>% dplyr::select(Close)
  
  if(ftype == "past_features"){
  
  # append past X's to test data
  past_features <- past_features %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester)
  
  out$df_forecast <- cbind(out$df_forecast,past_features)
  #out$date_forecast <- out$df_forecast %>% dplyr::select(date)
  
  
  }else if(ftype == "forecasted_features"){
  covariates <- out$df_train %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester) %>% 
      names()

  for(i in covariates){
    
  Lambda <- BoxCox.lambda(out$df_train[,i])
  arima_fit <-  auto.arima(out$df_train[,i],D=1,approximation = F,allowdrift = T,
                           allowmean = T,seasonal = T,lambda = Lambda)
  preds_cov <- forecast(arima_fit,h = n_ahead)
  
  out$df_forecast <- cbind(out$df_forecast,fcast = preds_cov$mean)
  names(out$df_forecast)[ncol(out$df_forecast)] <- paste0(i)
  
  # out$date_forecast <- out$df_forecast %>% dplyr::select(date)
  
    }
  }else{
    help_df <- data.frame(matrix(ncol = ncol(sample)-ncol(out$df_forecast)
                                 , nrow = n_ahead))
    x_names <- out$df_train %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester) %>% names()
    colnames(help_df) <- x_names
    
    out$df_forecast <- cbind(help_df,out$df_forecast)
    #move date column to the first position
    out$df_forecast <- out$df_forecast[,c(which(colnames(out$df_forecast)=="date"),which(colnames(out$df_forecast)!="date"))]

    #numeric
    
    out$df_forecast[, x_names] <- lapply(x_names, function(x) as.numeric(out$df_forecast[[x]]))
 
   }
  return(out)
}

#' @export
#' @rdname xgboost_prep
split_data_for_ahead <- function(sample,n_ahead2,ftype2){
  names(sample)[1] <- "date"
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date),
                  quarter = lubridate::quarter(date,fiscal_start = 1),
                  semester = lubridate::semester(date))
  
  n <- dim(sample)[1]
  out <- NULL
  out$df_train <- sample
  
  dats_seq <- seq(from = as.Date(max(sample$date))+1, 
                  to = as.Date(max(sample$date)) + n_ahead2, by = "day")
  
  out$df_forecast <- data.frame(matrix(ncol = 1 , nrow = n_ahead2 ))
  x <- c("date")
  colnames(out$df_forecast) <- x
  
  out$df_forecast$date <- dats_seq
  
  out$df_forecast <- out$df_forecast %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date),
                  quarter = lubridate::quarter(date,fiscal_start = 1),
                  semester = lubridate::semester(date))
  
  n_train <- dim(out$df_train)[1]
  past_features <- out$df_train[(n_train-n_ahead2)+1:n_train,]
  past_features <- drop_na(past_features)

  
  
  if(ftype2 == "past_features"){
    
    # append past X's to test data
    past_features <- past_features %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester)
    
    out$df_forecast <- cbind(out$df_forecast,past_features)
    #out$date_forecast <- out$df_forecast %>% dplyr::select(date)
    
    
  }else if(ftype2 == "forecasted_features"){
    covariates <- out$df_train %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester) %>% 
      names()
    
    for(i in covariates){
      
      Lambda <- BoxCox.lambda(out$df_train[,i])
      arima_fit <-  auto.arima(out$df_train[,i],D=1,approximation = F,allowdrift = T,
                               allowmean = T,seasonal = T,lambda = Lambda)
      preds_cov <- forecast(arima_fit,h = n_ahead2)
      
      out$df_forecast <- cbind(out$df_forecast,fcast = preds_cov$mean)
      names(out$df_forecast)[ncol(out$df_forecast)] <- paste0(i)
      
      # out$date_forecast <- out$df_forecast %>% dplyr::select(date)
      
    }
  }else{
    help_df <- data.frame(matrix(ncol = ncol(sample)-ncol(out$df_forecast)
                                 , nrow = n_ahead2))
    x_names <- out$df_train %>% dplyr::select(-date,-days,-weeks,-months,-years,-quarter,-semester) %>% names()
    colnames(help_df) <- x_names
    
    out$df_forecast <- cbind(help_df,out$df_forecast)
    #move date column to the first position
    out$df_forecast <- out$df_forecast[,c(which(colnames(out$df_forecast)=="date"),which(colnames(out$df_forecast)!="date"))]
    #numeric
    
    out$df_forecast[, x_names] <- lapply(x_names, function(x) as.numeric(out$df_forecast[[x]]))
    
  }
  return(out)
}

#' @export
#' @rdname xgboost_prep
split_data_eval <- function(sample,n_ahead3){
  names(sample)[1] <- "date"
  
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date),
                  quarter = lubridate::quarter(date,fiscal_start = 1),
                  semester = lubridate::semester(date))
  
  n <- dim(sample)[1]
  out <- NULL
  out$df_train <- sample[1:(n-n_ahead3),]
  out$df_forecast <- sample[(n-n_ahead3)+1:n,]
  out$df_forecast <- drop_na(out$df_forecast)
  out$y_forecast <- sample[(n-n_ahead3)+1:n,"Close"] #insert here
  out$f_dates <- sample[(n-n_ahead3)+1:n,"date"]
 
  return(out)
}

#' @export
#' @rdname xgboost_prep
corona_dummy <- function(res){
  names(res)[1] <- "date"
  res$date <- as.Date(res$date)
  res$corona_shock_dummy <- 0
  res$corona_shock_dummy[(res$date >= "2020-02-10") & (res$date <= "2020-05-25")] <- 1
return(res)
}
#' 
#' #' @export
#' #' @rdname xgboost_prep
# Ma_part_neu <- function(res,var,list_input){

# 
#   list_nums <- regmatches(list_input, gregexpr("[[:digit:]]+", list_input))
#   num_vec <- as.numeric(unlist(list_nums))
#   single_nums <- unlist(str_split(list_input, ","))
#   variables_list <- rep(var, length(num_vec))
#   helpi <- rep("MA",length(num_vec))
#   names_vec <- paste0(helpi,variables_list, single_nums)
#   #
# 
#   help_matrix <- mapply(c,num_vec, variables_list, SIMPLIFY = T)
#   lies <- NULL
# 
#   for(i in 1:length(num_vec)){
# 
#     if(help_matrix[1,i] > 0){
#       avg_len <- as.numeric(help_matrix[1,i])
#       x <- zoo(res[,help_matrix[2,i]])
#       x <- as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))
#       names(x)[1] <- paste("MA_",help_matrix[2,i],sep = "")
#       lies[[i]] <- x
#     }else{
#       res
#     }
# 
# 
#   }
# 
#   lies <- as.data.frame(do.call(cbind, lies))
#   colnames(lies) <- names_vec
#   return(lies)
# }