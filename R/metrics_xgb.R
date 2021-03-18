

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
