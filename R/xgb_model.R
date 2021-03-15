#' Plot Output Functions
#' @export
#' @rdname xgboost
model_xgb <- function(res){
  set.seed(123) # should i do it dynamic

  names(res)[2] <- "y"

  slidng_eval_window <- rsample::sliding_period(res,index = date,"month",lookback = 5  , assess_stop = 1,step = 2)

  res$date <- NULL

  preprocessing_recipe <-
    recipes::recipe(y ~ ., data = res) %>% recipes::prep()

  model_xgboost <-  parsnip::boost_tree(
    mode = "regression",
    mtry = 30,
    trees = 200,
    min_n = 25,
    tree_depth = 7,
    learn_rate = 0.02,
    loss_reduction = 0.0009,
    sample_size = 0.7) %>%
    parsnip::set_engine(engine = "xgboost", objective = "reg:squarederror")

  xgboost_wf <-
    workflows::workflow() %>%
    workflows::add_model(model_xgboost) %>%
    workflows::add_formula(y ~ .)


  single_fits <-
    xgboost_wf %>%
    tune::fit_resamples(slidng_eval_window)

  l_metrics <- tune::collect_metrics(single_fits)

  df_metrics <- single_fits %>% dplyr::select(id, .metrics) %>%  unnest(.metrics) %>% group_by(.metric)


  xgboost_best_params <- single_fits %>%
    tune::select_best("rmse")

  xgboost_model_final <- model_xgboost %>%
    tune::finalize_model(xgboost_best_params)

  train_processed <- recipes::bake(preprocessing_recipe,  new_data = res)

  model <- xgboost_model_final %>%
    parsnip::fit(
      formula = y ~ .,
      data    = train_processed
    )
  return_list <- list(xgboost_model_final,df_metrics,l_metrics)
  return(return_list)

}

#' @export
#' @rdname xgboost
model_xgb_custom <- function(res,mtry,trees,min_n,tree_depth,learn_rate,loss_reduction,
                             sample_size){

  set.seed(123) # should i do it dynamic

  names(res)[2] <- "y"

  slidng_eval_window <- rsample::sliding_period(res,index = date,"month",lookback = 5  , assess_stop = 1,step = 2)

  res$date <- NULL

  preprocessing_recipe <-
    recipes::recipe(y ~ ., data = res) %>% recipes::prep()


  model_xgboost <-  parsnip::boost_tree(
    mode = "regression",
    mtry = mtry,
    trees = trees,
    min_n = min_n,
    tree_depth = tree_depth,
    learn_rate = learn_rate,
    sample_size = sample_size,
    loss_reduction = loss_reduction) %>%
    parsnip::set_engine(engine = "xgboost", objective = "reg:squarederror")

  xgboost_wf <-
    workflows::workflow() %>%
    workflows::add_model(model_xgboost) %>%
    workflows::add_formula(y ~ .)

  single_fits <-
    xgboost_wf %>%
    tune::fit_resamples(slidng_eval_window)

  l_metrics <- tune::collect_metrics(single_fits)

  df_metrics <- single_fits %>% dplyr::select(id, .metrics) %>%  unnest(.metrics) %>% group_by(.metric)


  xgboost_best_params <- single_fits %>%
    tune::select_best("rmse")

  xgboost_model_final <- model_xgboost %>%
    tune::finalize_model(xgboost_best_params)

  train_processed <- recipes::bake(preprocessing_recipe,  new_data = res)

  model <- xgboost_model_final %>%
    parsnip::fit(
      formula = y ~ .,
      data    = train_processed
    )
  return_list <- list(xgboost_model_final,df_metrics,l_metrics)
  return(return_list)
}
#' @export
#' @rdname xgboost

model_xgb_hyp <- function(res,trees_hyp,grid_size){
  set.seed(123) # should i do it dynamic

  names(res)[2] <- "y"

  slidng_eval_window <- rsample::sliding_period(res,index = date,"month",lookback = 5  , assess_stop = 1,step = 2)

  res$date <- NULL

  preprocessing_recipe <-
    recipes::recipe(y ~ ., data = res) %>% recipes::prep()


  xgboost_model <-
    parsnip::boost_tree(
      mode = "regression",
      trees = trees_hyp,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
    parsnip::set_engine("xgboost", objective = "reg:squarederror")

  xgboost_params <-
    dials::parameters(
      min_n(),
      tree_depth(),
      learn_rate(),
      loss_reduction()
    )

  xgboost_grid <-
    dials::grid_max_entropy(
      xgboost_params,
      size = grid_size
    )


  xgboost_wf <-
    workflows::workflow() %>%
    workflows::add_model(xgboost_model) %>%
    workflows::add_formula(y ~ .)

  xgboost_tuned <- tune::tune_grid(
    object = xgboost_wf,
    resamples = slidng_eval_window,
    grid = xgboost_grid,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae),
    control = tune::control_grid(verbose = TRUE)
  )

  l_metrics <- collect_metrics(xgboost_tuned)

  df_metrics <- xgboost_tuned %>% dplyr::select(id, .metrics) %>%  tidyr::unnest(.metrics) %>% group_by(.metric)

  xgboost_best_params <- xgboost_tuned %>%
    tune::select_best("rmse")

  xgboost_model_final <- xgboost_model %>%
    tune::finalize_model(xgboost_best_params)



  train_processed <- recipes::bake(preprocessing_recipe,  new_data = res)

  xgboost_model_final %>%
    parsnip::fit(
      formula = y ~ .,
      data    = train_processed
    )

  return_list <- list(xgboost_model_final,df_metrics,l_metrics)
  return(return_list)
}
