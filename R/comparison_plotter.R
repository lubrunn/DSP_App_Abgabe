

stock_plotter <- function(df, input_metric, input_comp, input_roll){




####### filter out relevant variable and companies
    df <- df %>% select(Dates, input_metric, name) %>%
      filter(
          grepl(paste(input_comp, collapse = "|"), name))
    ## convert to wide format for dygraph
    df <- df %>% pivot_wider(names_from = name, values_from = input_metric)
    ##### one df for values, one for dates
    df_values <- df %>% select(-Dates)
    df_dates <- as.Date(df$Dates)
    # set up time series
    don <- xts::xts(df_values, df_dates)

    ### plot
    dygraphs::dygraph(don,
                      ylab = input_metric,
                      group = "comp_plots") %>%
      dygraphs::dySeries(label = input_metric,) %>%
      {if (!grepl("return", input_metric)) dygraphs::dyRebase(.,value = 100) else . } %>%
      dygraphs::dyOptions(axisLineWidth = 2) %>%
      dygraphs::dyLegend() %>%
      dygraphs::dyShading(from = min(df_dates), to = max(df_dates), color = "white") %>%
      {if (input_roll == T) dygraphs::dyRoller(., rollPeriod = 7, showRoller = F) else .}



}






covid_plotter <- function(df, selected_metric, input_country, input_roll = F){

  ## select relevant variables
  df <- df %>% select(date, selected_metric, location)

  # widen df
  df <- df %>% pivot_wider(names_from = location, values_from = selected_metric)

  # covnert to date for xts
  df$date <- as.Date(df$date)
  # convert to xts
  don <- xts::xts(subset(df, select = input_country), df$date)


  ##### format selected metrics so it looks nicer
  selected_metric <- gsub("_", " ", selected_metric) %>% stringr::str_to_title()


  dygraphs::dygraph(don,
                    ylab = selected_metric,
                    group = "comp_plots") %>%
   {if (length(input_country) > 1)dygraphs::dySeries(.) else dygraphs::dySeries(.,label = input_country)}  %>%
    dygraphs::dyOptions(axisLineWidth = 2) %>%
    dygraphs::dyLegend() %>%
    dygraphs::dyShading(from = min(df$date), to = max(df$date), color = "white") %>%
    {if (input_roll == T) dygraphs::dyRoller(., rollPeriod = 7, showRoller = F) else .}



  }





