#' UI Elements
#'
#'
#not original

#####################################################   Stocks

# sliderinput for dates
#' @export
#' @rdname uiElements
sliderinput_dates <- function(){
  sliderInput("dates",label="Timeseries",
              value = c(as.Date("2020-02-12"),as.Date("2021-02-12")),
              min = as.Date("2020-01-02"),
              max = as.Date("2021-02-12"),
              step = 1,timeFormat = "%F")
}
#plotoutput for german companies
#' @export
#' @rdname uiElements
plot_stocks_DE <- function() {
  plotOutput("plot_DE",hover = hoverOpts("plot_hover_DE", delay = 10, delayType = "debounce"),
             dblclick = "plot1_dblclick",
             brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE))
}
#hoverbox in german plot
#' @export
#' @rdname uiElements
hover_info_DE <- function() {
  uiOutput("hover_info_DE",style = "pointer-events: none")
}

################################################################## CORONA
# selectize input for corona
#' @export
#' @rdname uiElements
selectize_corona <- function() {
  selectizeInput("corona_measurement","Chose Corona measurement",
                 c("total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),
                 multiple = FALSE,
                 selected = "total_deaths")
}
# sliderinput for dates
#' @export
#' @rdname uiElements
sliderinput_dates_corona <- function(){
  sliderInput("dates_corona",label="Time",
              value = c(as.Date("2020-01-22"),Sys.Date()),
              min = as.Date("2020-01-22"),
              max = Sys.Date(),
              step = 1,timeFormat = "%F")}

#' @export
#' @rdname uiElements
plot_corona <- function() {
  plotOutput("corona_plot",hover = hoverOpts("plot_hover_corona", delay = 10, delayType = "debounce"),
             dblclick = "plot_corona_dblclick",
             brush = brushOpts(id = "plot_corona_brush",resetOnNew = TRUE))
}

#' @export
#' @rdname uiElements
hover_info_corona <- function() {
  uiOutput("hover_info_corona",style = "pointer-events: none")



}
################################################################## Regression



# selectize input for companies
#' @export
#' @rdname uiElements
selectize_Stocks_reg <- function() {
  #full_components <- rbind(components1,components2)

  selectInput("Stock_reg","Chose Company",
              c("Covestro ","adidas ","Allianz ","BASF ","Bayer ","Beiersdorf ","Bayerische Motoren Werke ",
                "Continental ","Daimler ","Deutsche Börse ","Deutsche Bank ","Delivery Hero ","Deutsche Post ",
                "Deutsche Telekom ","Deutsche Wohnen ","EON ","Fresenius Medical Care ","Fresenius ","HeidelbergCement ",
                "Henkel ","Infineon Technologies ","Linde ","MERCK ","MULTI-UNITS LUXEMBOURG - Lyxor Euro Government Bond (DR) UCITS ETF - Acc ",
                "Münchener Rückversicherungs-Gesellschaft ","RWE ","SAP ","Siemens ","Vonovia ","Volkswagen ",
                "Apple ","Amgen ","American Express Company ","The Boeing Company ","Caterpillar ","salesforcecom ",
                "Cisco Systems ","Chevron Corporation ","The Walt Disney Company ","Dow ",
                "The Goldman Sachs Group ","The Home Depot ","Honeywell International ","International Business Machines Corporation ",
                "Intel Corporation ","Johnson  Johnson ","JPMorgan Chase ","The Coca-Cola Company ","McDonald's Corporation ",
                "3M Company ","Merck ","Microsoft Corporation ","NIKE ","The Procter  Gamble Company ","The Travelers Companies ","UnitedHealth Group Incorporated ",
                "Visa ","Verizon Communications ","Walgreens Boots Alliance ","Walmart")
                ,selected = "adidas ")

}


#' @export
#' @rdname uiElements
parameter_tabsi <- function(){
  tabsetPanel(
  id = "industry_tab",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(),
           radioButtons("language1","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1", "clear selected"),
           radioButtons("minRetweet_stocks1", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)


  ),
  tabPanel("yes",
           selectInput("industry", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2","Language of tweets ?",
                        choices = c("en","de"),inline=T),

           selectizeInput("aggregation2", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2", "clear selected"),
           radioButtons("minRetweet_stocks2", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)

  )

)
}

#' @export
#' @rdname uiElements
parameter_tabs <- function(){
  tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                    "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi()

  )

)
}



#' @export
#' @rdname uiElements
tabs_custom <- function(){
  tabsetPanel(
  id = "regression_tabs",
  tabPanel("Model specifcation",
           radioButtons("country_regression","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression"),
           radioButtons("regression_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume","Return"),selected = "Close"),
           switchInput("senti_yesno_reg","Include Sentiment?",onLabel="Yes",offLabel="No"),
           conditionalPanel(
             condition = "input.regressiontabs==1",
             numericInput("Quantiles","Choose quantile",value=0.5,min=0.05,max=0.95,step = 0.05)),
           uiOutput("Controls"),
           actionButton("reset_regression", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression",label = "Timeseries",
                       value = c(as.Date("2020-01-02"),as.Date("2021-02-12")),
                       min = as.Date("2020-01-02"),
                       max = as.Date("2021-02-12"),
                       step = 1,timeFormat = "%F")



  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs()

  )

)
}

################################################################ VAR

#' @export
#' @rdname uiElements
parameter_tabsi_var <- function(){
  tabsetPanel(
    id = "industry_tab_var",
    type = "hidden",
    tabPanel("no",
             selectize_Stocks_reg(),
             radioButtons("language1_var","Language of tweets ?",
                          choices = c("en","de"),inline=T),
             selectizeInput("aggregation1_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                           "Mean weighted by likes", "Mean weighted by length"),
                            select = "Mean"),
             actionButton("reset1_var", "clear selected"),
             radioButtons("minRetweet_stocks1_var", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
             radioButtons("tweet_length_stock1_var","Tweet larger than median length:",
                          choices = c("yes","no"),selected = "no",inline=T)


    ),
    tabPanel("yes",
             selectInput("industry_var", "Industry", choices = c("Consumer Cyclical","Financial Services")),

             radioButtons("language2_var","Language of tweets ?",
                          choices = c("en","de"),inline=T),

             selectizeInput("aggregation2_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                           "Mean weighted by likes", "Mean weighted by length"),
                            select = "Mean"),
             actionButton("reset2_var", "clear selected"),
             radioButtons("minRetweet_stocks2_var", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
             radioButtons("tweet_length_stock2_var","Tweet larger than median length:",
                          choices = c("yes","no"),selected = "no",inline=T)

    )

  )
}

#' @export
#' @rdname uiElements
parameter_tabs_var <- function(){
  tabsetPanel(
    id = "params_var",
    type = "hidden",
    tabPanel("NoFilter",
             radioButtons("language_var","Choose Langugage of Tweets:",choices = c("En","De")),
             selectizeInput("aggregation_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                          "Mean weighted by likes", "Mean weighted by length"),
                            select = "Mean"),
             radioButtons("minRetweet_var", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
             radioButtons("minLikes_var", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
             radioButtons("tweet_length_var","Tweet larger than median length:",
                          choices = c("yes","no"),inline=T)


    ),
    tabPanel("Stocks",
             radioButtons("industry_sentiment_var","Sentiment by industry ?",
                          choices = c("yes","no"),selected = "no",inline=T),
             parameter_tabsi_var()

    )

  )
}


#' @export
#' @rdname uiElements
tabs_custom_var <- function(){
  tabsetPanel(
    id = "regression_tabs_var",
    tabPanel("Model specifcation",
             radioButtons("country_regression_var","Which country?",c("Germany","USA"),selected = "Germany"),
             uiOutput("stock_regression_var"),
             radioButtons("regression_outcome_var","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume","Return"),selected = "Close"),
             switchInput("senti_yesno","Include Sentiment?",onLabel="Yes",offLabel="No"),
             uiOutput("Controls_var"),
             actionButton("reset_regression_var", "clear selected"),
             #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
             #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
             sliderInput("date_regression_var",label = "Timeseries",
                         value = c(as.Date("2020-02-12"),as.Date("2021-02-12")),
                         min = as.Date("2020-01-02"),
                         max = as.Date("2021-02-12"),
                         step = 1,timeFormat = "%F")



    ),
    tabPanel("Filter sentiment input",
             selectInput("Sentiment_type_var", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                         selected = "NoFilter"),
             parameter_tabs_var()

    )

  )
}


#####################################################  Xgboost
#' @export
#' @rdname uiElements
parameter_tabsi_xgb <- function(){
tabsetPanel(
  id = "industry_tab_xgb",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(),
           radioButtons("language1_xgb","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1_xgb", "clear selected"),
           radioButtons("minRetweet_stocks1_xgb", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)
           
           
  ),
  tabPanel("yes",
           selectInput("industry_xgb", "Industry", choices = c("Consumer Cyclical","Financial Services")),
           
           radioButtons("language2_xgb","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           
           selectizeInput("aggregation2_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2_xgb", "clear selected"),
           radioButtons("minRetweet_stocks2_xgb", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)
           
  )
  
)

}



#' @export
#' @rdname uiElements
parameter_tabs_xgb <- function(){
tabsetPanel(
  id = "params_xgb",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language_xgb","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                    "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet_xgb", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes_xgb", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)
           
           
  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment_xgb","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi_xgb()
           
  )
  
)

}


#' @export
#' @rdname uiElements
tabs_custom_xgb <- function(){
tabsetPanel(
  id = "regression_tabs_xgb",
  tabPanel("Model specifcation",
           radioButtons("country_regression_xgb","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression_xgb"),
           radioButtons("regression_outcome_xgb","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close",inline = T),
           switchInput("senti_yesno_xgb","Include Sentiment?",onLabel="Yes",offLabel="No"),
           uiOutput("Controls_xgb"),
           actionButton("reset_regression_xgb", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression_xgb",label = "Timeseries",
                       value = c(as.Date("2020-02-12"),as.Date("2021-02-12")),
                       min = as.Date("2020-01-02"),
                       max = as.Date("2021-02-12"),
                       step = 1,timeFormat = "%F"),
           radioButtons("country_corona_xgb","Which country ?",c("Germany","United States"),selected = "Germany"),
           uiOutput("corona_vars_xgb"),
           actionButton("reset_corona_xgb", "clear selected")
           
           
           
  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type_xgb", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs_xgb()
           
  )
  
)

}


#' @export
#' @rdname uiElements
numeric_features <- function(){
tabsetPanel(
  id = "tabs_for_xgb",
  type = "hidden",
  tabPanel("1",
           #selectInput("var_1", "Chose variable to add AR and/or MA features", choices = ""),
           uiOutput("add_features"),
           # corona variablen auch in add_features
           numericInput("num_1","Chose length of moving average",min=0,value = 2),
           numericInput("num_2","Chose Autoregressive lags for",min=0,value = 1),
           actionButton("addButton", "Upload"),
           uiOutput("finish_button"),
           actionButton("reset_cus", "Reset")
           
           
  )
  # tabPanel("2",
  #          selectInput("var_2", "Select varaible", choices = ""), #could I use var_1 here?
  #          numericInput("num_3","Chose length of moving average",min=1,value = 1),
  #          numericInput("num_4","Chose Autoregressive lags for",min=1,value = 1),
  #          selectInput("var_3", "Select varaible", choices = ""),
  #          numericInput("num_5","Chose length of moving average",min=1,value = 1),
  #          numericInput("num_6","Chose Autoregressive lags for",min=1,value = 1)
  # )
  
)

}
model_specification <- function(){
tabsetPanel(
  id = "mod_spec",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           numericInput("mtry","number of predictors that will be randomly sampled",min = 2,max=30,step = 1,value = 20),
           numericInput("trees","number of trees contained in the ensemble",min = 50,max=1000,step = 10,value = 200),
           numericInput("min_n","minimum number of data points in a node",min = 1,max=20,step = 1,value = 3),
           numericInput("tree_depth","maximum depth of the tree",min = 1,max=50,step = 1,value = 8),
           numericInput("learn_rate","rate at which the boosting algorithm adapts",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("loss_reduction","reduction in the loss function required to split further",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("sample_size","amount of data exposed to the fitting routine",min = 0.1,max=1,step = 0.1,value = 0.7)
           
  ),
  tabPanel("hyperparameter_tuning",
           numericInput("trees_hyp","number of predictors that will be randomly sampled",min = 50,max=1000,step = 10,value = 200),
           numericInput("grid_size","size of tuning grid",min = 10,max=100,step = 5,value = 30)
           
           
  )
  
)

}

model_specification_for <- function(){
  
tabsetPanel(
  id = "mod_spec_for",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           numericInput("mtry1","number of predictors that will be randomly sampled",min = 2,max=30,step = 1,value = 20),
           numericInput("trees1","number of trees contained in the ensemble",min = 50,max=1000,step = 10,value = 200),
           numericInput("min_n1","minimum number of data points in a node",min = 1,max=20,step = 1,value = 3),
           numericInput("tree_depth1","maximum depth of the tree",min = 1,max=50,step = 1,value = 8),
           numericInput("learn_rate1","rate at which the boosting algorithm adapts",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("loss_reduction1","reduction in the loss function required to split further",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("sample_size1","amount of data exposed to the fitting routine",min = 0.1,max=1,step = 0.1,value = 0.7)
           
  ),
  tabPanel("hyperparameter_tuning",
           numericInput("trees_hyp1","number of predictors that will be randomly sampled",min = 50,max=1000,step = 10,value = 200),
           numericInput("grid_size1","size of tuning grid",min = 10,max=100,step = 5,value = 30)
           
           
  )
  
)
}

custom_lag_tab <- function(){
tabsetPanel(
  id = "lag_tab",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           selectInput("correlation_type", "Chose type of correlation plot:", choices = c("ACF","PACF"), selected = ""),
           uiOutput("correlation_plot_choice"),
           numeric_features()
           # actionButton("reset_arma", "clear selected")
           
  )
  
)

}







