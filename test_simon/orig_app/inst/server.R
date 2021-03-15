###############################################not orig
server <- function(input, output, session) {

  ############################################################# Stocks
  # load stock dataset
  stockdata_DE <- reactive({
    req(path_setter()[[3]][1] == "correct_path")

    load_all_stocks_DE()
  })

  stockdata_US <- reactive({
   req(path_setter()[[3]][1] == "correct_path")

    load_all_stocks_US()
  })


  output$stock_choice <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_stocks == "Germany"){
      input <- selectizeInput("Stock","Choose Companies:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = TRUE)
    } else {
      input <- selectizeInput("Stock","Choose Companies:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = TRUE)
    }
  })



  # reset button for stock selection
  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })
  # plot of the stocks
  output$plot_DE <- renderPlot({
    req(input$Stock)
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_stocks == "Germany"){
      plotdata <- filter(stockdata_DE(),
                         .data$name %in% (c(COMPONENTS_DE()[["Symbol"]],"GDAXI")[c(COMPONENTS_DE()[["Company.Name"]],"GDAXI") %in% .env$input$Stock]) &
                           .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    } else {
      plotdata <- filter(stockdata_US(),
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock]) &
                           .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    }

    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    ggplot(plotdata,aes_string("Dates",input$stock_outcome,color = "name"))+
      geom_line()+
      theme_classic()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  # hover info box
  output$hover_info_DE <- renderUI({
    req(input$hovering)
    create_hover_info_DE(input$plot_hover_DE,stockdata_DE())
  })
  # zoom functionality
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  #####################################################################

  ##################################################################### Corona

  corona_data <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    CORONA_xgb(input$CoronaCountry,input$dates_corona[1],input$dates_corona[2])
  })

  output$corona_plot <- renderPlot({
    if (!is.null(ranges2$x)) {
      ranges2$x <- as.Date(ranges2$x, origin = "1970-01-01")
    }

    ggplot(corona_data(), aes_string("date",input$corona_measurement,color = "location"))+
      geom_line() +
      theme_classic() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })

  # hover info box
  output$hover_info_corona <- renderUI({
    req(input$hovering_corona)
    create_hover_info_corona(input$plot_hover_corona, corona_data(),input$corona_measurement)
  })

  # zoom functionality
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_corona_dblclick, {
    brush <- input$plot_corona_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

  ##################################################################################### Granger




  output$Stock_Granger <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_granger == "Germany"){
      input <- selectizeInput("Stock_Granger","Choose dependent variable:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Granger","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
  })


  output$ControlsGranger <- renderUI({
    if (input$country_regression == "Germany"){
      input <- selectizeInput("Controls_GRANGER","Choose control variables:",
                              c(colnames(global_controls_test_DE())[-1],"DAX"),selected = "VIX",multiple = FALSE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls_GRANGER","Choose control variables:",
                              c(colnames(global_controls_test_US())[-1],"DOW"),selected = "VIX",multiple = FALSE)
    }
  })


  granger_data <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    req(input$Stock_Granger)
    if (input$country_granger == "Germany"){
      granger1 <- filter(stockdata_DE(),
                         .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Granger]) &
                           .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]
    } else {
      granger1 <- filter(stockdata_US(),
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Granger]) &
                           .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]

    }

    if (input$country_granger == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- GDAXI()  #load dax
      dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- left_join(dax,global_controls,by = c("Date")) #join final

    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dow <- DOW()
      dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      dow <- missing_date_imputer(dow,"Close.")
      colnames(dow)[2] <- "DOW"
      global_controls <- left_join(dow,global_controls,by = c("Date"))
    }
    names(global_controls)[1] <- "Dates"
    granger <- left_join(granger1,global_controls,by = c("Dates"))
    granger <- granger[c("Dates",input$Granger_outcome,input$Controls_GRANGER)]
    granger
  })

  optlags <- reactive({
    #library(vars)
    req(is.null(granger_data())==FALSE)
    VARselect(granger_data()[-1],lag.max = 7, type = "const")$selection[["AIC(n)"]]
  })

  dickey_fuller <- reactive({
    data <- granger_data()
    while (adf.test(data[[2]],k=optlags())$p.value > 0.1 | adf.test(data[[3]],k=optlags())$p.value > 0.1){
      data[2] <- c(diff(data[[2]],1),NA)
      data[3] <- c(diff(data[[3]],1),NA)
      data <- drop_na(data)
    }
    data
  })

  granger_result <- reactive({
    varobject <- VAR(dickey_fuller()[-1], p = optlags(), type = "const")
    cause <- NULL
    ifelse(input$direction_granger == TRUE,cause <- input$Controls_GRANGER,cause <- input$Granger_outcome)
    granger <- causality(varobject, cause = cause)
    granger$Granger
  })

  output$granger_result <- renderPrint({
    granger_result()})

  # output$stocks_granger <- renderPlot({
  #   req(input$Granger_outcome)
  #   ggplot(granger_data(),aes_string("Dates",input$Granger_outcome))+
  #     geom_line()
  # })

  output$stocks_granger <- dygraphs::renderDygraph({
    plotdata <- xts(granger_data()[input$Granger_outcome],order.by=granger_data()[["Dates"]])
    dygraphs::dygraph(plotdata)
  })


  output$second_granger <- dygraphs::renderDygraph({
    plotdata <- xts(granger_data()[input$Controls_GRANGER],order.by=granger_data()[["Dates"]])
    dygraphs::dygraph(plotdata)
  })

  output$grangertext1 <- renderUI({
    str1 <- paste("The optimal lag order for the VAR model using the Akaike information criterium (AIC)  is ",optlags()," lags.")
    HTML(paste(str1))
  })

  output$optimallags <- renderPrint({
    VARselect(granger_data()[-1],lag.max = 7, type = "const")
  })

  output$grangertext2 <- renderUI({
    if (nrow(dickey_fuller()) != nrow(granger_data())){
      str2 <- paste("The Dickey Fuller test found one of the timeseries to be non-stationary:")
    }else{
      str2 <-paste("The Dickey Fuller test found both timeseries to be stationary.
                   Hence, the granger causality analysis can be performed without tranformations:")
    }
  })


  #first variable
  output$dickey_fuller <- renderPrint({
    adf.test(granger_data()[[2]],k=optlags())
  })
  #second variable
  output$dickey_fuller_second <- renderPrint({
    adf.test(granger_data()[[3]],k=optlags())
  })

  output$grangertext3 <- renderUI({
    req(nrow(dickey_fuller()) != nrow(granger_data()))
      str3 <- paste("Differencing the series ",nrow(granger_data()) - nrow(dickey_fuller()),"times achieved stationarity:")
  })


  #first variable after differencing
  output$dickey_fuller_diff <- renderPrint({
    req(nrow(dickey_fuller()) != nrow(granger_data()))
    adf.test(dickey_fuller()[[2]],k=optlags())
  })
  #second variable after differencing
  output$dickey_fuller_second_diff <- renderPrint({
    req(nrow(dickey_fuller()) != nrow(granger_data()))
    adf.test(dickey_fuller()[[3]],k=optlags())
  })



  # output$dickey <- renderUI({
  #   str1 <- paste("The optimal lag order for the VAR model using the Akaike information criterium (AIC)  is ",optlags()," lags")
  #   if (nrow(dickey_fuller()) != nrow(granger_data())){
  #     str2 <- paste("The Dickey Fuller test found one of the timeseries to be non-stationary.")
  #     str3 <- paste("Differencing the series ",nrow(granger_data()) - nrow(dickey_fuller()),"times achieved stationarity")
  #   } else {
  #     str2 <-paste("The Dickey Fuller test found both timeseries to be stationary.")
  #     str3 <-paste("Hence, the granger causality analysis can be performed without tranformations")
  #   }
  #   HTML(paste(str1,str2,str3, sep = '<br/>'))
  # })

  output$granger_satz <- renderUI({
    if(input$direction_granger == TRUE){
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste(input$Controls_GRANGER, " granger causes ",input$Granger_outcome,"of",input$Stock_Granger)
      } else {
        str1 <- paste(input$Controls_GRANGER, " does not granger cause ",input$Granger_outcome,"of",input$Stock_Granger)
      }
    } else {
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste(input$Granger_outcome,"of",input$Stock_Granger, "granger causes ",input$Controls_GRANGER)
      } else {
        str1 <- paste(input$Granger_outcome,"of",input$Stock_Granger, "does not granger cause ",input$Controls_GRANGER)
      }
    }
    HTML(paste(str1))
  })

  output$info_granger <- renderUI({
    str1 <- paste("In this section, the user is able to perform a Granger causality test, which is a statistical hypothesis test for determining whether one time series is useful in forecasting another.
                  The term 'causality' in this context means nothing more than predictive causality and should not be mistaken for
                  'true causality'. It rather measures the ability of past values of one time series to predict future values of another time series.
                  ","<br/>")
    str2 <- paste("The following steps are automatically performed after the user selects two time series : ","<br/>",
                  "1. The optimal number of lags is calculated","<br/>",
                  "2. Stationarity is repeatedly tested and the series are differenced until sationarity is achieved","<br/>",
                  "3. A VAR model is estimated with the optimal number of lags and the (if necessary) transformed series","<br/>",
                  "4. A granger causality test is performed.")
    HTML(paste(str1,str2,sep = '<br/>'))
  })

  ################################################################################################### Regression

  ###flexible input for stocks: show either german or us companies
  output$stock_regression <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression == "Germany"){
      input <- selectizeInput("Stock_Regression","Choose dependent variable:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
  })

  # dataset_rec <- reactive({
  #   res <- dataset()
  # })

  output$Controls <- renderUI({
    #res <- dataset()
    #res$name <- NULL
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression == "Germany"){
      input <- selectizeInput("Controls","Choose control variables:",
                              c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls","Choose control variables:",
                              c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
    }

  })

  dataset <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression == "Germany"){
      data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression]) &
                           .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression]) &
                           .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen
    }

    if (input$country_regression == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- GDAXI()  #load dax
      dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- left_join(dax,global_controls,by = c("Date")) #join final

    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dow <- DOW()
      dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      dow <- missing_date_imputer(dow,"Close.")
      colnames(dow)[2] <- "DOW"
      global_controls <- left_join(dow,global_controls,by = c("Date"))
    }
    names(global_controls)[1] <- "Dates"
    data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
    ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
    data_reg2
  })


  df_selected_controls <- reactive({
    #req(input$Controls)
    res <- dataset()
    res <- res[c("Dates",input$regression_outcome,input$Controls)]
    res
  })

  observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
  })

  observeEvent(input$industry_sentiment, {                         #Observe event from input (model choices)
    req(input$industry_sentiment)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment)
  })

  dataset_senti <- reactive({
    req(input$Sentiment_type)
    req(path_setter()[[3]][1] == "correct_path")
    if(input$Sentiment_type == "NoFilter"){

      res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
      #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
      #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
      #input$language
    }else{
      req(input$Stock_reg)
      ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
      res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()

    }


  })
  # filter
  filtered_df <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    req(input$Sentiment_type)
    req(input$minRetweet_stocks1)
    req(input$minRetweet_stocks2)

    if(input$Sentiment_type == "NoFilter"){

      res <- dataset_senti()
    }else{ # live filtering
      req(input$industry_sentiment)
      res <- dataset_senti()
      if(input$industry_sentiment == "no"){
        res <- dataset_senti()
        if(input$tweet_length_stock1 == "yes"){

          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)) &
                                  (tweet_length > 81))}
        else{
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)))
        }
      }#else{
      #res <- dataset_senti()
      #if(input$tweet_length_stock2 == "yes"){
      # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
      #                          (tweet_length > 81))
      #}else{
      #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
      #}
      #}
    }
  })

  # aggregate dataset to get one sentiment per day
  aggri_select <- reactive({

    if(input$Sentiment_type == "NoFilter"){ # NoFilter files already aggregated
      res <- filtered_df()
      aggregation <- key(input$aggregation)  # select aggregation type: Mean, mean weighted by,...
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      if(input$industry_sentiment == "no"){
        res <- filtered_df()
        res <- aggregate_sentiment(res) # function to aggregate sentiment per day
        res <- res %>% filter(language == input$language1)
        aggregation <- key(input$aggregation1)
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }else{
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry,input$minRetweet_stocks2,
                                      input$tweet_length_stock2)      #function to gather all stock in certain industry
        aggregation <- key(input$aggregation2)                          #--> also calculates aggregation inside function
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }
    }

  })

  observeEvent(input$reset_regression,{
    updateSelectizeInput(session,"Controls",selected = "")
  })



  #merge sentiment with control+dep vars
  final_regression_df <- reactive ({
    if (input$senti_yesno_reg == TRUE){
      res <- aggri_select()
    } else {
      res <- aggri_select()[1]
    }
    res$date <- as.Date(res$date)
    res_c <- df_selected_controls()
    res <- left_join(res_c,res, by=c("Dates" = "date"))
    res <- res[-1]
    res
  })



  #regression
  regression_result <- reactive({
    req(ncol(final_regression_df())>=2)
    model <- lm(reformulate(".",input$regression_outcome), data = final_regression_df())
    #summary(model)
    coeftest(model, vcov = vcovHC(model, "HC1"))
  })

  #Qregression
  regression_result_Qreg <- reactive({
    req(ncol(final_regression_df())>=2)
    model <- rq(reformulate(".",input$regression_outcome),tau = input$Quantiles,data = final_regression_df())
    summary(model)
  })


  # output$testi_table <- renderPrint ({
  #   head(dataset())
  # })

  # output$senti <- renderPrint ({
  #   head(df_selected_controls())
  # })

  # output$senti_agg <- renderPrint ({
  #   head(final_regression_df())
  # })

  output$regression_result <- renderPrint({
    regression_result()})

  output$regression_equation <- renderUI({
    str1 <- paste("Linear regression: ",input$regression_outcome,"of ",input$Stock_Regression,"~",paste(input$Controls,collapse = " + "),"<br/>")
    HTML(paste(str1,sep = '<br/>'))
  })


  # output$plot_dens_Qreg <- renderPlot({
  #
  #   density_plot_reg(dataset())
  # })

  output$regression_result_Qreg <- renderPrint({
    regression_result_Qreg()})

  ###############################################################################
  ########################   VAR    #############################################
  ###############################################################################

  ###################################################### dataset ###############################################################
  ###flexible input for stocks: show either german or us companies
  output$stock_regression_var <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression_var == "Germany"){
      input <- selectizeInput("Stock_Regression_var","Choose dependent variable:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression_var","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
  })


  output$Controls_var <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression_var == "Germany"){
      input <- selectizeInput("Controls_var","Choose control variables:",
                              c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls_var","Choose control variables:",
                              c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
    }

  })

  dataset_var <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression_var == "Germany"){
      data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_var]) &
                           .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression_var]) &
                           .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
    }

    if (input$country_regression_var == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- GDAXI()  #load dax
      dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- left_join(dax,global_controls,by = c("Date")) #join final

    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dow <- DOW()
      dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      dow <- missing_date_imputer(dow,"Close.")
      colnames(dow)[2] <- "DOW"
      global_controls <- left_join(dow,global_controls,by = c("Date"))
    }
    names(global_controls)[1] <- "Dates"
    data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
    ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
    data_reg2
  })


  df_selected_controls_var <- reactive({
    #req(input$Controls_var)
    res <- dataset_var()
    res <- res[c("Dates",input$regression_outcome_var,input$Controls_var)]
    res
  })

  observeEvent(input$Sentiment_type_var, {                         #Observe event from input (model choices)
    req(input$Sentiment_type_var)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type_var)
  })

  observeEvent(input$industry_sentiment_var, {                         #Observe event from input (model choices)
    req(input$industry_sentiment_var)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_var)
  })

  dataset_senti_var <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    req(input$Sentiment_type_var)
    if(input$Sentiment_type_var == "NoFilter"){

      res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
      #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
      #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
      #input$language
    }else{
      req(input$Stock_reg)
      ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
      res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()

    }


  })
  # filter
  filtered_df_var <- reactive({
    req(path_setter()[[3]][1] == "correct_path")
    req(input$Sentiment_type_var)
    req(input$minRetweet_stocks1_var)
    req(input$minRetweet_stocks2_var)

    if(input$Sentiment_type_var == "NoFilter"){

      res <- dataset_senti_var()
    }else{ # live filtering
      req(input$industry_sentiment_var)
      res <- dataset_senti_var()
      if(input$industry_sentiment_var == "no"){
        res <- dataset_senti_var()
        if(input$tweet_length_stock1_var == "yes"){

          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_var)) &
                                  (tweet_length > 81))}
        else{
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_var)))
        }
      }#else{
      #res <- dataset_senti()
      #if(input$tweet_length_stock2 == "yes"){
      # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
      #                          (tweet_length > 81))
      #}else{
      #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
      #}
      #}
    }
  })

  # aggregate dataset to get one sentiment per day
  aggri_select_var <- reactive({

    if(input$Sentiment_type_var == "NoFilter"){ # NoFilter files already aggregated
      res <- filtered_df_var()
      aggregation <- key(input$aggregation_var)  # select aggregation type: Mean, mean weighted by,...
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      if(input$industry_sentiment_var == "no"){
        res <- filtered_df_var()
        res <- aggregate_sentiment(res) # function to aggregate sentiment per day
        res <- res %>% filter(language == input$language1_var)
        aggregation <- key(input$aggregation1_var)
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }else{
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry_var,input$minRetweet_stocks2_var,
                                      input$tweet_length_stock2_var)      #function to gather all stock in certain industry
        aggregation <- key(input$aggregation2_var)                          #--> also calculates aggregation inside function
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }
    }

  })

  observeEvent(input$reset_regression_var,{
    updateSelectizeInput(session,"Controls_var",selected = "")
  })

  #merge sentiment with control+dep vars
  final_regression_df_var <- reactive ({
    if (input$senti_yesno == TRUE){
      res <- aggri_select_var()
    } else {
      res <- aggri_select_var()[1]
    }
    res$date <- as.Date(res$date)
    res_c <- df_selected_controls_var()
    res <- left_join(res_c,res, by=c("Dates" = "date"))
    #res <- res[-1]
    res
  })

  ####################################################Summary statistics #####################################################

  df_need <- reactive({
    df_need <- round(describe(final_regression_df_var()[-1])[c(3, 4, 5, 8, 9)], 2)
    test <- nrow(df_need)
    test2 <- nrow(df_need)==1
    if (nrow(df_need == 1)) {
      row.names(df_need)[1] <- input$regression_outcome_var
    } else{
      df_need <- df_need
    }
    df_need

  })


  output$var_summary <- function(){
    #colnames(df_need)<- "value"
    knitr::kable(df_need(), caption = glue("Summary statistics"),colnames = NULL) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }

  output$correlation_var <- renderPlot({
    ggpairs(final_regression_df_var()[-1])
  })


  ##################################################   Validity    ######################################################
  output$datensatz_var <- renderPrint ({
    head(final_regression_df_var())
  })

  forecast_data <- reactive({
    final_regression_df_var()[1:(nrow(final_regression_df_var())-input$ahead),-1,drop=FALSE]
  })

  actual_values <- reactive({
    final_regression_df_var()[((nrow(final_regression_df_var())+1)-input$ahead):nrow(final_regression_df_var()),2]
  })



  stationary <- reactive({
    data <- forecast_data()
    if (adf.test(data[[1]],k=2)$p.value > 0.1){
      for (i in 1:ncol(data)){
        data[i] <- c(diff(data[[i]],1),NA)
      }
      data <- drop_na(data)
    }else{}
    data
  })


  #optimal lags
  optlags_var <- reactive({
    VARselect(stationary(),lag.max = 10, type = "none")$selection[["SC(n)"]]
  })

  #fit model
  var_model <- reactive({
    if (ncol(forecast_data()) == 1) {
      model <- arima(stationary(), order = c(optlags_var(), 0, 0))
    } else {
      model <- VAR(stationary(), p = optlags_var(), type = "none")
    }
    model
  })

  #test for autocorrelation: rejection = bad (means presence of correlated errors)
  serial_test <- reactive({
    if (ncol(forecast_data()) == 1) {
      test <- Box.test(var_model()$residuals,type= "Box-Pierce" )
    } else {
      test <- serial.test(var_model(), type="BG",lags.bg = optlags_var())
    }
    test
  })

  #forecast
  forecast_var <- reactive({
    fcast <- predict(var_model(), n.ahead = input$ahead)
    if (ncol(forecast_data()) == 1) {
      x <- fcast$pred[1:input$ahead]
      x <- cumsum(x) + forecast_data()[nrow(forecast_data()),1]
    }else {
      x <- fcast$fcst[[1]]
      x <- x[,1]
      x <- cumsum(x) + forecast_data()[nrow(forecast_data()),1]
    }
    x
  })

  #plot the actual vs. the predicted forecast
  output$plot_forecast <- dygraphs::renderDygraph({
    if (input$var_which_plot == "Forecasted period only"){
    plot <- data.frame(final_regression_df_var()$Dates[(nrow(forecast_data())+1):(nrow(forecast_data())+input$ahead)],#Dates
                        forecast_var(),                                                              #forecasted values
                        actual_values())#actual values
    colnames(plot) <- c("a","forecast","actual")
    # ggplot(plot1) +
    #   geom_line(aes(a,b),color="red")+
    #   geom_line(aes(a,c),color="gold")+
    #   labs(x="Date",y="StockPrice",title = "forecasted vs. actual")
    plot <- xts(plot[c("forecast","actual")],order.by=plot[["a"]])
    dygraphs::dygraph(plot)
  }else{
    plot <- data.frame(final_regression_df_var()$Dates,
                        c(forecast_data()[[1]],forecast_var()),
                        final_regression_df_var()[2])
    colnames(plot) <- c("a","forecast","actual")
    # ggplot(plot2) +
    #   geom_line(aes(a,b))+
    #   geom_line(aes(a,c))+
    #   labs(x="Date",y="StockPrice",title = "forecasted vs. actual, full series")
    plot <- xts(plot[c("forecast","actual")],order.by=plot[["a"]])

    dygraphs::dygraph(plot) %>%
      dyEvent(final_regression_df_var()$Dates[(nrow(forecast_data())+1)], "Start of prediction", labelLoc = "bottom")

  }

  })

  # output$plot_forecast2 <- dygraphs::renderDygraph({
  #
  #   plot2 <- data.frame(final_regression_df_var()$Dates,
  #                       c(forecast_data()[[1]],forecast_var()),
  #                       final_regression_df_var()[2])
  #   colnames(plot2) <- c("a","forecast","actual")
  #   # ggplot(plot2) +
  #   #   geom_line(aes(a,b))+
  #   #   geom_line(aes(a,c))+
  #   #   labs(x="Date",y="StockPrice",title = "forecasted vs. actual, full series")
  #   plot2 <- xts(plot2[c("forecast","actual")],order.by=plot2[["a"]])
  #   dygraphs::dygraph(plot2)
  #
  #
  # })

#   output$accuracy_var <- renderUI({
#     str1 <- paste("The RMSE is: ",sqrt(mean((forecast_var()-actual_values())^2)))
#     str2 <- paste("The MAE is: ",mean(abs(forecast_var()-actual_values())))
#     str3 <- paste("The MAPE is:",mean(abs((actual_values()-forecast_var())/actual_values()) * 100)
# )
#     HTML(paste(str1,str2,str3, sep = '<br/>'))
#
#   })

  output$var_metrics <- function(){

  df_need <- data.frame(c(sqrt(mean((forecast_var()-actual_values())^2)),
                          mean(abs(forecast_var()-actual_values())),
                          mean(abs((actual_values()-forecast_var())/actual_values()) * 100)),
                        row.names = c("RMSE","MAE","MAPE"))
  colnames(df_need)<- "value"
  knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
           kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                     position = "center",
                                     font_size = 16)
  }


  output$serial_test <- renderPrint({
    serial_test()
  })

  output$var <- renderUI({
    if (ncol(forecast_data()) == 1) {
      str1 <- paste("Box-Pierce test statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test()$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else{
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    } else {
      str1 <- paste("Breusch-Godfrey LM-statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test()$serial$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else {
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    }
    HTML(paste(str1,str2, sep = '<br/>'))
  })



  ##################################################   actual forecast    ######################################################
  forecast_data_real <- reactive({
    final_regression_df_var()[,-1,drop=FALSE]
  })



  stationary_real <- reactive({
    data <- forecast_data_real()
    if (adf.test(data[[1]],k=2)$p.value > 0.1){
      for (i in 1:ncol(data)){
        data[i] <- c(diff(data[[i]],1),NA)
      }
      data <- drop_na(data)
    }else{}
    data
  })


  #optimal lags
  optlags_var_real <- reactive({
    VARselect(stationary_real(),lag.max = 10, type = "none")$selection[["SC(n)"]]
  })

  #fit model
  var_model_real <- reactive({
    if (ncol(forecast_data_real()) == 1) {
      model <- arima(stationary_real(), order = c(optlags_var_real(), 0, 0))
    } else {
      model <- VAR(stationary_real(), p = optlags_var_real(), type = "none")
    }
    model
  })

  #test for autocorrelation: rejection = bad (means presence of correlated errors)
  # serial_test <- reactive({
  #   if (ncol(forecast_data()) == 1) {
  #     test <- Box.test(var_model()$residuals,type= "Box-Pierce" )
  #   } else {
  #     test <- serial.test(var_model(), type="BG",lags.bg = optlags_var())
  #   }
  #   test
  # })

  #forecast
  forecast_var_real <- reactive({
    fcast <- predict(var_model_real(), n.ahead = input$ahead)
    if (ncol(forecast_data_real()) == 1) {
      x <- fcast$pred[1:input$ahead]
      x <- cumsum(x) + forecast_data_real()[nrow(forecast_data_real()),1]
    }else {
      x <- fcast$fcst[[1]]
      x <- x[,1]
      x <- cumsum(x) + forecast_data_real()[nrow(forecast_data_real()),1]
    }
    x
  })

  output$plot_forecast_real <- dygraphs::renderDygraph({

    plot <- data.frame(c(final_regression_df_var()[["Dates"]],seq(as.Date(tail(final_regression_df_var()$Dates,1))+1,by = "day",length.out = input$ahead)),
                        c(forecast_data_real()[[1]],forecast_var_real()))
    colnames(plot) <- c("a","b")
    # ggplot(plot2) +
    #   geom_line(aes(a,b))+
    #   labs(x="Date",y="StockPrice",title = "forecasted series")
    plot <- xts(plot["b"],order.by=plot[["a"]])

    dygraphs::dygraph(plot) %>%
      dyEvent(max(final_regression_df_var()$Dates), "Start of prediction", labelLoc = "bottom")

  })

  # output$testins <- renderPrint({
  #   c(final_regression_df_var()[["Dates"]],seq(as.Date(tail(final_regression_df_var()$Dates,1)),by = "day",length.out = input$ahead))
  #   #seq(as.Date(tail(final_regression_df_var()[["Dates"]],1)),by = "day",length.out = input$ahead)
  # })

#################################################################################################### twitter

  ############################################################################
  ################# Directory ###############################################
  ###########################################################################
  # selecting directory
  # find home direcoty of user
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  # allow for searching directories
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  observe({

    cat("\ninput$directory value:\n\n")
    print(input$directory)
  })
  path_setter <- reactive({
    #browser()
    if (is.integer(input$directory)) {
      setwd(volumes)

      cat(glue("No directory has been selected. Current directory {getwd()})"))

    } else {

      path <- shinyFiles::parseDirPath(volumes, input$directory)
      setwd(path)
      con <- DBI::dbConnect(RSQLite::SQLite(), "SQLiteStudio/databases/clean_database.db")

      file_needed <- "SQLiteStudio"
      if(dir.exists(file_needed)) {
        #setwd(file_path)
        output_str <- glue("Current path {getwd()}")
      } else {
        #setwd(file_path)
        output_str <- "Current path selection does not seem correct. \n
                Are you sure it is set correctly?"
      }

      path_outputs <- list(a = con, b = output_str, c = "correct_path")
      path_outputs


    }
  })
  output$directorypath <- renderText({
    path_outputs <- path_setter()
    path_outputs[[2]][1]
  })


  ###############################################################################
  ##################### twitter logo directory page ############################
  ###############################################################################

  output$twitter_logo <- renderImage({

    req(path_setter()[[3]][1] == "correct_path")

    filename <- "shiny/images/twitter_image.png"



    list(src = filename,
         alt = "This is the Twitter Logo",
         contentType = "Images/png",
         height = 400, width = 400)
  }, deleteFile = F)


  ###############################################################################
  ############################### twitter descriptive ###########################
  ###############################################################################
  ### for histogram less choices



  ######## disconnect from database after exit
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    req(path_setter())
    con <- path_setter[[1]][1]
    DBI::dbDisconnect(con)
  })

 long <- reactive({
   if (input$long == T){
     long <- 81
   } else{
     long <- 0
   }
   long
 })





 ################################### path finder for histo files
  querry_histo <- reactive({
    if (input$long == T){
      long_name <- "long_only"
    } else{
      long_name <- "all"
    }

    lang <- lang_converter()


    ### account for case where sentiment is selected

    # replace sentiment with senti because refernced with senti in file
    value_var <- stringr::str_replace(input$value[1],"sentiment", "senti")
    # replace tweet_length with long becuase refernced with long in file
    value_var <- stringr::str_replace(value_var, "tweet_length", "long")







    # for no filter
    if (is.null(input$comp)){
    glue("histo_{value_var}_{lang}_NoFilter_rt_{input$rt}_li_{input$likes}_lo_{long_name}.csv")
    } else { #for chosen company
      req(!is.null(input$comp))





      glue("histo_{value_var}_{input$comp}_rt_{input$rt}_li_{input$likes}_lo_{long_name}.csv")

    }
     # old sql
      #browser()
      # if (input$value == "length"){
      #   tb_metric <- "len"
      #   col_value <- input$value
      #
      # } else if(input$value == "rt") {
      #
      #   tb_metric <- input$value
      #   col_val <- "retweets_count"
      #
      # } else if(input$value == "likes") {
      #   tb_metric <- input$value
      #   col_val <- "likes_count"
      # } else if(input$value == "sentiment"){
      #   tb_metric <- input$value
      #   col_val <- "sentiment_rd"
      # } else{
      #   Sys.sleep(0.2)
      # }
      #
      #
      #
      #
      #
      # table_name <- glue("histo_{tb_metric}_{tolower(input$lang)}")
      #
      # if (table_name %in% c("histo_rt_en", "histo_likes_en", "histo_len_en")){
      #   date_col <- "date"
      # } else{
      #   date_col <- "created_at"
      # }
      #
      # glue("SELECT {col_val}, sum(N) as n  FROM {table_name}  WHERE {date_col} >=  '{input$dates[1]}'
      # and {date_col} <= '{input$dates[2]}'
      # and retweets_count_filter = {input$rt} and likes_count_filter = {input$likes} and
      # tweet_length_filter = {long}
      #      group by {col_val}")


     #if closed
    }) #reactive closed




  ##################### summary statistics table data and time series data
    querry_sum_stats_table <- reactive({

long <- long()
#browser()
      if (is.null(input$comp)){
      table_name <- glue("sum_stats_{tolower(input$lang)}")

      glue("SELECT *  FROM {table_name}  WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}" )
      } else { #if company is chosen
        glue("SELECT *  FROM sum_stats_companies WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long} and company  = '{input$comp}' and
             language = '{tolower(input$lang)}'" )
      }


    })
    #browser()





  # ## get data from querry
  # data_time_series <- reactive({
  #
  #
  #
  #   con <- path_setter()
  #
  #
  #   con <- con[[1]]
  #
  #   string_value <- is.null(con)
  #   req(!string_value)
  #   df_need <- DBI::dbGetQuery(con, querry_time_series())
  #
  #  df_need
  # })
############################### data retriever for histogram
  data_histo <- reactive({

    lang <- lang_converter()
    a <- path_setter()



    # for case no company selected
    if (is.null(input$comp)){
  file_path <- file.path(glue("Twitter/plot_data/{lang}_NoFilter/{querry_histo()}"))
  exists <- file.exists(file_path)
  shinyFeedback::feedbackDanger("histo_plot", !exists, "Please make sure you picked the correct path. The \n
                                file cannot be found in the current directory")
  req(exists)
    df_need <- data.table::fread(file_path,
                                 select = 1:3)

    df_need
    } else { #for case of choosen company
      file_path <- file.path(glue("Twitter/plot_data/Companies/{input$comp}/{querry_histo()}"))
      df_need <- data.table::fread(file_path,
                                   select = 1:3)
      df_need

    }
  })



    ######################### time series plot for retweets etc.

  r <- reactiveValues(
    change_datewindow = 0,
    change_dates = 0,
    change_datewindow_auto = 0,
    change_dates_auto = 0,
    dates = c( as.Date("2018-11-30"), as.Date("2021-02-19"))
  )


  observeEvent(input$sum_stats_plot_date_window, {
    message(crayon::blue("observeEvent_input_sum_stats_plot_date_window"))
    r$change_datewindow <- r$change_datewindow + 1
    if (r$change_datewindow > r$change_datewindow_auto) {

      r$change_dates_auto <- r$change_dates_auto + 1
      r$change_datewindow_auto <- r$change_datewindow

      start <- as.Date(ymd_hms(input$sum_stats_plot_date_window[[1]])+ days(1))
      stop  <- as.Date(ymd_hms(input$sum_stats_plot_date_window[[2]])+ days(1))
      updateAirDateInput(session = session,
                         inputId = "dates_desc",
                         value = c(start, stop),
      )
    } else {
      if (r$change_datewindow >= 10) {
        r$change_datewindow_auto <- r$change_datewindow <- 0
      }
    }
  })

  observeEvent(input$dates, {
    message("observeEvent_input_dates")
    r$change_dates <- r$change_dates + 1
    if (r$change_dates > r$change_dates_auto) {
      message("event input_year update")

      r$change_datewindow_auto <- r$change_datewindow_auto
      r$change_dates_auto <- r$change_dates

      r$dates <- input$dates

    }
  })




  output$sum_stats_plot <- dygraphs::renderDygraph({
    message("renderDygraph")
    req(!is.null(input$value) | input$num_tweets_box == T)

    df <- get_data_sum_stats_tables()

    if (input$num_tweets_box == F){
      time_series_plotter2(df, input$metric, input$value, num_tweets = F, input$dates_desc[1], input$dates_desc[2], r)
    } else {
      time_series_plotter2(df, input$metric, input$value, num_tweets = F, input$dates_desc[1], input$dates_desc[2], r)
    }
    # dygraphs::dygraph(don) %>%
    #   dygraphs::dyRangeSelector( input$dates_desc + 1, retainDateWindow = T
    #   )
  })




####### block metric selection if chosen number of tweets
  # observeEvent(input$metric,{
  #  # browser()
  #   if (input$metric == "N"){
  #     shinyjs::disable("value")
  #   } else {
  #     shinyjs::enable("value")
  #   }
  # })




  ##################### disable log scale option for sentiment because as negative values
  observeEvent(input$value, {
    #browser()
    if (grepl("sentiment",input$value[1])) {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = T,
                                      value = F)
    } else {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = F)
    }
  })


  ######################################## histogram output
  output$histo_plot <- renderPlot({
   #  df <- data_histo()
   #
   # df %>%
   #      group_by(.[[2]]) %>% summarise(N = sum(N)) %>%
   #     mutate(metric = case_when(input$log_scale == T ~ log(as.numeric(.[[1]])+ 0.0001),
   #                                input$log_scale == F ~ as.numeric(.[[1]])),
   #             bins = cut_interval(metric, n = input$bins)) %>%
   #
   #      ggplot(aes(bins, N)) +
   #      geom_col() +
   #      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   #
   req(input$value)
   # df <- data_histo()
   #
   #   df %>%   group_by(.[[2]]) %>% summarise(N = sum(N)) %>%
   #     mutate(metric = case_when(input$log_scale == T ~ log(as.numeric(.[[1]])+ 0.0001),
   #                                input$log_scale == F ~ as.numeric(.[[1]])),
   #             bins = cut_interval(metric, n = input$bins)) %>%
   #
   #      ggplot(aes(bins, N)) +
   #      geom_col() +
   #      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  histogram_plotter(data_histo(), date_input1 = input$dates_desc[1], date_input2 = input$dates_desc[2],
                    input_bins = input$bins, input_log = input$log_scale)

  })




############################# get data for sum stats table
  get_data_sum_stats_tables <- reactive({
    con <- path_setter()
    con <- con[[1]]
    string_value <- is.null(con)
    req(!string_value)
    df_need <- DBI::dbGetQuery(con,  querry_sum_stats_table())

    df_need
  })

  ################################# sum stats table
  output$sum_stats_table <- function(){
     # browser()
    df_need <- get_data_sum_stats_tables()
    sum_stats_table_creator(df_need, input$dates_desc[1], input$dates_desc[2])
  }


  ###### number of tweets display
  output$number_tweets_info <- renderText({
    df_need <- get_data_sum_stats_tables()

   glue("For current selection: {round(mean(df_need$N))} tweets on average per day")
  })


  ######################################################
  ########################## Word Frequencies ###########
  #######################################################
 lang_converter <- reactive({

  lang <- stringr::str_to_title(input$lang)
 })

  data_expl <- reactive({

    lang <- lang_converter()



    if (input$long == T){
      long <- "long_only"
      tweet_length_filter <- 81
    } else{
      long <- "all"
      tweet_length_filter <- 0
    }

    correct_path <- path_setter()[[3]]
    # if (correct_path == "correct_path"){
    #   Sys.sleep(0.2)
    # } else{
    #   return()
    # }

    # go into specified folder and load dataframe


    if (input$ngram_sel == "Unigram"){
      subfolder <- "uni_appended"
      add_on <- "uni"
    } else {
      subfolder <- "bi_appended"
      add_on <- "bi"
    }


    if (!is.null(input$comp)) {
      folder <- file.path("Companies")
      file_name <- glue("term_freq_{input$comp}_all_rt_{input$rt}_li_{input$likes}_lo_{long}.csv")
      file_path <- file.path("Twitter/term_freq",folder, subfolder, file_name)
      # read file
      data.table::fread(file_path, select = c("date_variable",
                                                              "language_variable",
                                                              "word",
                                                              "N",
                                                              "emo"),
                        colClasses = c("created_at" = "Date"))
    } else {
      folder <- glue("{lang}_NoFilter")
      file_name <- glue("{add_on}_{lang}_NoFilter_rt_{input$rt}_li_{input$likes}_lo_{long}.csv")
      file_path <- file.path("Twitter/term_freq",folder, subfolder, file_name)
      # read file
      data.table::fread(file_path, select = c("date",
                                              "language",
                                              "word",
                                              "N",
                                              "emo"),
                        colClasses = c("created_at" = "Date"))
    }






    #%>%
    # filter(between(date_variable, input$dates[1], input$dates[2]))








  })

  ######################### freq_plot
  output$freq_plot <- renderPlot({
    # dynamically change height of plot
    #height = function() input$n * 30 + 400,


      df <- data_expl()

#browser()
      if (input$plot_type_expl == "Frequency Plot"){
        df <- word_freq_data_wrangler(df, input$dates_desc[1], input$dates_desc[2],
                                      input$emo, emoji_words,
                                      input$word_freq_filter,
                                      tolower(input$lang),
                                      input$comp)

        df <- df_filterer(df, input$n)

        term_freq_bar_plot(df)

      }
    })

################## wordcloud
  output$wordcloud <- wordcloud2::renderWordcloud2({
  req(input$plot_type_expl == "Word Cloud")

    if (input$plot_type_expl == "Word Cloud"){
      df <- word_freq_data_wrangler(data_expl(), input$dates[1], input$dates[2],
                                    input$emo, emoji_words,
                                    input$word_freq_filter,
                                    tolower(input$lang),
                                    input$comp)

      df <- df_filterer(df, input$n)

      word_cloud_plotter(df, input$size_wordcloud)
    }
  })


############################## time series bigram plot
  output$word_freq_time_series <- renderPlot({
    df <- word_freq_data_wrangler(data_expl(), input$dates_desc[1], input$dates[2],
                                  input$emo, emoji_words,
                                  input$word_freq_filter, input$lang,
                                  input$comp)

     word_filter_time_series_plotter(df)
  })



###########################################################################
###########################################################################
######################### GOING DEEPER ####################################
###########################################################################
###########################################################################
  # path to markdown files for helpers
  shinyhelper::observe_helpers(help_dir = "shiny/helpers")


  ###### network plot

  data_getter_net <- reactive({
    lang <- stringr::str_to_title(input$lang_net)
    network_plot_datagetter(lang, input$dates_net[1], input$dates_net[2], input$comp_net)
  })

  data_filterer_net <- reactive({
    df <- data_getter_net()
    network_plot_filterer(df, input$rt, input$likes_net, input$long_net,
                          input$sentiment_net, input$search_term_net,
                          input$username_net)
  })


  # if button is clicked compute correlations und plot the plot
  observeEvent(input$button_net,{


    waitress <- waiter::Waitress$new("nav", max = 4,  theme = "overlay")
    #Automatically close it when done
    on.exit(waitress$close())

    waitress$notify()

    ### progress bar elements
    #hostess <- waiter::Hostess$new("load")




    ################################


    insertUI("#placeholder", "beforeEnd", ui = networkD3::forceNetworkOutput("network_plot"))

    # insertUI("#network_plotr", "beforeEnd", ui = networkD3::forceNetworkOutput("network_plot") %>%
    #            shinycssloaders::withSpinner())

    #insertUI("#placeholder", "afterEnd", ui = networkD3::forceNetworkOutput('network_plot'))

    initial.ok <- input$cancel_net


    shinyjs::showElement(id = "loading")
    # disable the button after computation started so no new computation can
    # be startedd


    disable("button_net")
    enable("cancel_net")


    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    ### read all files for the dates

    df <- data_getter_net()

    #hostess$set(2 * 10)
    waitress$inc(1)


   if(is.null(df)){
     enable("button_net")
     return()
   }

    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }




      network <- data_filterer_net()


      #hostess$set(2 * 10)
      waitress$inc(1)


    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    if (input$word_type_net == "word_pairs_net"){
      network <- network_unnester(network, df, input$emo_net)
    } else{
      network <- network_unnester_bigrams(network, input$emo_net)
    }

      #hostess$set(2 * 10)
      waitress$inc(1)


     if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    if (input$word_type_net == "word_pairs_net"){
      df <- network_word_corr(network, input$n_net,
                                             input$corr_net)
    } else {
      df <- network_bigrammer(df, network, input$n_net, input$n_bigrams_net)
    }



      # hostess$set(2 * 10)
      waitress$inc(1)

    # ### set up data for network
    # df <- network_plot_filterer(df, input$rt_net, input$likes_net, input$long_net,
    #                             input$sentiment_net, input$search_term_net,
    #                             input$username_net, input$n_net,
    #                             input$corr_net)



    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

        # if(is.null(df)){
    #   enable("button_net")
    #   return()
    # }


    # render the network plot
      if (input$word_type_net == "word_pairs_net"){
    output$network_plot <- networkD3::renderForceNetwork({





      req(input$button_net)
      #if (is.null(df)) return()
      validate(need(!is.null(df), message = "No data found for current selection"))
      if (initial.ok < input$cancel_net) {
        initial.ok <<- initial.ok + 1
        validate(need(initial.ok == 0, message = "The computation has been aborted."))
      }


      #hostess$set(2 * 10)
     # waitress$inc(1)
        network_plot_plotter(df)



    })
  } else {
    output$network_plot <- networkD3::renderForceNetwork({
    req(input$button_net)
    #if (is.null(df)) return()
    validate(need(!is.null(df), message = "No data found for current selection"))
    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    #hostess$set(2 * 10)
    #waitress$inc(1)
    network_plot_plotter_bigrams(df)




})



  }
    # Hide loading element when done
    # shinyjs::hideElement(id = 'loading')
    enable("button_net")
    disable("cancel_net")

  })


  observeEvent(input$reset_net,{
     # shinyjs::hide(id = "loading",
     #                     "network_plot",
     #               animType = T,
     #               time = 0)
    removeUI("#network_plot")
  })

  # observeEvent(input$button_net, {
  #
  #
  # })
  # ##

  ######## message for aborting process
  observeEvent(input$cancel_net, {

    showNotification("Computation has been aborted", type = "error")
  })




  output$raw_tweets_net <- DT::renderDataTable({
    dt <- data_filterer_net()

    DT::datatable(dt, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"))
    )
  })




######################################################################### add companies choice
 
###############################################################################
########################   XGboost    #########################################
###############################################################################
###flexible input for stocks: show either german or us companies
observe_helpers(withMathJax = TRUE, help_dir = "helpers")
  
  
output$stock_regression_xgb <- renderUI({
    req(path_setter()[[3]][1] == "correct_path")
    if (input$country_regression_xgb == "Germany"){
      input <- selectizeInput("Stock_Regression_xgb","Choose dependent variable:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression_xgb","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
})


output$Controls_xgb <- renderUI({
  #res <- dataset()
  #res$name <- NULL
  req(path_setter()[[3]][1] == "correct_path")
  if (input$country_regression_xgb == "Germany"){
    input <- selectizeInput("Controls_xgb","Choose control variables:",
                            c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
    #c(colnames(res[3:length(res)])),multiple = TRUE
  }else{
    input <- selectizeInput("Controls_xgb","Choose control variables:",
                            c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
  }
  
})
  

dataset_xgb <- reactive({
  req(path_setter()[[3]][1] == "correct_path")
  if (input$country_regression_xgb == "Germany"){
    data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_xgb]) &
                         .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
  } else {
    data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression_xgb]) &
                         .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
  }
  
  if (input$country_regression_xgb == "Germany"){
    global_controls <- global_controls_test_DE()   #load controls
    global_controls$Date <- as.Date(global_controls$Date) #transform date
    dax <- GDAXI()  #load dax
    dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
    dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
    colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
    global_controls <- left_join(dax,global_controls,by = c("Date")) #join final
    
  }else {
    global_controls <- global_controls_test_US() #same procedure as above
    global_controls$Date <- as.Date(global_controls$Date)
    dow <- DOW()
    dow$Date <- as.Date(dow$Date, " %b %d, %Y")
    dow <- missing_date_imputer(dow,"Close.")
    colnames(dow)[2] <- "DOW"
    global_controls <- left_join(dow,global_controls,by = c("Date"))
  }
  names(global_controls)[1] <- "Dates"
  data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
  ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
  data_reg2
})



df_selected_controls_xgb <- reactive({
  #req(input$Controls_var)
  res <- dataset_xgb()
  res <- res[c("Dates",input$regression_outcome_xgb,input$Controls_xgb)]
  res
})


observeEvent(input$Sentiment_type_xgb, {                         #Observe event from input (model choices)
  req(input$Sentiment_type_xgb)
  updateTabsetPanel(session, "params_xgb", selected = input$Sentiment_type_xgb)
})

observeEvent(input$industry_sentiment_xgb, {                         #Observe event from input (model choices)
  req(input$industry_sentiment_xgb)
  updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_xgb)
})


dataset_senti_xgb <- reactive({
  req(path_setter()[[3]][1] == "correct_path")
  req(input$Sentiment_type_xgb)
  if(input$Sentiment_type_xgb == "NoFilter"){
    
    res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
    #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
    #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
    #input$language
  }else{
    req(input$Stock_reg)
    ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
    res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()
    
  }
  
  
})


filtered_df_xgb <- reactive({
  req(path_setter()[[3]][1] == "correct_path")
  req(input$Sentiment_type_xgb)
  req(input$minRetweet_stocks1_xgb)
  req(input$minRetweet_stocks2_xgb)
  
  if(input$Sentiment_type_xgb == "NoFilter"){
    
    res <- dataset_senti_xgb()
  }else{ # live filtering
    req(input$industry_sentiment_xgb)
    res <- dataset_senti_xgb()
    if(input$industry_sentiment_xgb == "no"){
      res <- dataset_senti_xgb()
      if(input$tweet_length_stock1_xgb == "yes"){
        
        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)) &
                                (tweet_length > 81))}
      else{
        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)))
      }
    }#else{
    #res <- dataset_senti()
    #if(input$tweet_length_stock2 == "yes"){
    # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
    #                          (tweet_length > 81))
    #}else{
    #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
    #}
    #}
  }
})


aggri_select_xgb <- reactive({
  
  if(input$Sentiment_type_xgb == "NoFilter"){ # NoFilter files already aggregated
    res <- filtered_df_xgb()
    aggregation <- key(input$aggregation_xgb)  # select aggregation type: Mean, mean weighted by,...
    res <- res %>% tidyr::gather("id", "aggregation", aggregation)
    res <- res[c("date","aggregation")]
  }else{
    if(input$industry_sentiment_xgb == "no"){
      res <- filtered_df_xgb()
      res <- aggregate_sentiment(res) # function to aggregate sentiment per day
      res <- res %>% filter(language == input$language1_xgb)
      aggregation <- key(input$aggregation1_xgb)
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      res <- get_industry_sentiment(COMPONENTS_DE(),input$industry_xgb,input$minRetweet_stocks2_xgb,
                                    input$tweet_length_stock2_xgb)      #function to gather all stock in certain industry
      aggregation <- key(input$aggregation2_xgb)                          #--> also calculates aggregation inside function
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }
  }
  
})


observeEvent(input$reset_regression_xgb,{
  updateSelectizeInput(session,"Controls_xgb",selected = "")
})

#merge sentiment with control+dep vars
final_regression_df_xgb <- reactive ({
  if (input$senti_yesno_xgb == TRUE){
    res <- aggri_select_xgb()
  } else {
    res <- aggri_select_xgb()[1]
  }
  res$date <- as.Date(res$date)
  res_c <- df_selected_controls_xgb()
  res <- left_join(res_c,res, by=c("Dates" = "date"))
  res_corona <- df_selected_corona_xgb()
  res_corona$date <- as.Date(res_corona$date)
  res <- left_join(res,res_corona,by=c("Dates" = "date"))
  
  res
})


df_selected_corona_xgb <- reactive({
  #req(input$Controls_var)
  res <- corona_data_xgb()
  res <- res %>% dplyr::select(-X,-location)
  res <- res[c("date",input$corona_xgb)]
  res
})


corona_data_xgb <- reactive({
  req(path_setter()[[3]][1] == "correct_path")
  req(input$country_corona_xgb)
  CORONA_xgb(input$country_corona_xgb)
})

output$corona_vars_xgb <- renderUI({
  req(path_setter()[[3]][1] == "correct_path")
  res <- corona_data_xgb()
  res <- res %>% dplyr::select(-X,-location,-date)
  input <- selectizeInput("corona_xgb","Choose a corona related variable:",
                          names(res),multiple = TRUE)
  
})
observeEvent(input$reset_corona_xgb,{
  updateSelectizeInput(session,"corona_vars_xgb",selected = "")
})


output$xgb_summary <- function(){
  knitr::kable(df_need_xgb(), caption = glue("Summary statistics"),colnames = NULL) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)
}


df_need_xgb <- reactive({
  df_need <- round(describe(final_regression_df_xgb()[-1])[c(3, 4, 5, 8, 9)], 2)
  test <- nrow(df_need)
  test2 <- nrow(df_need)==1
  if (nrow(df_need == 1)) {
    row.names(df_need)[1] <- input$regression_outcome_xgb
  } else{
    df_need <- df_need
  }
  df_need
  
})

output$correlation_xgb <- renderPlot({
  ggpairs(final_regression_df_xgb()[-1])
})




output$acf_plot_xgb <- renderPlot({
  req(input$correlation_xgb_plot)
  req(input$correlation_type)
  acf_plot_xgb(final_regression_df_xgb(),input$correlation_xgb_plot)
})


output$pacf_plot_xgb <- renderPlot({
  req(input$correlation_xgb_plot)
  req(input$correlation_type)
  pacf_plot_xgb(final_regression_df_xgb(),input$correlation_xgb_plot)
})


output$correlation_plot_choice <- renderUI({
  res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
    input <- selectInput("correlation_xgb_plot","Select variable for plot",
                       names(res))
  
})


# output$Lag_choice <- renderUI({
#   res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
#   input <- selectizeInput("var_list_xgb","Add AR and MA columns for which variables?",
#                           names(res),selected="")
#   
# })

# observeEvent(input$number_of_vars, {                         #Observe event from input (model choices)
#   req(input$number_of_vars)
#   updateTabsetPanel(session, "tabs_for_xgb", selected = as.character(input$number_of_vars))
# })
# 
# observeEvent(input$lag_tabs,{
#   res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
#   updateSelectInput(session, "var_1",
#                     choices = names(res))
# })

output$add_features <- renderUI({
  res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
  input <- selectInput("var_1","Chose variable to add AR and/or MA features",
                       names(res))
  
})


observeEvent(input$lag_tabs, {                         #Observe event from input (model choices)
  req(input$lag_tabs)
  updateTabsetPanel(session, "lag_tab", selected = input$lag_tabs)
})
######################################Custom dataset############################
xchange <- reactiveValues()
xchange$df_full <- NULL
xchange$df_fullb <- NULL
xchange$df_full2 <- NULL
xchange$df_full3 <- NULL
xchange$df_full4 <- NULL
xchange$df_full5 <- NULL


xchange$df1 <- NULL
xchange$df2 <- NULL
#
Ma_part1 <- reactive({
  res <- final_regression_df_xgb()
  res <- lag_cols(res,input$regression_outcome_xgb)
  res <- make_ts_stationary(res)
  list_nums <- regmatches(input$ma_select, gregexpr("[[:digit:]]+", input$ma_select))
  num_vec <- as.numeric(unlist(list_nums))
  single_nums <- unlist(str_split(input$ma_select, ","))
  variables_list <- rep(input$var_1, length(num_vec))
  helpi <- rep("MA",length(num_vec))
  names_vec <- paste0(helpi,variables_list, single_nums)
  #

  help_matrix <- mapply(c,num_vec, variables_list, SIMPLIFY = T)
  lies <- NULL

  for(i in 1:length(num_vec)){

    if(help_matrix[1,i] > 0){
      avg_len <- as.numeric(help_matrix[1,i])
      x <- zoo(res[,help_matrix[2,i]])
      x <- as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))
     # names(x)[1] <- paste("MA_",help_matrix[2,i],sep = "")
      lies[[i]] <- x
    }else{
      res
    }


  }

  lies <- as.data.frame(do.call(cbind, lies))
  colnames(lies) <- names_vec
  lies
  
})

Ma_part2 <- reactive({
  
  a <- input$ma_select2
  res <- final_regression_df_xgb()
  res <- lag_cols(res,input$regression_outcome_xgb)
  res <- make_ts_stationary(res)
  list_nums <- regmatches(a, gregexpr("[[:digit:]]+", a))
  num_vec <- as.numeric(unlist(list_nums))
  single_nums <- unlist(str_split(a, ","))
  variables_list <- rep(input$var_1, length(num_vec))
  helpi <- rep("EMA",length(num_vec))
  names_vec <- paste0(helpi,variables_list, single_nums)
  #

  help_matrix <- mapply(c,num_vec, variables_list, SIMPLIFY = T)
  lies <- NULL

  for(i in 1:length(num_vec)){

    if(help_matrix[1,i] > 0){
      avg_len <- as.numeric(help_matrix[1,i])
      x <- res[,help_matrix[2,i]]
      x <- as.data.frame(TTR::EMA(x, avg_len))
      lies[[i]] <- x
    }else{
      res
    }


  }

  lies <- as.data.frame(do.call(cbind, lies))
  colnames(lies) <- names_vec
  lies


})


final_regression_diff <- reactive({
  
  res <- final_regression_df_xgb()
  res <- lag_cols(res,input$regression_outcome_xgb)
  res <- make_ts_stationary(res)
  res
})
v <- reactive({
  validate(validate_MA(input$num_1))
})
# validate that MA cannot be 1

rv_action_button <- reactiveValues(i = 0)

observeEvent(input$addButton,{
  rv_action_button$i <- 0
})

observe({
  isolate({rv_action_button$i = rv_action_button$i + 1})
}) 

output$error_text <- renderText({
  if(input$ma_select == "" | input$ma_select2 == ""){
    b <- "If you dont want to add a moving average - set  input to 1"
  }else{
    b <- ""
  }
})

v_1a <- reactive({
  validate(validate_iregulars(input$ma_select))
})
v_1b <- reactive({
  validate(validate_iregulars(input$ma_select2))
})

v_2a <- reactive({
  validate(validate_no_decimals(input$ma_select))
})
v_2b <- reactive({
  validate(validate_no_decimals(input$ma_select2))
})

v_3a <- reactive({
  validate(validate_negatives(input$ma_select))
})

v_3b <- reactive({
  validate(validate_negatives(input$ma_select2))
})
v_4a <- reactive({
  validate(validate_Large_numbers(input$ma_select))
})

v_4b <- reactive({
  validate(validate_Large_numbers(input$ma_select2))
})
v_5a <- reactive({
  validate(validate_no_zeros(input$ma_select))
})
v_5b <- reactive({
  validate(validate_no_zeros(input$ma_select2))
})


observe({
  #if(input$addButton > 0) {
  if(rv_action_button$i == 0){ 
    validate(
      need(input$ma_select != "","dont"))
    validate(
      need(input$ma_select2 != "","dont"))
    v_1a()
    v_1b()
    v_2a()
    v_2b()
    v_3a()
    v_3b()
    v_4a()
    v_4b()
    v_5a()
    v_5b()
    if((input$var_1 == "Close") | (input$var_1 == "Return")){
      #v()
      
      isolate(c <- Ma_part2())
      isolate(b <- Ma_part1())
      rv_action_button$i <- 1
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full <-  cbind(final_regression_diff(),Ar_part,c,b))}
    
    else if(input$var_1 == "VIX"){
     # v()
      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full2 <-  cbind(final_regression_diff(),Ar_part,c,b))}
    else if(input$var_1 == "coronavirus"){
      #v()
      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full3 <-  cbind(final_regression_diff(),Ar_part,c,b))}
    else{
      #v()
      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full4 <-  cbind(final_regression_diff(),Ar_part,c,b))
    }
  }
 
})

 
observe({
  if(input$reset_cus > 0) {
  xchange$df_full <- NULL
  xchange$df_fullb <- NULL
  xchange$df_full2 <- NULL
  xchange$df_full3 <- NULL
  xchange$df_full4 <- NULL
  xchange$df_full5 <- NULL}
})

custom_df <- eventReactive(input$finish, { 
    list_dfs <- c(xchange$df_full,xchange$df_full2,xchange$df_full3,xchange$df_full4)
      df <- data.frame((sapply(list_dfs,c)))
      df <- df[!duplicated(as.list(df))]
      df <- df %>% dplyr::select(-contains("."))
      df$Dates <- as.Date(df$Dates)
      cols <- setdiff(colnames(df), "date")
      df <- df[,cols]
      if(input$corona_dummy == "yes"){
        df <- corona_dummy(df)
      }else{
        df
      }
      df
})


# Render text when button is clicked
# output$text <- renderText({
#   if (input$actionButtonId == 0)
#     return("")
#   isolate({
#     # Your logic here
#   })
# })
output$tableCustom <- DT::renderDataTable({
  DT::datatable(custom_df(),options = list(
    autoWidth = FALSE, scrollX = TRUE)) %>% DT::formatStyle(names(custom_df()),
                                                            lineHeight = '80%',
                                                            lineWidth = '80%') 
})

######################################Default dataset###########################
df_xgb <- reactive({

  res <- final_regression_df_xgb()

  res <- ARMA_creator(res,input$regression_outcome_xgb)
})

observeEvent(input$lag_tabs, {
  req(input$lag_tabs)
  if((ncol(final_regression_df_xgb()) == 2)){
  showNotification("No further variables selected. Model will be trained on features from dep. variable", type = "warning")}

})


output$df_xgb_default <- DT::renderDataTable({
  DT::datatable(df_xgb(),options = list(
    autoWidth = FALSE, scrollX = TRUE)) %>% DT::formatStyle(names(df_xgb()),
                                                            lineHeight = '80%',
                                                            lineWidth = '80%')
})
# 

df_xgb_train <- reactive({
  if(input$lag_tabs == "default"){
    res <- final_regression_df_xgb()
    res <- lag_cols(res,input$regression_outcome_xgb)
    res <- make_ts_stationary(res)
    list_dfs <- split_data_for(res,input$n_ahead,input$ftpye,input$regression_outcome_xgb)
    res <- ARMA_creator(res,input$regression_outcome_xgb)
    
    cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
    res <- res[,c("date",cols)]
    list_dfs$df_train <- left_join(list_dfs$df_train,res)
  }else{
    res <- custom_df()
    
    list_dfs <- split_data_for(res,input$n_ahead,input$ftpye,input$regression_outcome_xgb)
  }

  res <- ARMA_creator_for(list_dfs$df_forecast,list_dfs$df_train)

  #rename with columns from train
  list_dfs$df_forecast <- res
  # corona dummy
  
  list_dfs
})



df_xgb_train_for <- reactive({
  if(input$lag_tabs == "default"){
    res <- final_regression_df_xgb()
    res <- lag_cols(res,input$regression_outcome_xgb)
    res <- make_ts_stationary(res)
    list_dfs <- split_data_for_ahead(res,input$n_ahead2,input$ftpye2)
    res <- ARMA_creator(res,input$regression_outcome_xgb)
    
    cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
    res <- res[,c("date",cols)]
    list_dfs$df_train <- left_join(list_dfs$df_train,res)
  }else{
    res <- custom_df()
    list_dfs <- split_data_for_ahead(res,input$n_ahead2,input$ftpye2)
  }
  
  res <- ARMA_creator_for(list_dfs$df_forecast,list_dfs$df_train)
  
  #rename with columns from train
  list_dfs$df_forecast <- res
  
  list_dfs
})


# output$model_xgb <- renderPrint({
#   model_xgbi()[[1]]
# })
model_xgbi <- eventReactive(input$run,{
  req(input$model_spec)
  if(input$model_spec == "default"){
    res <- df_xgb_train()
    model1 <- model_xgb(res$df_train)
    model1
    
    }else if(input$model_spec == "custom"){
    res <- df_xgb_train()
    model2 <- model_xgb_custom(res$df_train,input$mtry,input$trees,input$min_n,input$tree_depth,
                               input$learn_rate,input$loss_reduction,input$sample_size)
    model2
  }else{
    res <- df_xgb_train()
    model3 <- model_xgb_hyp(res$df_train,input$trees_hyp,input$grid_size)
    model3
  }
  
  
})

observeEvent(input$model_spec, {                         #Observe event from input (model choices)
  req(input$model_spec)
  updateTabsetPanel(session, "mod_spec", selected = input$model_spec)
})

observeEvent(input$model_spec_for, {                         #Observe event from input (model choices)
  req(input$model_spec_for)
  updateTabsetPanel(session, "mod_spec_for", selected = input$model_spec_for)
})
##########disable prediction when the forecast window changes --> enable when model is trained
rv_disable <- reactiveValues(i = 1)

observeEvent(input$run,{
  rv_disable$i <- 0
})

observe({
  isolate({rv_disable$i = rv_disable$i + 1})
})

observe({
  if(rv_disable$i == 0){ 
    shinyjs::enable("pred")}
  else{
    shinyjs::disable("pred")
    shinyjs::show("text2")
    
  }
})

rv_prev_input <- reactiveValues(prev_input = NULL)

observeEvent(input$n_ahead, {
  rv_prev_input$prev_input <- c(rv_prev_input$prev_input , input$n_ahead)
  if(rv_prev_input$prev_input[1] != input$n_ahead){
    rv_disable$i <- 1
  }
})

#########################

rv_disable_2 <- reactiveValues(i = 1)

observeEvent(input$run,{
  rv_disable_2$i <- 0
})

observe({
  isolate({rv_disable_2$i = rv_disable_2$i + 1})
})

observe({
  if(rv_disable_2$i == 0){ 
    shinyjs::enable("pred")}
  else{
    shinyjs::disable("pred")
    shinyjs::show("text2")
    
  }
})

rv_prev_input_2 <- reactiveValues(prev_input = NULL)

observeEvent(input$mod_spec, {
  rv_prev_input_2$prev_input <- c(rv_prev_input_2$prev_input , input$mod_spec)
  if((rv_prev_input_2$prev_input[1] != input$mod_spec)){
    rv_disable_2$i <- 1
  } else{
    if(rv_prev_input$prev_input[1] == input$n_ahead){
    rv_disable_2$i <- 0}
    else{
      rv_disable_2$i <- 1
    }
  }
})










#################

prediction_xgb <-  eventReactive(input$pred,{
  
  
  res <- df_xgb_train() 
  colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
  
  colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  model <- model_xgbi()[[1]]
  
  preds <- model %>%
    fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
    predict(new_data = res$df_forecast[,c(-1)])
  
  df_orig <- final_regression_df_xgb()
  #a <- df_orig$Close[1]
  #abc <- diffinv(res$df_train$y, xi = a)
  if(adf.test(df_orig[,2],k=2)$p.value > 0.1){
  preds <- cumsum(preds) + df_orig[(nrow(res$df_train)),2]
  }
  preds
})


observeEvent(input$run, {
  shinyjs::disable("run")
  shinyjs::show("text1")
  shinyjs::disable("pred")
  shinyjs::hide("text2")
  
})  




output$model_fit <- function(){

  if(rv_prev_input$prev_input[1] != input$n_ahead){
    return()
  }else{
      res <- df_xgb_train()
      colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"

      model_xgboost <-  model_xgbi()[[1]] %>%
        fit(formula = y ~ .,data = res$df_train[,c(-1)])

      fits <- predict(model_xgboost,res$df_train[,c(-1)])
      
      diff <- (fits[,1]-res$df_train[,"y"])
      mean_diff <- mean(diff[,1]^2)
      rsme <- sqrt(mean_diff)
      mean_abs <- mean(abs(diff[,1]))
      diff_per <- ((res$df_train[,"y"]-fits[,1])/res$df_train[,"y"]) 
      diff_per <- diff_per[!is.infinite(rowSums(diff_per)),]
      mape <- mean(abs(diff_per)*100)
   
      
      df_need <- data.frame(c(rsme,
                              mean_abs,
                              mape),
                            row.names = c("RMSE","MAE","MAPE"))
      colnames(df_need)<- "value"
      knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
        kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                  position = "center",
                                  font_size = 16)
}
}


output$xgb_metrics <- function(){
 
  
  preds <- prediction_xgb()
  res <- df_xgb_train()
  df_orig <- final_regression_df_xgb()
  y <- df_orig  %>% filter(Dates >= min(res$f_dates) & Dates <= max(res$f_dates))
  diff <- (preds[,1]-y[,2])
  mean_diff <- mean(diff[,1]^2)
  rsme <- sqrt(mean_diff)
  mean_abs <- mean(abs(diff[,1]))
  diff_per <- ((y[,2]-preds[,1])/y[,2]) 
  mape <- mean(abs(diff_per[,1])*100)
  
  df_need <- data.frame(c(rsme,
                          mean_abs,
                          mape),
                        row.names = c("RMSE","MAE","MAPE"))
  colnames(df_need)<- "value"
  knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)
  
}


output$serial_out_xgb <- function(){
  shinyjs::enable("run")
  shinyjs::hide("text1")
  shinyjs::enable("pred")
  shinyjs::hide("text2")
  if(rv_prev_input$prev_input[1] != input$n_ahead){
    return()
  }else{
  res <- df_xgb_train()
  colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  
    model_xgboost <- model_xgbi()[[1]] %>%
      fit(formula = y ~ .,data = res$df_train[,c(-1)])

  
  fits <- predict(model_xgboost,res$df_train[,c(-1)])
  
  resids <- (res$df_train$y - fits)
  m <- Box.test(resids, lag = 12, type="Lj")
  m
  
  df_need <- data.frame(c(round(m$statistic,5),
                          round(m$p.value,5),
                          m$method),
                        row.names = c("statistic","p.value","method"))
  colnames(df_need)<- "Summary"
  knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)
  
  
  
  
  }
}
# 
# output$serial_out_xgb <- renderPrint({
#   serial_test_xgb()
# })

output$forecast_xgb <- renderDygraph({
  full_df <- final_regression_df_xgb()
  res <- df_xgb_train()
  preds <- prediction_xgb()
  preds <- preds %>%
    zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))
  
  if(input$forecast_plot_choice == "Full"){

    ts <- full_df %>% pull(input$regression_outcome_xgb) %>%
      zoo(seq(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), by = "day"))
    
    {cbind(actuals=ts, predicted=preds)} %>% dygraph() %>%
      dyEvent(as.Date(min(res$f_dates)), "Start of prediction", labelLoc = "bottom",color = "red") %>%  dyOptions(colors = c("white","green"))
    
  }else if(input$forecast_plot_choice == "Forecasted"){
    ts <- full_df %>% pull(input$regression_outcome_xgb) %>%
      zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))
    
    {cbind(actuals=ts, predicted=preds)} %>% dygraph() %>%  dyOptions(colors = c("white","green"))
    
    
  }else{
    colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
    
    model_xgboost <-  model_xgbi()[[1]] %>%
      fit(formula = y ~ .,data = res$df_train[,c(-1)])
    plot <- xgb.importance(model=model_xgboost$fit) %>% xgb.ggplot.importance(
      top_n=20, measure=NULL, rel_to_first = F)
  plot
  }
  
})


model_xgbi2 <- eventReactive(input$run2,{#maybe rename y column to y
  req(input$model_spec_for)
  if(input$model_spec_for == "default"){
    res <- df_xgb_train_for()
    model1 <- model_xgb(res$df_train)
    model1
  }else if(input$model_spec_for == "custom"){
    res <- df_xgb_train_for()
    model2 <- model_xgb_custom(res$df_train,input$mtry1,input$trees1,input$min_n1,input$tree_depth1,
                               input$learn_rate1,input$loss_reduction1,input$sample_size1)
    model2
  }else{
    res <- df_xgb_train_for()
    model3 <- model_xgb_hyp(res$df_train,input$trees_hyp1,input$grid_size1)  
    model3
  }
})

output$model_xgb2 <- renderPrint({
  model_xgbi2()[[1]]
})

prediction_xgb_actual <-  eventReactive(input$pred2,{
  res <- df_xgb_train_for()
  colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
  colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  
  preds <- model_xgbi2()[[1]]  %>%
    fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
    predict(new_data = res$df_forecast[,c(-1)])
  df_orig <- final_regression_df_xgb()
  #a <- df_orig$Close[1]
  #abc <- diffinv(res$df_train$y, xi = a)
  if(adf.test(df_orig[,2],k=2)$p.value > 0.1){
    preds <- cumsum(preds) + df_orig[(nrow(res$df_train)),2]
  }

})


output$plot_1_xgb_actual <- renderDygraph({
  full_df <- final_regression_df_xgb()
  res <- df_xgb_train_for()
  preds <- prediction_xgb_actual()

  preds <- preds %>%
    zoo(seq(from = as.Date(max(full_df$Dates)) +1,
            to = as.Date(max(full_df$Dates)) + input$n_ahead2, by = "day"))
  
  ts <- full_df %>% pull(input$regression_outcome_xgb) %>%
    zoo(seq(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), by = "day"))
  
  {cbind(actuals=ts, predicted=preds)} %>% dygraph() %>%
    dyEvent(as.Date(max(full_df$Dates)), "Start forecast", labelLoc = "bottom",color = "red") %>%  dyOptions(colors = c("white","green"))
  #%>% dyCSS("C:/Users/simon/Desktop/WS_20_21/Git_tracked_Sentiment_App/DSP_Sentiment_Covid_App/test_simon/SimonApp/css/dygraph.css")
  
  
})


serial_test_xgb_for <- reactive({
  res <- df_xgb_train_for()
  colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  
  model_xgboost <-  model_xgbi2()[[1]]  %>%
    fit(formula = y ~ .,data = res$df_train[,c(-1)])
  
  fits <- predict(model_xgboost,res$df_train[,c(-1)])
  
  resids <- res$df_train$y - fits

  m <- Box.test(resids, lag = 12)
  
  m
  
  
})

output$serial_out_xgb_for <- renderPrint({
  serial_test_xgb_for()
})


output$xgb_date_check <- renderText({
  
  if(length(input$date_regression_xgb) > 1){
    days_inrange <- difftime(as.Date(input$date_regression_xgb[2]) ,as.Date(input$date_regression_xgb[1]) , units = c("days"))
    if (days_inrange < 30){
      
      validate("Less than 30 days selected. Please choose more days.")
      
    }
  } else if (is.null(input$date_regression_xgb)){
    
    validate("Need to select at least one day.")
  }
})

output$finish_button <- renderUI({
  req(!is.null(xchange$df_full) | !is.null(xchange$df_full2) | !is.null(xchange$df_full3)
      | !is.null(xchange$df_full4) | !is.null(xchange$df_full5))
  actionButton("finish", "Finish")
  
})



}
