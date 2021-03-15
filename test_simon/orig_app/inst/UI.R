############################not original
Sys.setlocale("LC_TIME", "English")


##### die ui struktur für die Regression und VAR sind jetzt im file "uiElements.R" als function




#################################################################################
################################### directory ###################################
#################################################################################

dir_setter_panel <- function() {
  tabPanel("Select Working Directory",
           fluidRow(column(4,

             tags$p(),
             tags$p("Please choose the directory containing the folder containig \n
               the data called 'Data'."),
             shinyFiles::shinyDirButton("directory", "Folder select", "Please select a folder"
             ),

           ),
           column(8,
             tags$h4("Selected folder"),
             tags$p(HTML("Please check that you picked the correct folder containing \n
             the 'Data' folder. otherwise the App will not work.")),
             textOutput("directorypath"),
             tags$hr())
           ),


           fluidRow(column(12, align = "center",
              tags$hr(),
              tags$p(),
             imageOutput("twitter_logo")
           )
        )

  )
}


###############################################################################
######################### twitter #############################################
###############################################################################

# the main panel for twitter containing all sub panels and main panels
twitter_main_panel <- function(){
  navbarMenu("Twitter",

             ###### tab panel for descriptive
             tabPanel("Descriptives Main",
                      # sidebar panel for descriptive
                      #twitter_desc_panel(),
                      sidebarPanel(
                        twitter_tab_desc
                      ),





                      ########## main panels for Descritpive reiter
                       mainPanel(
                        tabsetPanel(id = "tabselected",

                          ### panel with histograms and summary table
                          tabPanel("Time Series Sentiment & Other Metrics", value = 1,
                                    #summary statistics table
                                   tableOutput("sum_stats_table"),


                                    # first time series plot
                                    textOutput("number_tweets_info"),
                                   tags$head(tags$style("#number_tweets_info{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white
                                 }"
                                   )
                                   ),
                                     #plotOutput('sum_stats_plot'),
                                   tags$head(
                                     # Note the wrapping of the string in HTML()
                                     tags$style(HTML("

                                        .dygraph-axis-label {
                                          font-size: 12px;
                                          color:white;
                                        }
                                        .dygraph-legend {
                                          color: black;
                                        }


                                                     "))
                                   ),
                                   dygraphs::dygraphOutput("sum_stats_plot"),

                                   # seconds time series plot
                                   plotOutput('sum_stats_plot2'),

                                   # histogram
                                   plotOutput("histo_plot") %>%
                                     shinycssloaders::withSpinner()


                                   ),

                          ##### main panel with wod frequency and raw tweets
                          tabPanel("Exploratory Output", value = 3,
                                  # mainPanel(
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Frequency Plot'",
                                       plotOutput("freq_plot", height = "800px")
                                       #uiOutput("plot.ui")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Word Cloud'",
                                       "text",
                                       wordcloud2::wordcloud2Output('wordcloud', height = "800px", width = "auto")
                                     ),
                                  tags$hr(),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  conditionalPanel(
                                    condition = "input.ngram_sel == 'Bigram'",
                                    tags$h4("Number of Bigrams containing the choosen word (if no word selected shows all tweets in current selection)"),
                                    plotOutput("word_freq_time_series")
                                  )


                                   )
                                    ))
                      ),
             ################### tab panel descirptive end




             # andere reiter
             tabPanel("Going Deeper",
                      sidebarPanel(
                        network_sidebar
                      ),
                      mainPanel(
                      # shinyjs::hidden(div(id = "loading",
                      #                     networkD3::forceNetworkOutput("network_plot") %>%
                      #     shinycssloaders::withSpinner()))
                      tags$div(id = "placeholder")
                      ),


                      fluidRow(column(12,
                                      tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }

                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}


                    .dataTables_length select {
                           color: #000000;
                           background-color: #ffffff
                           }


                    .dataTables_filter input {
                            color: #000000;
                            background-color: #ffffff
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "


                                      )),
                                      DT::dataTableOutput("raw_tweets_net")))),
             tabPanel("Daily Analysis"),
             tabPanel("Going deeper"))

}




### conditional sidebar panel for the time series
twitter_desc_conditional_sum_stats <- conditionalPanel(

  #condition = "input.plot_type == 'Frequency Plot'",
  # keep for both because bigram also makes senese with wordcloud
  condition = "input.tabselected==1",
  radioButtons("metric", "Select a metric",
               choiceNames = c("Mean", "Standard deviation", "Median"),
               choiceValues = c("mean", "std", "median")),
  checkboxInput("num_tweets_box", label = "Show the average number of tweets per day", value = F)
)

#### sidebar layout for descriptives
twitter_tab_desc <- tabPanel( "Descriptives",


                              ####### all three
                              radioButtons("lang", "Select Language", choices = c("EN", "DE")),

                              selectInput("comp","Choose a company (optional)",
                                             c("adidas", "NIKE"),
                                             selected = "",multiple = TRUE),
                              shinyWidgets::airDatepickerInput("dates_desc", "Date range:",
                                                               range = TRUE,
                                                               value = c("2018-11-30", "2021-02-19"),
                                                               maxDate = "2021-02-19", minDate = "2018-11-30",
                                                               clearButton = T, update_on = "close"),


                              radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              #switchInput(inputId = "long", value = TRUE),
                              shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),



                              ##### only descr
                              conditionalPanel(
                                condition = "input.tabselected==1",




                                selectInput("value", "Which value would you like to show",
                                            choices = c(
                                              "Sentiment" = "sentiment",
                                              "Retweets Weighted Sentiment" = "sentiment_rt",
                                              "Likes Weighted Sentiment" = "sentiment_likes",
                                              "Length Weighted Sentiment" = "sentiment_tweet_length",
                                              "Retweets" = "rt",
                                              "Likes"="likes",
                                              "Tweet Length" = "tweet_length"
                                            ),
                                            selected = "sentiment", multiple = T),

                                # additional elemtns for time series analysis
                                twitter_desc_conditional_sum_stats,

                                ## additional elements for histogram
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$hr(),
                                tags$h3("Histogram"),
                                sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 500, value = 50),


                                # add switch whether to use logarithmic scale
                                shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                                                          value = F,
                                                          size = "small",
                                                          handleWidth = 100)



                              ),

                            conditionalPanel(
                              condition = "input.tabselected==3",
                              shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F),
                              selectInput("plot_type_expl", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")),
                              sliderInput("n", "Number of words to show", min = 5, max = 200, value = 15),

                              conditionalPanel(

                                #condition = "input.plot_type == 'Frequency Plot'",
                                # keep for both because bigram also makes senese with wordcloud
                                condition = "true == true",
                                radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
                              ),

                              # word search bigrams
                              conditionalPanel(
                                condition = "input.ngram_sel == 'Bigram'",
                                shinyWidgets::searchInput("word_freq_filter", "Enter your search term",
                                                          placeholder = "Placeholder",
                                                          value = "",
                                                          btnSearch = icon("search"),
                                                          btnReset = icon("remove"))
                              ),
                              conditionalPanel(
                                condition = "input.word_freq_filter != '' & input.plot_type_expl == 'Word Cloud'",
                                sliderInput("size_wordcloud", "Change the size of the wordcloud", min = 1, max = 5, value = 1)
                              ),
                            )

                            )




#################################### going deeeper sidbarpanel
network_sidebar <- tabPanel( "Network Analysis",

          waiter::use_waitress(color = "#375a7f"),
          #waiter::use_waiter(),
          #waiter::use_hostess(),
          #waiter::hostess_loader("load", text_color = "white", center_page = TRUE),


          ###### language selector
          radioButtons("lang_net", "Select Language", choiceNames = c("English Tweets", "German Tweets"),
                       choiceValues = c("en", "de")),
          # company selector
          selectInput("comp_net","Choose a company (optional)",
                      c("adidas", "NIKE"),
                      selected = "",multiple = TRUE),

          # datepicker
          shinyWidgets::airDatepickerInput("dates_net", "Date range:",
                                           range = TRUE,
                                           value = "2020-03-01",
                                           maxDate = "2021-02-19", minDate = "2018-11-30",
                                           clearButton = T, update_on = "close",
                                           multiple = 5),

          ##### same but continous choices
          # retweets count
          numericInput("rt_net", "Choose a minimum number of retweets", min = 0, max = 10000, value = 0),

          # likes count
          numericInput("likes_net", "Choose a minimum number of likes", min = 0, max = 10000, value = 0),

          # long tweets switch
          shinyWidgets::materialSwitch(inputId = "long_net", label = "Long Tweets only?", value = F),

          # filter out emoji words
          shinyWidgets::materialSwitch(inputId = "emo_net", label = "Remove Emoji Words?", value = F),



          ### filter by sentiment
          sliderInput("sentiment_net", label = h3("Choose a sentiment range"),
                      min = -1, max = 1, value = c(-1, 1), step = 0.01, dragRange = T),




          ##### additional
          ######### search terms
          textInput("search_term_net", "Only select tweets containing the following:"),
          textInput("username_net", "Only show tweets for usernames containing the following:"),


          ####### type of plot bigram/word pairs
          selectInput("word_type_net", "Select the type of word combination you would like to analyse", choices = c("Bigram" = "bigrams_net",
                                                                                                                    "Word Pairs" = "word_pairs_net")),



          ######## adjusting plot
          numericInput("n_net", "Minimum number of occurences of a single word in the sample",
                       min = 50, value = 50),


          ### panel in case of word pairs to compute word correlations
          conditionalPanel(
            condition = "input.word_type_net == 'word_pairs_net'",
            numericInput("corr_net", "Minimum word correlation of word pairs", value = 0.15, min = 0.15, max = 1,
                         step = 0.01)

          ),


          ### panel for minium number of time bigrams arise
          conditionalPanel(
            condition = "input.word_type_net == 'bigrams_net'",
            numericInput("n_bigrams_net", "Minimum number of occurences of a Bigram in the selected sample",
                         min = 50, value = 50)
          ),


          actionButton("button_net", "Render Plot") %>%
          shinyhelper::helper(type = "markdown",
                   title = "Inline Help",
                   content = "network_plot_button",
                   buttonLabel = "Got it!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "s"),

          #### removing plot
          actionButton("reset_net", "Remove Plot"),


          ### canceling computation
          shinyjs::disabled(actionButton("cancel_net", "Cancel Rendering"))
)


#############################################################################
################### database
############################################################################
### connect to database






# #### word freq tab
# tab_panel_twitter_expl <-
# )



Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("darkly"),
  #shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("APP",

             dir_setter_panel(),
             twitter_main_panel(),
             tabPanel("Sentiment"),
             tabPanel("Stocks",
                      sidebarPanel(
                        radioButtons("country_stocks","Which country?",c("Germany","USA"),selected = "Germany"),
                        #selectize_Stocks(COMPONENTS_DE()),
                        uiOutput("stock_choice"),
                        #selectizeInput("stock_choice", choices = "Platzhalter"),
                        radioButtons("stock_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume","Return"),selected = "Close"),
                        actionButton("reset", "clear selected"),
                        checkboxInput("hovering","Enable hover",value = FALSE),
                        sliderinput_dates()
                      ),
                      mainPanel(
                        plot_stocks_DE(),
                        hover_info_DE()
                      ),#close MainPanel
             ),#close tabPanel stock
             tabPanel("Corona",
                      sidebarPanel(
                        selectize_corona(),
                        checkboxGroupInput("CoronaCountry","Country",c("Germany","United States"),selected = "Germany"),
                        sliderinput_dates_corona(),
                        checkboxInput("hovering_corona","Enable hover",value = FALSE)
                      ),
                      mainPanel(
                        plot_corona(),
                        hover_info_corona()
                      )
             ),#close tabPanel Corona
             navbarMenu("Model",
                        tabPanel("Granger",
                                 sidebarPanel(
                                   radioButtons("country_granger","Which country?",c("Germany","USA"),selected = "Germany"),
                                   uiOutput("Stock_Granger"),
                                   # selectizeInput("Stock_Granger","Choose first argument: Company or Index",
                                   #                c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                                   #                selected = "Bayer ",multiple = FALSE),
                                   radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume","Return"),selected = "Close"),
                                   uiOutput("ControlsGranger"),
                                   #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
                                   sliderInput("date_granger",label="Timeseries",
                                               value = c(as.Date("2020-02-12"),as.Date("2021-02-12")),
                                               min = as.Date("2020-01-02"),
                                               max = as.Date("2021-02-12"),
                                               step = 1,timeFormat = "%F"),
                                   checkboxInput("direction_granger","Second variable causes first?",value = TRUE)
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information Granger",
                                              htmlOutput("info_granger")),
                                     tabPanel("Visualize",
                                              dygraphs::dygraphOutput("stocks_granger"),
                                              dygraphs::dygraphOutput("second_granger")),
                                     tabPanel("Background-steps",
                                              htmlOutput("grangertext1"),
                                              verbatimTextOutput("optimallags"),
                                              htmlOutput("grangertext2"),
                                              verbatimTextOutput("dickey_fuller"),
                                              verbatimTextOutput("dickey_fuller_second"),
                                              htmlOutput("grangertext3"),
                                              verbatimTextOutput("dickey_fuller_diff"),
                                              verbatimTextOutput("dickey_fuller_second_diff")),
                                     tabPanel("Results",
                                              verbatimTextOutput("granger_result"),
                                              htmlOutput("granger_satz"))))),
                        tabPanel("Regression Analysis",
                                 sidebarPanel(
                                   tabs_custom()
                                 ),
                                 mainPanel(
                                   tabsetPanel(id = "regressiontabs",
                                     tabPanel("regression",
                                              #verbatimTextOutput("testi_table"),
                                              #verbatimTextOutput("senti"),
                                              #verbatimTextOutput("senti_agg"),
                                              htmlOutput("regression_equation"),
                                              verbatimTextOutput("regression_result")),
                                     tabPanel("Quantile Regression",value=1,
                                              #plotOutput("plot_dens_Qreg"),
                                              verbatimTextOutput("regression_result_Qreg")

                                     )
                                   )
                                 )
                        ),
                        tabPanel("VAR-forecasting",
                                 sidebarPanel(
                                   tabs_custom_var(),
                                   numericInput("ahead", "choose how many days to forecast", value = 5, min = 1, max = 100),
                                   selectInput("var_which_plot","Select plot to show:",c("Forecasted period only","Full time series"),selected="Forecasted period only")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information VAR"),
                                     tabPanel("Summary statistics",
                                              tableOutput("var_summary"),
                                              plotOutput("correlation_var")
                                     ),
                                     tabPanel("Validity",
                                              #verbatimTextOutput("datensatz_var"),
                                              dygraphs::dygraphOutput("plot_forecast"),
                                              #htmlOutput("accuracy_var"),
                                              tableOutput("var_metrics"),
                                              verbatimTextOutput("serial_test"),
                                              htmlOutput("var"),
                                              #dygraphs::dygraphOutput("plot_forecast2")
                                     ),#close tabpanel validity
                                     tabPanel("Actual Forecast",
                                              verbatimTextOutput("testins"),
                                              dygraphs::dygraphOutput("plot_forecast_real"))#close tabpanel actual forecast
                                   ))),#close tabpanel VAR forecasting
                        tabPanel("XGboost-forecasting",
                                 sidebarPanel(
                                     conditionalPanel(condition="input.tabs == 'Summary statistics'",       
                                                      tabs_custom_xgb()),
                                     conditionalPanel(condition="input.tabs == 'AR & MA structure'",
                                                      radioButtons("lag_tabs","How do you want to proceed?",choices = c("default","custom"),
                                                                   selected = "default")  %>% shinyhelper::helper(type = "markdown",
                                                                                                                  title = "Inline Help",
                                                                                                                  content = "default_lag_selection",
                                                                                                                  buttonLabel = "Got it!",
                                                                                                                  easyClose = FALSE,
                                                                                                                  fade = TRUE,
                                                                                                                  size = "s"),
                                                      custom_lag_tab()
                                                      
                                                      
                                     ),
                                     conditionalPanel(condition="input.tabs == 'Validity'",       
                                                      #  numericInput("split_at","select training/test split",min = 0.1, value=0.7,max = 1,
                                                      #              step = 0.1),
                                                      radioButtons("model_spec","Choose model specification",choices = c("default","custom","hyperparameter_tuning"),
                                                                   selected = "default") %>% shinyhelper::helper(type = "markdown",
                                                                                                                title = "Inline Help",
                                                                                                                content = "model_selection",
                                                                                                                buttonLabel = "Got it!",
                                                                                                                easyClose = FALSE,
                                                                                                                fade = TRUE,
                                                                                                                size = "s"),
                                                      model_specification(),
                                                      numericInput("n_ahead","select forecast",min = 1, value=5,max = 20,
                                                                   step = 1),
                                                      radioButtons("ftpye","Select usage of features",choices = c("no_features","past_features","forecasted_features"),
                                                                   selected = "no_features") %>% shinyhelper::helper(type = "markdown",
                                                                                                                     title = "Inline Help",
                                                                                                                     content = "features",
                                                                                                                     buttonLabel = "Got it!",
                                                                                                                     easyClose = FALSE,
                                                                                                                     fade = TRUE,
                                                                                                                     size = "s"),
                                                      actionButton("run", "Run Model"),
                                                      shinyjs::hidden(p(id = "text1", "Processing...")),
                                                      actionButton("pred", "Predict"),
                                                      shinyjs::hidden(p(id = "text2", "Please run the model first")),
                                                      selectInput("forecast_plot_choice","Select plot to show:",
                                                                  c("Forecasted","Full","Feature_Importance"),selected="Full")
                                                      
                                                      
                                     ),
                                     conditionalPanel(condition="input.tabs == 'Actual forecast'", 
                                                      radioButtons("model_spec_for","Choose model specification",choices = c("default","custom","hyperparameter_tuning"),
                                                                   selected = "default"),
                                                      model_specification_for(),
                                                      numericInput("n_ahead2","select forecast window (days):",min = 1, value=5,max = 20,
                                                                   step = 1),
                                                      radioButtons("ftpye2","Select covariates for forecast",choices = c("no_features","past_features","forecasted_features"),
                                                                   selected = "forecasted_features"),
                                                      actionButton("run2", "Run Model on the full dataset"),
                                                      actionButton("pred2", "Predict"))
                                     
                                   ),
                                  mainPanel(
                                    tabsetPanel(type = "tabs", id = "tabs",
                                                tabPanel("Summary statistics",value="Summary statistics",
                                                         tableOutput("xgb_summary"),
                                                         plotOutput("correlation_xgb"),
                                                         textOutput("xgb_date_check")
                                                         # plotOutput("correlation_plot")
                                                ),
                                                tabPanel("AR & MA structure", value = "AR & MA structure",
                                                         conditionalPanel(
                                                           condition = "input.correlation_type == 'ACF' && input.lag_tabs == 'custom'",
                                                           plotOutput("acf_plot_xgb")),
                                                         conditionalPanel(
                                                           condition = "input.correlation_type == 'PACF'  && input.lag_tabs == 'custom'",
                                                           plotOutput("pacf_plot_xgb")),
                                                         conditionalPanel("input.lag_tabs == 'custom'",     
                                                                          textOutput("error_text"),
                                                                          DT::dataTableOutput("tableCustom")),
                                                         conditionalPanel("input.lag_tabs == 'default'",
                                                                          DT::dataTableOutput("df_xgb_default"))
                                                ),
                                                tabPanel("Validity", value = "Validity",
                                                        # verbatimTextOutput("model_xgb"),
                                                         tableOutput("model_fit"),
                                                        tableOutput("serial_out_xgb"),
                                                         dygraphs::dygraphOutput("forecast_xgb"),
                                                         tableOutput("xgb_metrics")
                                            ),
                                                tabPanel("Actual forecast", value = "Actual forecast",
                                                     verbatimTextOutput("model_xgb2"),
                                                     verbatimTextOutput("serial_out_xgb_for"),
                                                     dygraphs::dygraphOutput("plot_1_xgb_actual")
                                    )    
                                  )    
                                 )
                              )
                        )#close Navbarmenu
             )#close Navbarpage
  )#close fluidpage
  