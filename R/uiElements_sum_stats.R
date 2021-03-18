



### conditional sidebar panel for the time series
twitter_desc_conditional_sum_stats <- conditionalPanel(

  #condition = "input.plot_type == 'Frequency Plot'",
  # keep for both because bigram also makes senese with wordcloud
  condition = "input.tabselected==1",

  tags$hr(),
  tags$h4("Time Series"),

  ##### select a value retweets/likes etc.
  div(id = "metric_desc_instr",
      selectInput("value", "Select a value to show (multiple possible)",
                  choices = c(
                    "Sentiment" = "sentiment",
                    "Retweets Weighted Sentiment" = "sentiment_rt",
                    "Likes Weighted Sentiment" = "sentiment_likes",
                    "Length Weighted Sentiment" = "sentiment_tweet_length",
                    "Retweets" = "rt",
                    "Likes"="likes",
                    "Tweet Length" = "tweet_length"
                  ),
                  selected = "sentiment",
                  multiple = T),
      # data.step = 5,
      # data.intro = "Here you can choose a metric to analyse. Each metric is the aggreagted value
      # on a daily level. Mutiple selections are possible. As soon
      # as two metrics are selected all time series get scaled to mean 0 and a standard deviation of
      # 1 in order to fit on the same scale"
      # ),



      #### select summary statistic
      #rintrojs::introBox(
      shinyWidgets::radioGroupButtons("metric", "Select a statistic to plot",
                                      choiceNames = c("Mean", "SD", "Median"),
                                      choiceValues = c("mean", "std", "median"),

                                      status = "primary",
                                      checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon"),
                                        no = icon("remove",
                                                  lib = "glyphicon")),
                                      size = "xs"),


      shinyWidgets::awesomeCheckbox("num_tweets_box", label = "Show average number of tweets", value = F)),


  #### style checkbox
  tags$style(HTML('.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before {
    background-color: #2b3e50;
    border-color: #888888;
}')),

###### 7 day smoothin toggle
div(id = "twitter_7day",shinyWidgets::materialSwitch(inputId = "roll_twitter", label = "7 day smoothing?", value = F)),

#rintrojs::introBox(
actionButton("plot_saver_button", "Save the plot"),
#   data.step = 7,
#   data.intro = "With this button you can temporarily store a generated plot in a second field. This allows for easier
#   comparison when wanting to compare tweets with different search terms."
# )
)





###### sidebarpanel for word frequencies
word_freq_cond <- conditionalPanel(
  condition = "input.tabselected==3",


  tags$hr(),
  div(id = "emoji_instr",     tags$h4("Word controls"),

      ##### remove emoji words
      shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F) %>%
        shinyhelper::helper(type = "inline",
                            title = "",
                            content = c("During the cleaning process of the tweets we replace emojis and
                                                                emoticons with text. For exmaple a lauging emoji becomes 'laughing face'.
                                                                As many tweets contain emojis these substitution words dominate the
                                                                frequency analysis. With this button you may remove these words from
                                                                the analysis in order to focus on potentially more interesting words/bigrams."),
                            size = "m")
  ),

  ##### select plot type
  div(id = "plot_type_expl_instr",
      selectInput("plot_type_expl", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")
      )
  ),

  ####### panel for wordcloud, adjust size of wordcloud
  conditionalPanel(
    condition = "input.plot_type_expl == 'Word Cloud'",

    sliderInput("size_wordcloud", "Change the size of the wordcloud", min = 0.1, max = 10, value = 1, step = 0.1) %>%
      shinyhelper::helper(type = "inline",
                          title = "",
                          content = c("With this control you can adjust the size of wordcloud."),
                          size = "s")
  ),

  ###### panel for frequency plot, show different max value for slider input because becomes overcrowed otherwise
  conditionalPanel(
    condition = "input.plot_type_expl == 'Frequency Plot'",
    sliderInput("n_freq", "Maximum number of words to show", min = 5, max = 100, value = 20) %>%
      shinyhelper::helper(type = "inline",
                          title = "",
                          content = c("Select the Top N (according to frequency) words/bigrams to display. Note that you can
                                                                  select more when choosing the wordcloud."),
                          size = "s")

  ),
  ####### slider input for wordcloud with larger max because wordcloud can handle more
  conditionalPanel(
    condition = "input.plot_type_expl != 'Frequency Plot'",
    sliderInput("n_freq_wc", "Maximum number of words to show", min = 5, max = 500, value = 100)%>%
      shinyhelper::helper(type = "inline",
                          title = "",
                          content = c("Select the Top N (according to frequency) words/bigrams to display. Note that you can
                                                                  select more when choosing the wordcloud."),
                          size = "s")
  ),

  ####### select single words or bigrams
div(id = "ngram_sel_instr",  shinyWidgets::radioGroupButtons("ngram_sel", "Single words or bigrams?",
                                  choices = c("Unigram", "Bigram"),
                                  status = "primary",
                                  checkIcon = list(
                                    yes = icon("ok",
                                               lib = "glyphicon"),
                                    no = icon("remove",
                                              lib = "glyphicon")),
                                  size = "xs")
    ),




  # word search bigrams
  conditionalPanel(
    condition = "input.ngram_sel == 'Bigram'",
    shinyWidgets::searchInput("word_freq_filter", "Show bigrams containing specific terms",
                              placeholder = "Enter word",
                              value = "",
                              btnSearch = icon("search"),
                              btnReset = icon("remove")) %>%
      shinyhelper::helper(type = "inline",
                          title = "",
                          content = c("With this search field you can filter for bigrams containing one specific word. Note that
                                                                  as the words are stemmed (e.g. science is now scienc) we automatically stem your search term
                                                                  using the same algorithm so you don't need to worry about it. The search is also not
                                                                  case sensitive."),
                          size = "m")
  ),

)









#### sidebar layout for descriptives and word frequencies
twitter_tab_desc <- tabPanel( "Descriptives",


                              ########################################
                              ################## BOTH
                              #######################################


                              ######## instrucion buttons
                              conditionalPanel(
                                condition = "input.tabselected==1",
                                actionButton("instrucitons_desc", "Instructions")
                              ),
                              conditionalPanel(
                                condition = "input.tabselected==3",
                                actionButton("instrucitons_expl", "Instructions")
                              ),


                              tags$hr(),

                              ################################
                              ###### tweet filters
                              div(id = "tweets_all_expl",

                                  div(id = "lang_instr",  tags$h4("Tweet Controls"),


                                      ##### language selection tweets
                                      shinyWidgets::radioGroupButtons("lang", "Language of tweets",
                                                                      choices = c("English" = "EN",
                                                                                  "German" = "DE"),
                                                                      status = "primary",
                                                                      checkIcon = list(
                                                                        yes = icon("ok",
                                                                                   lib = "glyphicon"),
                                                                        no = icon("remove",
                                                                                  lib = "glyphicon")),
                                                                      size = "sm")),
                                ######## company selector tweets
                                  div(id = "comp_instr",     selectInput("comp","Choose tweets",
                                                                         company_terms,
                                                                         selected = "NoFilter")),
                                  ##### daterange selector
                                  div(id = "date_instr",   shinyWidgets::airDatepickerInput("dates_desc", "Date range:",
                                                                                            range = T,
                                                                                            value = c("2018-11-30", date_avail),
                                                                                            maxDate = date_avail, minDate = "2018-11-30",
                                                                                            clearButton = T, update_on = "close"),


                                      ####### reset date range button
                                      actionButton("reset_dates_desc", "Reset date range")),
                                  tags$br(),


                                    #########################################
                                  ####### filter min rt, likes, long tweets
                                ##########################################


                                ###### min rt
                                  div(id = "desc_filter_instr",
                                      shinyWidgets::radioGroupButtons("rt", "Minimum retweets",
                                                                      choices = c(0, 10, 50, 100, 200),
                                                                      status = "primary",
                                                                      checkIcon = list(
                                                                        yes = icon("ok",
                                                                                   lib = "glyphicon"),
                                                                        no = icon("remove",
                                                                                  lib = "glyphicon")),
                                                                      size = "xs"),

                                        ###### min likes selector
                                      shinyWidgets::radioGroupButtons("likes", "Minimum Likes",
                                                                      choices = c(0, 10, 50, 100, 200),
                                                                      status = "primary",
                                                                      checkIcon = list(
                                                                        yes = icon("ok",
                                                                                   lib = "glyphicon"),
                                                                        no = icon("remove",
                                                                                  lib = "glyphicon")),
                                                                      size = "xs"),



                                      ###### style radio buttons
                                      tags$style(HTML('.btn-primary {background-color: #4e5d6c;}')),
                                      tags$style(HTML('.btn-primary.active {background-color: #2b3e50;}')),
                                      tags$style(HTML('.btn-primary:active, .btn-primary.active, .open>.dropdown-toggle.btn-primary {
                                              background-color: #2b3e50}')),
                                      tags$style(HTML('.btn-primary:focus, .btn-primary.focus.active, .open>.dropdown-toggle.btn-primary {
                                              background-color: #2b3e50}')),
                                      tags$style(HTML('.btn-primary:hover, .btn-primary.hover, .open>.dropdown-toggle.btn-primary {
                                              background-color: #2b3e50}')),
                                      tags$style(HTML('.btn-primary:active:hover, .btn-primary.active:hover {
                                              background-color: #2b3e50}')),


                                        ###### switch button for long tweets
                                       shinyWidgets::materialSwitch(inputId = "long",
                                                                   label = "Long Tweets only?", value = F) %>%
                                        shinyhelper::helper(type = "inline",
                                                            title = "",
                                                            content = c("Long Tweets are tweets that contain more
                                                                than 80 characters"),
                                                            size = "s"))
                              ),




                              ######################################
                              ############## descirptives
                              ##################################
                              conditionalPanel(
                                condition = "input.tabselected==1",


                               # additional elemtns for time series analysis
                                twitter_desc_conditional_sum_stats,

                                ),

                              ######################################################################################
                              #####################################################################################
                              ############################### Word Frequencies ####################################
                              #####################################################################################

                              word_freq_cond
)















########################################
#################### Outputs Descriptives and Word freq
timeseries_main <- ### panel with histograms and summary table
  tabPanel("Summary Statistics", value = 1,

           #########################################
           ###########################################


           ##### style dygraphs
           ### styling of dygraphs
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
                                        .dygraph-title{
                                        font-size: 18px;
                                        }



                                                     "))
           ),



           ##### first time series
           tags$br(),
           div(id = "time_series1_instr",
               dygraphs::dygraphOutput("sum_stats_plot")%>% shinycssloaders::withSpinner(type = 5)),
           tags$br(),

           # seconds time series plot
           div(id = "time_series2_instr",
               dygraphs::dygraphOutput('sum_stats_plot2')%>% shinycssloaders::withSpinner(type = 5)),



           #########################################
           ###########################################

           #summary statistics table
           tags$head(tags$style(HTML("#sum_stats_table{
                                   color: black;
                                 font-size: 18px;
                                 font-style: bold;
                                 color: white !important;
                                 }"
           )
           ))






  )

##### main panel with wod frequency and raw tweets
word_freq_tab_main <-  tabPanel("Word Frequency Analysis", value = 3,

                                tags$br(),
                                div(id = "num_words_expl_instr",   htmlOutput("number_words")),
                                div(id = "expl_plots_instr",     conditionalPanel(
                                  condition = "input.plot_type_expl == 'Frequency Plot'",
                                  plotly::plotlyOutput("freq_plot", height = "1000px")%>% shinycssloaders::withSpinner(type = 5)
                                  #uiOutput("plot.ui")
                                ),
                                conditionalPanel(
                                  condition = "input.plot_type_expl == 'Word Cloud'",
                                  uiOutput("cloud"),
                                  #wordcloud2::wordcloud2Output('wordcloud', height = "1000px", width = "auto")
                                ))



)


########## main panels for Descritpive reiter
twitter_desc_main <- mainPanel(
  tabsetPanel(id = "tabselected",

              timeseries_main,

              word_freq_tab_main

  ))


####### input panel for histogram
histo_tab <- sidebarPanel(
  div(id = "histo_instr",
      tags$h4("Histogram"),
      selectInput("histo_value", "Which value would you like to show",
                  choices = c(
                    "Sentiment" = "sentiment",
                    #"Retweets Weighted Sentiment" = "sentiment_rt",
                    # "Likes Weighted Sentiment" = "sentiment_likes",
                    #"Length Weighted Sentiment" = "sentiment_tweet_length",
                    "Retweets" = "rt",
                    "Likes"="likes",
                    "Tweet Length" = "tweet_length"
                  ),
                  selected = "sentiment"),
      sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 100, value = 50)),


  # add switch whether to use logarithmic scale
  div(id = "log_scale_instr",
      shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                                value = F,
                                size = "mini",
                                handleWidth = 100))
)


######## output panel for histogram
histo_output_tab <- mainPanel(
  div(id = "histo_plot_instr",
      textOutput("histo_plot_info"),
      tags$head(tags$style("#histo_plot_info{
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
      )
      ),
      plotly::plotlyOutput("histo_plot") %>%
        shinycssloaders::withSpinner(type = 5))
)



### sum_stats_table
sum_stats_main <- fluidRow(column(10, offset = 1,
                                  tags$hr(),
                                  div(id = "sum_stats_table_instr",
                                      textOutput("sum_stats_table_header"),
                                      #### change font sizhe
                                      tags$head(tags$style("#sum_stats_table_header{
                                 font-size: 20px;

                                 }"
                                      )
                                      ),
                                 tags$br(),

                                 tableOutput("sum_stats_table")%>% shinycssloaders::withSpinner(type = 5)),

))

other_desc_main <-   conditionalPanel(

  condition = "input.tabselected == 1",

  ## sum stats table
  sum_stats_main,

  #### histogram
  tags$br(),
  tags$hr(),

  tags$br(),
  histo_tab,
  histo_output_tab


)




