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
             shinyFiles::shinyDirButton("directory", "Select folder", "Please select a folder"
             ),
             ## in case dir path chooser not working enter manually
             tags$br(),
             tags$br(),
             tags$p("In the case that choosing a path through the 'Select Folder' button \
             is not possible you can also enter your path manually"),
             textInput("dir_path_man", ""),
             actionButton("dir_path_man_btn", "Set path")

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
                        twitter_tab_desc,

                      ),






                      ########## main panels for Descritpive reiter
                       mainPanel(
                        tabsetPanel(id = "tabselected",

                          ### panel with histograms and summary table
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
                                   dygraphs::dygraphOutput("sum_stats_plot")%>% shinycssloaders::withSpinner(type = 5),
                                    tags$br(),

                                    # seconds time series plot
                                   dygraphs::dygraphOutput('sum_stats_plot2')%>% shinycssloaders::withSpinner(type = 5),



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






                                   ),


                          ##### main panel with wod frequency and raw tweets
                          tabPanel("Exploratory Output", value = 3,

                                   tags$br(),
                                   htmlOutput("number_words"),
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Frequency Plot'",
                                        plotly::plotlyOutput("freq_plot", height = "1000px")%>% shinycssloaders::withSpinner(type = 5)
                                       #uiOutput("plot.ui")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Word Cloud'",
                                       uiOutput("cloud"),
                                       #wordcloud2::wordcloud2Output('wordcloud', height = "1000px", width = "auto")
                                     )
                                  # tags$hr(),
                                  # tags$br(),
                                  # tags$br(),
                                  # tags$br(),
                                  # tags$br(),
                                  # tags$br(),
                                  # tags$hr(),
                                  # conditionalPanel(
                                  #   condition = "input.ngram_sel == 'Bigram'",
                                  #   tags$h4("Number of Bigrams containing the choosen word (if no word selected shows all tweets in current selection)"),
                                  # plotly::plotlyOutput("word_freq_time_series") %>% shinycssloaders::withSpinner(type = 5)
                                  # )


                                   )
                                    )),
                      ##### histogram

                      tags$hr(),
                      conditionalPanel(

                        condition = "input.tabselected == 1",
                        tags$hr(),
                        fluidRow(column(10, offset = 1,
                                        tags$hr(),

                                        textOutput("sum_stats_table_header"),
                                        #### change font sizhe
                                        tags$head(tags$style("#sum_stats_table_header{
                                 font-size: 20px;

                                 }"
                                        )
                                        ),
                                 tags$br(),
                                        tableOutput("sum_stats_table")%>% shinycssloaders::withSpinner(type = 5),

                        )),

                        #########################################
                        ###########################################
#
#                         ##### violin plot
#                       fluidRow(column(10, offset = 1,
#                                       tags$h4("Distrubtion of aggregated tweets"),
#                                       plotOutput("violin_sum")%>% shinycssloaders::withSpinner(type = 5))),
#                         tags$br(),
#                       tags$hr(),
                        #########################################
                        ###########################################
                      tags$hr(),

                      tags$br(),
                      histo_tab,
                      histo_output_tab


                      )

                    ),
             ################### tab panel descirptive end




             # andere reiter
             tabPanel("Going Deeper",
                     # sidebarPanel(
                        network_sidebar,
                      tags$style(HTML('#sw-content-dropdown, .sw-dropdown-in {background-color: #4e5d6c;
                                      margin: 0 0;}'))

                     # )
                      ,
                      #mainPanel(
                      # conditionalPanel(
                      #   condition = "input.button_net == 0",
                      #   shinyjs::hidden(div(id = "loading",
                      #                       networkD3::forceNetworkOutput("network_plot_placeholder")
                      #
                      # ),
                     # mainPanel(
                        tabsetPanel(id = "network_analysis",
                                    tabPanel("Description", value = 1,
                                             tags$br(),
                                             htmlOutput("network_description")
                                            ),
                                    tabPanel("Network Analysis", value = 2,
                      tags$br(),
                      tags$h4("Word Network Analysis", color = "white"),
                      tags$br(),
                      tags$div(id = "placeholder", style = "height: 800px; width: 90%;"),
                      tags$br(),
                      tags$br(),
                     # ),

                      ###### datatable
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
                     tags$br()
                     )),

                      fluidRow(column(12,
                                      textOutput("number_tweets_net"),
                                      tags$head(tags$style("#number_tweets_net{
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
                                      )
                                      ),
                                      tags$br(),
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
                                      DT::dataTableOutput("raw_tweets_net")
                                      )

                               )

                     )

             )

}




### conditional sidebar panel for the time series
twitter_desc_conditional_sum_stats <- conditionalPanel(

  #condition = "input.plot_type == 'Frequency Plot'",
  # keep for both because bigram also makes senese with wordcloud
  condition = "input.tabselected==1",

  tags$hr(),
  tags$h4("Time Series"),

  ##### select a value retweets/likes etc.
  rintrojs::introBox(
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
  data.step = 5,
  data.intro = "Here you can choose a metric to analyse. Each metric is the aggreagted value
  on a daily level. Mutiple selections are possible. As soon
  as two metrics are selected all time series get scaled to mean 0 and a standard deviation of
  1 in order to fit on the same scale"
  ),



  #### select summary statistic
  rintrojs::introBox(
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


  shinyWidgets::awesomeCheckbox("num_tweets_box", label = "Show average number of tweets", value = F),
  data.step = 6,
  data.intro = "You may choose to see the to depict the means, standard deviations or the medians aggreagted
  on a daily level"
  ),

 #### style checkbox
 tags$style(HTML('.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before {
    background-color: #2b3e50;
    border-color: #888888;
}')),

rintrojs::introBox(
  actionButton("plot_saver_button", "Save the plot"),
  data.step = 7,
  data.intro = "With this button you can temporarily store a generated plot in a second field. This allows for easier
  comparison when wanting to compare tweets with different search terms."
)
)




####### input panel for histogram
histo_tab <- sidebarPanel(
  tags$h3("Histogram"),
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
  sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 100, value = 50),


  # add switch whether to use logarithmic scale
  shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                            value = F,
                            size = "mini",
                            handleWidth = 100)
)


######## output panel for histogram
histo_output_tab <- mainPanel(
  textOutput("histo_plot_info"),
  tags$head(tags$style("#histo_plot_info{
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
  )
  ),
  plotly::plotlyOutput("histo_plot") %>%
    shinycssloaders::withSpinner(type = 5)
)

date_avail <- as.character(Sys.Date() - lubridate::days(1))

#### sidebar layout for descriptives
twitter_tab_desc <- tabPanel( "Descriptives",


                              ####### all three
                              ### instrucitons button

                              conditionalPanel(
                                condition = "input.tabselected==1",
                              actionButton("instrucitons_desc", "Instructions")
                              ),


                              tags$hr(),
                              tags$h4("Filter Tweets"),

                              ###### langauge of tweets selector
                              rintrojs::introBox(
                              shinyWidgets::radioGroupButtons("lang", "Language of tweets",
                                                              choices = c("English" = "EN",
                                                                          "German" = "DE"),
                                                              status = "primary",
                                                              checkIcon = list(
                                                                yes = icon("ok",
                                                                           lib = "glyphicon"),
                                                                no = icon("remove",
                                                                          lib = "glyphicon")),
                                                              size = "sm"),
                                                              data.step = 1,
                                                              data.intro = "Here you can set the language of the tweets"
                                                                  ),



                              ###### choose tweet type (company or unfiltererd)
                              rintrojs::introBox(
                              selectInput("comp","Choose tweets",
                                             company_terms,
                                             selected = "NoFilter"),
                              data.step = 2,
                              data.intro = "Here you can either select company specific tweets or tweets that were randomly scraped
                              without any search term"
                              ),

                              ####### select date range
                              rintrojs::introBox(
                              shinyWidgets::airDatepickerInput("dates_desc", "Date range:",
                                                               range = T,
                                                               value = c("2018-11-30", date_avail),
                                                               maxDate = date_avail, minDate = "2018-11-30",
                                                               clearButton = T, update_on = "close"),
                              data.step = 3,
                              data.intro = "Here you can select the date range you would like to analyse. We have data from
                              2018-11-30 until yesterday"
                              ),

                              ####### reset date range button
                              actionButton("reset_dates_desc", "Reset date range"),
                              tags$br(),



                              ####### filter min rt, likes, long tweets
                              rintrojs::introBox(
                              shinyWidgets::radioGroupButtons("rt", "Minimum tweets",
                                           choices = c(0, 10, 50, 100, 200),
                                           status = "primary",
                                           checkIcon = list(
                                             yes = icon("ok",
                                                        lib = "glyphicon"),
                                             no = icon("remove",
                                                       lib = "glyphicon")),
                                           size = "xs") %>%
                                shinyhelper::helper(type = "inline",
                                                    title = "",
                                                    content = c("Choose the minimum number of retweets
                                                    a tweet needs to have"),
                                                    size = "s"),

                              shinyWidgets::radioGroupButtons("likes", "Minimum Likes",
                                           choices = c(0, 10, 50, 100, 200),
                                           status = "primary",
                                           checkIcon = list(
                                             yes = icon("ok",
                                                        lib = "glyphicon"),
                                             no = icon("remove",
                                                       lib = "glyphicon")),
                                           size = "xs") %>%
                                shinyhelper::helper(type = "inline",
                                                    title = "",
                                                    content = c("Choose the minimum number of likes
                                                                a tweet needs to have"),
                                                    size = "s"),


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







                              #switchInput(inputId = "long", value = TRUE),
                              shinyWidgets::materialSwitch(inputId = "long",
                                                           label = "Long Tweets only?", value = F) %>%
                                shinyhelper::helper(type = "inline",
                                                    title = "",
                                                    content = c("Long Tweets are tweets that contain more
                                                                than 80 characters"),
                                                    size = "s"),
                              data.step = 4,
                              data.intro = "In this section you can apply filters to the tweets. You may choose to only see tweets
                              with a minimum number of retweets or likes or only tweets that have at least 80 characters"
                              ),



                              ##### only descr
                              conditionalPanel(
                                condition = "input.tabselected==1",






                                # additional elemtns for time series analysis
                                twitter_desc_conditional_sum_stats,






                              ),
                            ######################################################################################
                            #####################################################################################
                            ############################### Word Frequencies ####################################
                            #####################################################################################
                            conditionalPanel(
                              condition = "input.tabselected==3",

                              ##### remove emoji words
                              shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F) %>%
                                shinyhelper::helper(type = "inline",
                                                    title = "",
                                                    content = c("During the cleaning process of the tweets we replace emojis and
                                                                emoticons with text. For exmaple a lauging emoji becomes 'laughing face'.
                                                                As many tweets contain emojis these substitution words dominate the
                                                                frequency analysis. With this button you may remove these words from
                                                                the analysis in order to focus on potentially more interesting words/bigrams."),
                                                    size = "m"),

                              ##### select plot type
                              selectInput("plot_type_expl", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")
                                          ),

                              ####### panel for wordcloud, adjust size of wordcloud
                              conditionalPanel(
                                condition = "input.plot_type_expl == 'Word Cloud'",
                                sliderInput("size_wordcloud", "Change the size of the wordcloud", min = 0.1, max = 10, value = 1, step = 0.1)
                              ),

                              ###### panel for frequency plot, show different max value for slider input because becomes overcrowed otherwise
                              conditionalPanel(
                                condition = "input.plot_type_expl == 'Frequency Plot'",
                                sliderInput("n_freq", "Number of words to show", min = 5, max = 100, value = 20)

                              ),
                              ####### slider input for wordcloud with larger max because wordcloud can handle more
                              conditionalPanel(
                                condition = "input.plot_type_expl != 'Frequency Plot'",
                                sliderInput("n_freq_wc", "Number of words to show", min = 5, max = 1000, value = 100)
                              ),

                              ####### select single words or bigrams
                              shinyWidgets::radioGroupButtons("ngram_sel", "Single words or bigrams?",
                                             choices = c("Unigram", "Bigram"),
                                             status = "primary",
                                             checkIcon = list(
                                               yes = icon("ok",
                                                          lib = "glyphicon"),
                                               no = icon("remove",
                                                         lib = "glyphicon")),
                                             size = "xs"),




                              # word search bigrams
                              conditionalPanel(
                                condition = "input.ngram_sel == 'Bigram'",
                                shinyWidgets::searchInput("word_freq_filter", "Show bigrams containing specific terms",
                                                          placeholder = "Placeholder",
                                                          value = "",
                                                          btnSearch = icon("search"),
                                                          btnReset = icon("remove"))
                              ),

                            )

                            )




#################################### going deeeper sidbarpanel
#tabPanel( "Network Analysis",
network_sidebar <- shinyWidgets::dropdown(
  icon = icon("align-justify"),tooltip = tooltipOptions(title = "Set filters for network anylsis"),
  circle = T,

          waiter::use_waitress(color = "#616f7d"),
          #waiter::use_waiter(),
          #waiter::use_hostess(),
          #waiter::hostess_loader("load", text_color = "white", center_page = TRUE),


          ###### language selector
  shinyWidgets::radioGroupButtons("lang_net", "Select Language", choiceNames = c("English Tweets", "German Tweets"),
                       choiceValues = c("en", "de"),
                       status = "primary",
                       checkIcon = list(
                         yes = icon("ok",
                                    lib = "glyphicon"),
                         no = icon("remove",
                                   lib = "glyphicon")),
                       size = "s"),
          # company selector
          selectInput("comp_net","Choose tweets for",
                      company_terms,
                      selected = "NoFilter"),

          # datepicker
          shinyWidgets::airDatepickerInput("dates_net", "Select a maximum of 2 days",
                                           range = TRUE,
                                           value = "2018-11-30",
                                           maxDate = date_avail, minDate = "2018-11-30",
                                           clearButton = T, update_on = "close",
                                           multiple = 5),
          textOutput("date_checker_net"),
  tags$head(tags$style("#date_checker_net{color: red;

                                 }"
  )
  ),

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
          #shinyWidgets::setSliderColor(c("red"), c(1)),
          sliderInput("sentiment_net", label = h5("Choose a sentiment range"),
                      min = -1, max = 1, value = c(-1, 1), step = 0.01, dragRange = T),




          ##### additional
          ######### search terms
          textInput("search_term_net", "Only select tweets containing the following:"),
          textInput("username_net", "Only show tweets for usernames containing the following:"),

      conditionalPanel(
        condition = "input.network_analysis == 2",
        ####### type of plot bigram/word pairs
        shinyWidgets::radioGroupButtons("word_type_net",
                                        "Select the type of word combination you would like to analyse",
                                        choices = c("Bigram" = "bigrams_net",
                                                    "Word Pairs" = "word_pairs_net"),
                                        status = "primary",
                                        checkIcon = list(
                                          yes = icon("ok",
                                                     lib = "glyphicon"),
                                          no = icon("remove",
                                                    lib = "glyphicon")),
                                        size = "xs"),



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
        actionButton("reset_net", "Remove Network"),


        ### canceling computation
        shinyjs::disabled(actionButton("cancel_net", "Cancel Rendering"))

      )

          )
#)


#############################################################################
################### database
############################################################################
### connect to database






# #### word freq tab
# tab_panel_twitter_expl <-
# )



Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(



  #### this gets the dimension of the current window --> helps adjusting width and height of plots that
  # dont do it automatically
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  rintrojs::introjsUI(),

  #### change color of instrucitons boxes
 tags$style(HTML(".introjs-tooltip {
    background-color: #1d465a;}"
 )),


  theme = shinythemes::shinytheme("superhero"),
  ### change button colors
  tags$style(HTML('.btn-default {
  background-color: #2b3e50;
  border-color: #888888;
                                          }')),
  #shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("DSP",

             dir_setter_panel(),
             twitter_main_panel(),

              tabPanel("Comparison",
                       tags$br(),
                       tags$br(),
                       tags$br(),
                      fluidRow(column(4,
                        ######## stocks
                        wellPanel(tags$h4("Stocks"),

                        #selectize_Stocks(COMPONENTS_DE()),
                        selectInput("stocks_comp", "Select a company or index",
                                    company_terms_stock, multiple = T,
                                    selected = "AAPL"),
                        #selectizeInput("stock_choice", choices = "Platzhalter"),
                        shinyWidgets::radioGroupButtons("stocks_metric_comp","Which variable?",c("Return","Adj.Close" = "Adj.Close"),
                                     selected = "Return",
                                     status = "primary",
                                     checkIcon = list(
                                       yes = icon("ok",
                                                  lib = "glyphicon"),
                                       no = icon("remove",
                                                 lib = "glyphicon")),
                                     size = "s"),

                       # actionButton("reset", "clear selected"),
                        shinyWidgets::materialSwitch(inputId = "roll_stock_comp", label = "7 day smoothing?", value = F)
                       )),
                      column(8,
                        dygraphs::dygraphOutput("stocks_comp")
                      )),





                    tags$br(),

                      fluidRow(column(4,
                        ####### twitter
                    wellPanel(tags$h4("Twitter"),

                        tags$hr(),
                        tags$h4("Filter Tweets"),

                        ###### langauge of tweets selector
                        shinyWidgets::radioGroupButtons("lang_comp", "Language of tweets",
                                                        choices = c("English" = "EN",
                                                                    "German" = "DE"),
                                                        status = "primary",
                                                        checkIcon = list(
                                                          yes = icon("ok",
                                                                     lib = "glyphicon"),
                                                          no = icon("remove",
                                                                    lib = "glyphicon")),
                                                        size = "sm"),
                        ###### choose tweet type (company or unfiltererd)
                        selectInput("twitter_comp_comp","Choose tweets",
                                    company_terms,
                                    selected = "NoFilter"),


                        ####### filter min rt, likes, long tweets
                        shinyWidgets::radioGroupButtons("rt_comp", "Minimum tweets",
                                                        choices = c(0, 10, 50, 100, 200),
                                                        status = "primary",
                                                        checkIcon = list(
                                                          yes = icon("ok",
                                                                     lib = "glyphicon"),
                                                          no = icon("remove",
                                                                    lib = "glyphicon")),
                                                        size = "xs") %>%
                          shinyhelper::helper(type = "inline",
                                              title = "",
                                              content = c("Choose the minimum number of retweets
                                                    a tweet needs to have"),
                                              size = "s"),


                      ####### minimum likes
                        shinyWidgets::radioGroupButtons("likes_comp", "Minimum Likes",
                                                        choices = c(0, 10, 50, 100, 200),
                                                        status = "primary",
                                                        checkIcon = list(
                                                          yes = icon("ok",
                                                                     lib = "glyphicon"),
                                                          no = icon("remove",
                                                                    lib = "glyphicon")),
                                                        size = "xs") %>%
                          shinyhelper::helper(type = "inline",
                                              title = "",
                                              content = c("Choose the minimum number of likes
                                                                a tweet needs to have"),
                                              size = "s"),

                        ####### long tweet switch
                        shinyWidgets::materialSwitch(inputId = "long_comp",
                                                     label = "Long Tweets only?", value = F) %>%
                          shinyhelper::helper(type = "inline",
                                              title = "",
                                              content = c("Long Tweets are tweets that contain more
                                                                than 80 characters"),
                                              size = "s"),
                      ##### select a value retweets/likes etc.
                      selectInput("value_comp", "Select a value to show (multiple possible)",
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
                      shinyWidgets::materialSwitch(inputId = "roll_twitter_comp", label = "7 day smoothing?", value = F)

                      )),

                      column(8,
                        dygraphs::dygraphOutput("twitter_comp")
                      )),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    ######## covid
                    fluidRow(column(4,

                                    wellPanel(tags$h4("COVID-19"),
                                              selectize_corona(),
                                              selectInput("CoronaCountry","Country",c("Germany","USA" = "United States"),selected = "United States",
                                                          multiple = T),
                                              shinyWidgets::materialSwitch(inputId = "roll_covid_comp", label = "7 day smoothing?", value = F)
                                    )),

                             column(8,
                                    dygraphs::dygraphOutput("covid_comp")
                             ))
             ),#close tabPanel Corona








             navbarMenu("Model",
                        tabPanel("Granger",
                                 sidebarPanel(
                                   tabs_custom_gra()
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information Granger",
                                              htmlOutput("info_granger"),
                                              withMathJax()),
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
                                               tabPanel("Information Regression",
                                                        htmlOutput("info_regression"),
                                                        withMathJax()),
                                               tabPanel("Summary Statistics",
                                                        tableOutput("reg_summary"),
                                                        plotOutput("correlation_reg")
                                               ),
                                               tabPanel("Linear Regression",
                                                        htmlOutput("regression_equation"),
                                                        verbatimTextOutput("regression_result")),
                                               tabPanel("Quantile Regression",value=1,
                                                        verbatimTextOutput("regression_result_Qreg")

                                               )
                                   )
                                 )
                        ),
                        tabPanel("VAR-forecasting",
                                 sidebarPanel(
                                   tabs_custom_var()
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information VAR",
                                              htmlOutput("info_var"),
                                              withMathJax()),
                                     tabPanel("Summary Statistics",
                                              tableOutput("var_summary"),
                                              plotOutput("correlation_var")
                                     ),
                                     tabPanel("Validity",
                                              dygraphs::dygraphOutput("plot_forecast"),
                                              tableOutput("var_metrics"),
                                              verbatimTextOutput("serial_test"),
                                              htmlOutput("var"),
                                     ),
                                     tabPanel("Actual Forecast",
                                              dygraphs::dygraphOutput("plot_forecast_real"),
                                              verbatimTextOutput("serial_test_real"),
                                              htmlOutput("var_real"))
                                   ))),#close tabpanel VAR forecasting
                        tabPanel("XGboost-forecasting",
                                 sidebarPanel(
                                   conditionalPanel(condition="input.tabs == 'Summary statistics'",
                                                    tabs_custom_xgb()),
                                   conditionalPanel(condition="input.tabs == 'AR & MA structure'",
                                                    radioButtons("lag_tabs","How do you want to generate your dataset?",choices = c("default","custom"),
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
                                                                c("Forecasted","Full"),selected="Full")


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
                                                    shinyjs::hidden(p(id = "text1_act", "Processing...")),
                                                    actionButton("pred2", "Predict")),
                                                    shinyjs::hidden(p(id = "text2_act", "Please run the model first")),


                                 ),
                                 mainPanel(
                                   tabsetPanel(type = "tabs", id = "tabs",
                                               tabPanel("Summary statistics",value="Summary statistics",
                                                        tableOutput("xgb_summary"),
                                                        plotOutput("correlation_xgb")

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
                                                        #verbatimTextOutput("model_xgb"),
                                                        tableOutput("model_fit")%>% shinycssloaders::withSpinner(type = 5),
                                                        tableOutput("serial_out_xgb"),
                                                        htmlOutput("test_text_xgb"),
                                                        dygraphs::dygraphOutput("forecast_xgb")%>% shinycssloaders::withSpinner(type = 5),
                                                        tableOutput("xgb_metrics")
                                               ),
                                               tabPanel("Actual forecast", value = "Actual forecast",
                                                        tableOutput("model_fit_act")%>% shinycssloaders::withSpinner(type = 5),
                                                        tableOutput("serial_out_xgb_for"),
                                                        htmlOutput("test_text_xgb"),
                                                        dygraphs::dygraphOutput("plot_1_xgb_actual")

                                               )
                                   )
                                 )
                        )
              )#close Navbarmenu
  )#close Navbarpage
)#close fluidpage








