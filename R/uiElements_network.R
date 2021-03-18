


#################################### going deeeper sidbarpanel
##### sidebarpanel of network tab
network_sidebar <- sidebarPanel(width = 4,

                                #shinyWidgets::dropdown(
                                #icon = icon("align-justify"),tooltip = tooltipOptions(title = "Set filters for network anylsis"),
                                #circle = T,

                                waiter::use_waitress(color = "#616f7d"),
                                #waiter::use_waiter(),
                                #waiter::use_hostess(),
                                #waiter::hostess_loader("load", text_color = "white", center_page = TRUE),

                                #### instructions button
                                actionButton("net_instr", "Instructions"),
                                ###### language selector
                                tags$hr(),
                                div(id = "tweet_net_instr",  tags$h4("Tweet controls"),
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
                                 sliderInput("rt_net", "Choose a minimum number of retweets", min = 0, max = 1000, value = 0,
                                             step = 1),

                                 # likes count
                                 sliderInput("likes_net", "Choose a minimum number of likes", min = 0, max = 1000, value = 0,
                                              step = 1),

                                 # long tweets switch
                                 shinyWidgets::materialSwitch(inputId = "long_net", label = "Long Tweets only?", value = F)  %>%
                                   shinyhelper::helper(type = "inline",
                                                       title = "",
                                                       content = c("Long Tweets are tweets that contain more
                                                                than 80 characters"),
                                                       size = "s")



                                ),
                                ### filter by sentiment
                                #shinyWidgets::setSliderColor(c("red"), c(1)),
                                div(id = "sentiment_net_instr", sliderInput("sentiment_net", label = h5("Choose a sentiment range"),
                                                                            min = -1, max = 1, value = c(-1, 1), step = 0.01, dragRange = T)

                                ),

                                # filter out emoji words
                                div(id = "emoji_net_instr",   shinyWidgets::materialSwitch(inputId = "emo_net", label = "Remove Emoji Words?", value = F) %>%
                                      shinyhelper::helper(type = "inline",
                                                          title = "",
                                                          content = c("During the cleaning process of the tweets we replace emojis and
                                                                emoticons with text. For exmaple a lauging emoji becomes 'laughing face'.
                                                                As many tweets contain emojis these substitution words dominate the
                                                                frequency analysis. With this button you may remove these words from
                                                                the analysis in order to focus on potentially more interesting words/bigrams."),
                                                          size = "m")

                                ),






                                ##### additional
                                ######### search terms
                                div(id = "search_net_instr",      textInput("search_term_net", "Only select tweets containing the following:"),
                                    textInput("username_net", "Only show tweets for usernames containing the following:")
                                ),
                                #### optional text in case less than 4 chars input to username
                                textOutput("username_checker"),
                                tags$head(tags$style("#username_checker{color: red;

                                         }"
                                )),


                                ####### type of plot bigram/word pairs
                                div(id = "net_type_instr",     shinyWidgets::radioGroupButtons("word_type_net",
                                                                                               "Select the type of word combination you would like to analyse",
                                                                                               choices = c("Bigram" = "bigrams_net",
                                                                                                           "Word Pairs" = "word_pairs_net"),
                                                                                               status = "primary",
                                                                                               checkIcon = list(
                                                                                                 yes = icon("ok",
                                                                                                            lib = "glyphicon"),
                                                                                                 no = icon("remove",
                                                                                                           lib = "glyphicon")),
                                                                                               size = "xs")
                                ),

                                ##### advanced settings toggle
                                div(id = "adv_settings_net_instr",    shinyWidgets::materialSwitch("adv_settings_net", "Advanced Settings", value = F)),
                                conditionalPanel(
                                  condition = "input.adv_settings_net == true",
                                  ######## adjusting plot
                                  numericInput("n_net", "Minimum number of occurences of a single word in the sample",
                                               min = 10, value = 50) %>%
                                    shinyhelper::helper(type = "inline",
                                                        title = "",
                                                        content = c("This sets the minimum number of times a single word needs to occur within
                                                                    the filtered dataset. The minimum threshold is 10 or 0.1% of the numebr of tweets.
                                                                    Values below these thresholds will default to the threshold for the normal
                                                                    search. For the username search the values can be set as low as 1."),
                                                        size = "s"),




                                  ### panel in case of word pairs to compute word correlations
                                  conditionalPanel(
                                    condition = "input.word_type_net == 'word_pairs_net'",
                                    numericInput("corr_net", "Minimum word correlation of word pairs", value = 0.15, min = 0.15, max = 1,
                                                 step = 0.01) %>%
                                      shinyhelper::helper(type = "inline",
                                                          title = "",
                                                          content = c("This sets the minimum correlation two words need to have.
                                                                       Values below 0.15 will default to 0.15. For username search this
                                                                      minimum threshold is at 0.1."),
                                                          size = "s")

                                  ),


                                  ### panel for minium number of time bigrams arise
                                  conditionalPanel(
                                    condition = "input.word_type_net == 'bigrams_net'",
                                    numericInput("n_bigrams_net", "Minimum number of occurences of a Bigram in the selected sample",
                                                 min = 50, value = 50)%>%
                                      shinyhelper::helper(type = "inline",
                                                          title = "",
                                                          content = c("This sets the minimum number of times a bigram needs to occur within
                                                                    the filtered dataset. The minimum threshold is 10 or 0.1% of the numebr of tweets.
                                                                    Values below these thresholds will default to the threshold for the normal
                                                                    search. For the username search the values can be set as low as 1."),
                                                          size = "s"),
                                  )
                                ),



                                div(id = "buttons_net_instr",    actionButton("button_net", "Render Plot"),
                                    # shinyhelper::helper(type = "markdown",
                                    #                     title = "Inline Help",
                                    #                     content = "network_plot_button",
                                    #                     buttonLabel = "Got it!",
                                    #                     easyClose = FALSE,
                                    #                     fade = TRUE,
                                    #                     size = "s"),

                                    #### removing plot
                                    shinyjs::disabled(actionButton("reset_net", "Remove Network")),


                                    ### canceling computation
                                    shinyjs::disabled(actionButton("cancel_net", "Cancel Rendering"))

                                )

)
#)

####### datatable output panel
data_table_net_main <-  fluidRow(column(12,

                                        tags$br(),
                                        tags$h4("Tweet data for current selection"),
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
                   div(id = "data_table_instr",    DT::dataTableOutput("raw_tweets_net"))
)

)



##### network plot otuput panel
network_tab_main <- tabsetPanel(id = "network_analysis",
                                tabPanel("Description", value = 1,
                                         tags$br(),
                                         htmlOutput("network_description")
                                ),
                                tabPanel("Network Analysis", value = 2,
                                         tags$br(),

                                         tags$br(),
                                         network_sidebar,
                                         mainPanel(

                                           div(id = "num_tweets_info_net_instr",   textOutput("number_tweets_net")),
                                           tags$head(tags$style("#number_tweets_net{
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
                                           )
                                           ),
                                 tags$div(id = "placeholder", style = "height: 1000px; width: 100%;")
                                         ),
                                 tags$br(),
                                 tags$br(),

                                 data_table_net_main




                                ))



