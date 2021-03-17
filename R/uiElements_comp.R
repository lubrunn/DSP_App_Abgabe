
######## stocks input and output panel
stocks_comp_tab <- fluidRow(column(4,
                                   ######## stocks
                                   wellPanel(
                                     div(style="display: inline-block;",tags$h4("Stocks")),
                                     div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")),
                                     actionButton("instructions_comp", "Instructions"),
                                     tags$hr(),

                                     div(id = "stock_instr",  selectInput("stocks_comp", "Select a company or index",
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
                                                                         size = "s")),


                                     # actionButton("reset", "clear selected"),
                                     div(id = "stock_roll_instr",
                                         shinyWidgets::materialSwitch(inputId = "roll_stock_comp", label = "7 day smoothing?", value = F))
                                   )),
                            column(8,
                                   div(id = "stocks_comp_plot_instr",
                                       dygraphs::dygraphOutput("stocks_comp"))
                            ))

###### twitter input and output panel of compariosn tab
twitter_comp_tab <-fluidRow(column(4,
                                   ####### twitter
                                   div(id = "twitter_comp_instr",
                                       wellPanel(tags$h4("Twitter"),

                                                 tags$hr(),
                                                 tags$h4("Tweet controls"),

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

                                       ))),

                            ##### otuput

                            column(8,
                                   div(id = "twitter_comp_plot_instr",
                                       dygraphs::dygraphOutput("twitter_comp")
                                   )
                            ))

####### control variables input and output tab
control_comp_tab <- fluidRow(column(4,

                                    wellPanel(tags$h4("Control Variables"),
                                              div(id = "covid_comp_instr",
                                                  selectInput("controls_comp", "Select a control variable",
                                                              choices =  controls_comp_list,
                                                              selected = "new_cases_per_million"),
                                                  selectInput("ControlCountry","Country",c("Germany","USA" = "United States"),selected = "United States",
                                                              multiple = T),
                                                  shinyWidgets::materialSwitch(inputId = "roll_control_comp", label = "7 day smoothing?", value = F)
                                              ))),

                             column(8,
                                    div(id = "covid_plot_comp_instr",
                                        dygraphs::dygraphOutput("covid_comp"))
                             ))

comp_tab <- tabPanel("Comparison",

                     tags$br(),
                     stocks_comp_tab,





                     tags$br(),

                     twitter_comp_tab,
                     tags$br(),


                     ######## covid
                     control_comp_tab
)
