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

  #######
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  rintrojs::introjsUI(),
  cicerone::use_cicerone(),

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

             ##### directory tab
             dir_setter_panel(),
             ##### twitter
             twitter_main_panel(),
              ##### comparison tab
             comp_tab,

             ####### model tabs
              navbarMenu("Model",
                        tabPanel("Granger",
                                 sidebarPanel(
                                   tabs_custom_gra()
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information Granger",
                                              tags$hr(),
                                              htmlOutput("info_granger"),
                                              withMathJax()),
                                     tabPanel("Visualize",
                                              tags$hr(),
                                              dygraphs::dygraphOutput("stocks_granger"),
                                              tags$hr(),
                                              dygraphs::dygraphOutput("second_granger")),
                                     tabPanel("Background-steps",
                                              tags$hr(),
                                              htmlOutput("grangertext1"),
                                              verbatimTextOutput("optimallags"),
                                              tags$hr(),
                                              htmlOutput("grangertext2"),
                                              verbatimTextOutput("dickey_fuller"),
                                              verbatimTextOutput("dickey_fuller_second"),
                                              tags$hr(),
                                              htmlOutput("grangertext3"),
                                              verbatimTextOutput("dickey_fuller_diff"),
                                              verbatimTextOutput("dickey_fuller_second_diff")),
                                     tabPanel("Results",
                                              tags$hr(),
                                              verbatimTextOutput("granger_result"),
                                              htmlOutput("granger_satz"),
                                              tags$head(tags$style(HTML("#granger_satz{

                                                 font-size: 22px;
                                                 font-style: bold;
                                                 color: white !important;}")))
                                     )))),
                        tabPanel("Regression Analysis",
                                 sidebarPanel(
                                   tabs_custom()
                                 ),
                                 mainPanel(
                                   tabsetPanel(id = "regressiontabs",
                                               tabPanel("Information Regression",
                                                        tags$hr(),
                                                        htmlOutput("info_regression"),
                                                        withMathJax()),
                                               tabPanel("Summary Statistics",
                                                        tags$hr(),
                                                        "Summary Statistics",
                                                        tableOutput("reg_summary"),
                                                        tags$head(tags$style(HTML("#reg_summary{
                                   color: black;
                                 font-size: 18px;
                                 font-style: bold;
                                 color: white !important;
                                 }"
                                                        )
                                                        )),
                                                        plotOutput("correlation_reg")
                                               ),
                                               tabPanel("Linear Regression",
                                                        tags$hr(),
                                                        htmlOutput("regression_equation"),
                                                        tags$hr(),
                                                        tableOutput("regression_result")),
                                               tabPanel("Quantile Regression",value=1,
                                                        tags$hr(),
                                                        tableOutput("regression_result_Qreg")

                                               )
                                   )
                                 )
                        ),
                        tabPanel("VAR-forecasting",
                                 sidebarPanel(
                                   tabs_custom_var()
                                 ),
                                 mainPanel(
                                   tabsetPanel(id = "vartabs",
                                               tabPanel("Information VAR",
                                                        tags$hr(),
                                                        htmlOutput("info_var"),
                                                        withMathJax()),
                                               tabPanel("Summary Statistics",
                                                        tags$hr(),
                                                        "Summary Statistics",
                                                        tableOutput("var_summary"),
                                                        plotOutput("correlation_var")
                                               ),
                                               tabPanel("Validity",
                                                        tags$hr(),
                                                        dygraphs::dygraphOutput("plot_forecast"),
                                                        tags$hr(),
                                                        "Performance Metrics:",
                                                        tableOutput("var_metrics"),
                                                        tags$hr(),
                                                        verbatimTextOutput("serial_test"),
                                                        htmlOutput("var"),value=1,
                                               ),
                                               tabPanel("Actual Forecast",
                                                        tags$hr(),
                                                        dygraphs::dygraphOutput("plot_forecast_real"),
                                                        tags$hr(),
                                                        verbatimTextOutput("serial_test_real"),
                                                        htmlOutput("var_real"))
                                   ))),#close tabpanel VAR forecasting
                        tabPanel("XGboost-forecasting",
                                 sidebarPanel(
                                   conditionalPanel(condition="input.tabs == 'Summary statistics'",
                                                    tabs_custom_xgb()),
                                   conditionalPanel(condition="input.tabs == 'AR & MA structure'",
                                                    tags$h4("Generate features:"),
                                                    shinyWidgets::radioGroupButtons("lag_tabs","Choose type of feature generation?",choices = c("default","custom"),
                                                                                    selected = "default",status = "primary",checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                                                                                                             no = icon("remove",lib = "glyphicon")),size = "sm")  %>% shinyhelper::helper(type = "markdown",
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
                                                    tags$h4("Set up model:"),
                                                    shinyWidgets::radioGroupButtons("model_spec","Choose model specification",choices = c("default","custom","hyperparameter tuning"="hyperparameter_tuning"),
                                                                                    selected = "default",status = "primary",checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                                                                                                             no = icon("remove",lib = "glyphicon")),size = "sm") %>% shinyhelper::helper(type = "markdown",
                                                                                                                                                                                                                         title = "Inline Help",
                                                                                                                                                                                                                         content = "model_selection",
                                                                                                                                                                                                                         buttonLabel = "Got it!",
                                                                                                                                                                                                                         easyClose = FALSE,
                                                                                                                                                                                                                         fade = TRUE,
                                                                                                                                                                                                                         size = "s"),
                                                    model_specification(),


                                                    tags$br(),
                                                    tags$h6("Choose role of features in forecast period:"),
                                                    shinyWidgets::radioGroupButtons("ftpye","Select usage of features",choices = c("no features"="no_features","past features"="past_features","forecasted features"="forecasted_features"),
                                                                                    selected = "no_features",status = "primary",checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                                                                                                                 no = icon("remove",lib = "glyphicon")),size = "sm") %>% shinyhelper::helper(type = "markdown",
                                                                                                                                                                                                                             title = "Inline Help",
                                                                                                                                                                                                                             content = "features",
                                                                                                                                                                                                                             buttonLabel = "Got it!",
                                                                                                                                                                                                                             easyClose = FALSE,
                                                                                                                                                                                                                             fade = TRUE,
                                                                                                                                                                                                                             size = "s"),
                                                    numericInput("n_ahead","Select forecast horizon",min = 1, value=5,max = 20,
                                                                 step = 1),

                                                    actionButton("run", "Run Model"),
                                                    shinyjs::hidden(p(id = "text1", "Processing...")),
                                                    actionButton("pred", "Predict"),
                                                    shinyjs::hidden(p(id = "text2", "Please run the model first")),
                                                    selectInput("forecast_plot_choice","Select plot to show:",
                                                                c("Forecasted","Full"),selected="Full")


                                   ),
                                   conditionalPanel(condition="input.tabs == 'Actual forecast'",
                                                    tags$h4("Set up model:"),
                                                    shinyWidgets::radioGroupButtons("model_spec_for","Choose model specification",choices = c("default","custom","hyperparameter tuning"="hyperparameter_tuning"),
                                                                                    selected = "default",status = "primary",checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                                                                                                             no = icon("remove",lib = "glyphicon")),size = "sm"),
                                                    model_specification_for(),


                                                    tags$br(),
                                                    tags$h6("Choose role of features in forecast period:"),
                                                    shinyWidgets::radioGroupButtons("ftpye2","Select covariates for forecast",choices = c("no features"="no_features","past features"="past_features","forecasted features"="forecasted_features"),
                                                                                    selected = "no_features",status = "primary",checkIcon = list(yes = icon("ok",lib = "glyphicon"),no = icon("remove",lib = "glyphicon")),size = "sm"),
                                                    numericInput("n_ahead2","select forecast window (days):",min = 1, value=5,max = 20,
                                                                 step = 1),
                                                    actionButton("run2", "Run Model on the full dataset"),
                                                    shinyjs::hidden(p(id = "text1_act", "Processing...")),
                                                    actionButton("pred2", "Predict")),
                                   shinyjs::hidden(p(id = "text2_act", "Please run the model first"))



                                 ),
                                 mainPanel(
                                   tabsetPanel(type = "tabs", id = "tabs",
                                               tabPanel("Information XGB",
                                                        tags$hr(),
                                                        htmlOutput("info_xgb_firstpage"),
                                                        withMathJax()),
                                               tabPanel("Summary statistics",value="Summary statistics",
                                                        tableOutput("xgb_summary"),
                                                        tags$hr(style="border-color: white;"),
                                                        tags$br(),
                                                        tags$br(),
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
                                                                         tags$br(),
                                                                         tags$hr(),
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
                                                        htmlOutput("test_text_xgb_act"),
                                                        dygraphs::dygraphOutput("plot_1_xgb_actual")%>% shinycssloaders::withSpinner(type = 5)

                                               )
                                   )
                                 )
                        )
              )#close Navbarmenu
  )#close Navbarpage
)#close fluidpage








