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
               tags$p("In the case that choosing a path through the 'Select Folder' button \n
             is not possible we recommentd updating RStudio. If that does not help you can
             also enter your path manually"),
             textInput("dir_path_man", ""),
             actionButton("dir_path_man_btn", "Set path"),
             tags$head(tags$style("#path_checker_man{color: red;

                                 }"
             )
             ),
             textOutput("path_checker_man")

           ),
           column(8,
                  tags$p("Welcom to our shiny app. Please select the folder containing the \n
                         data in order to continue."),
                  tags$h4("Selected folder"),

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
