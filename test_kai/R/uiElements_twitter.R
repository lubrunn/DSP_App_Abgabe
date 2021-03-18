###############################################################################
######################### twitter #############################################
###############################################################################

# the main panel for twitter containing all sub panels and main panels
twitter_main_panel <- function(){
  navbarMenu("Twitter",

             ###### tab panel for descriptive
             tabPanel("Descriptives",
                      # sidebar panel for descriptive
                      #twitter_desc_panel(),
                      sidebarPanel(
                        twitter_tab_desc,

                      ),



                      twitter_desc_main,


                      tags$hr(),
                      other_desc_main


             ),
             ################### tab panel descirptive end




             # andere reiter
             tabPanel("Going Deeper",
                      # sidebarPanel(

                      tags$style(HTML('#sw-content-dropdown, .sw-dropdown-in {background-color: #4e5d6c;
                                      margin: 0 0;}')),



                      network_tab_main

             )

  )

}

