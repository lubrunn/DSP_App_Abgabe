

############################################################# granger instructions
observeEvent(input$instructions_granger,{
  guide_granger$init()$start()

})

guide_granger <- cicerone::Cicerone$
  new()$
  step(
    el = "first_variable_granger",
    title = "Choose the first variable",
    description = "Here, you can select the first variable for the Granger causality analysis. First, choose a country for the company/index. Secondly select the desired
    company/index. Lastly pick a variable."
  )$
  step(
    "second_variable_granger",
    "Choose the first variable",
    "Here, you can select the second variable. If you decide to use Sentiment, click on the switch and then specify the type of sentiment using the 'Filter sentiment
    input' tab at the top. Alternatively, a variable from the first dropdown or second dropdown can be selected."
  )$
  step(
    "date_variable_granger",
    "Choose timeseries",
    "Here, you can choose which period to focus on. The analysis is based on the selected range of Dates. Choose at least a period of 30 days!"
  )$
  step(
    "direction_variable_granger",
    "Direction",
    "Here, you can choose the direction of the granger causality. If this box is checked, the analysis verifies whether the second variable granger causes the first."
  )
##################################################################

############################################################# reg instructions
observeEvent(input$instructions_regression,{
  guide_regression$init()$start()

})

guide_regression <- cicerone::Cicerone$
  new()$
  step(
    el = "first_variable_regression",
    title = "Choose the first variable",
    description = "Here, you can select the dependent variable for the regression analysis.First, choose a country for the company/index. Secondly select the desired
    company/index. Lastly pick a variable."
  )$
  step(
    "control_variable_regression",
    "Choose the control variable(s)",
    "Here, you can select control variables. If you decide to use Sentiment, click on the switch and then specify the type of sentiment using the 'Filter sentiment
    input' tab at the top. Alternatively, a variable from the first dropdown or second dropdown can be selected."
  )$
  step(
    "date_variable_regression",
    "Choose timeseries",
    "Here, you can choose which period to focus on. The analysis is based on the selected range of Dates. Choose at least a period of 30 days!"
  )
##################################################################  var
observeEvent(input$instructions_var,{
  guide_var$init()$start()

})

guide_var <- cicerone::Cicerone$
  new()$
  step(
    el = "first_variable_var",
    title = "Choose the first variable",
    description = "Here, you can select the dependent variable for the VAR. First, choose a country for the company/index. Secondly select the desired
    company/index. Lastly pick a variable."
  )$
  step(
    "control_variable_var",
    "Choose the control variable(s)",
    "Here, you can select control variables. If you decide to use Sentiment, click on the switch and then specify the type of sentiment using the 'Filter sentiment
    input' tab at the top. Alternatively, a variable from the first dropdown or second dropdown can be selected. Caution! If no control variable is selected,
    an AR model is built using only the dependent variable!"
  )$
  step(
    "date_variable_var",
    "Choose timeseries",
    "Here, you can choose which period to focus on. The analysis is based on the selected range of Dates. Choose at least a period of 30 days!"
  )$
  step(
    "forecast_var",
    "Choose how many days to forecast",
    "Here, a number of days to forcast into the future can be selected. On the 'validity' tab, this is the period on which to calculate performance.
    On the 'actual forecast' tab, these are the future dates for the forecast."
  )
########










