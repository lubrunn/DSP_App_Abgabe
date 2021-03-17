#' Stock Calculations
#'

#CORONA dataset for selcted countries


#' @export
#' @rdname corona_calculations
CORONA <- function(country,datestart,dateend){
  filename <- "Corona/owid.csv"
  help <- dplyr::filter(read.csv(filename),location %in% c(country))
  help$date <- as.Date(help$date)
  help <- dplyr::filter(help,date >= datestart & date <= dateend)
  help
}
#' @export
#' @rdname corona_calculations
CORONA_xgb <- function(country){
  filename <- "Corona/owid.csv"
  help <- dplyr::filter(read.csv(filename),location %in% c(country))
  help$date <- as.Date(help$date)
  #help <- filter(help,date >= datestart & date <= dateend)
  help
}


#' @export
#' @rdname corona_calculations
CORONA_neu <- function(country){
  filename <- "Corona/owid.csv"
  help <- dplyr::filter(read.csv(filename),location %in% c(country))
  help$date <- as.Date(help$date)
  help
}


#' @export
#' @rdname corona_calculations
selectize_corona_granger <- function() {
  selectizeInput("corona_measurement_granger","Corona variable:",
                 c("total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),selected = "",
                 multiple = FALSE)
}

#' @export
#' @rdname corona_calculations
selectize_corona_regression <- function() {
  selectizeInput("corona_measurement_regression","Corona controls:",
                 c("","total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),selected = "",multiple = FALSE)
}

#' @export
#' @rdname corona_calculations
selectize_corona_var <- function() {
  selectizeInput("corona_measurement_var","Corona controls:",
                 c("","total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),selected = "",multiple = FALSE)
}


#' @export
#' @rdname corona_calculations
selectize_corona_xgb <- function() {
  selectizeInput("corona_measurement_xgb","Corona controls:",
                 c("","total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),selected = "",multiple = FALSE)
}


#' @export
#' @rdname corona_calculations
create_hover_info_corona <- function(hovercorona,coronadata,selectedmeasure){
  hover <- hovercorona
  point <- nearPoints(coronadata, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.6); ",
                  "left:", left_px + 2, "px; top:", top_px + 30, "px;")

  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(htmltools::HTML(paste0("<b> Company: </b>", point$location, "<br/>",
                  "<b> Date: </b>", point$date, "<br/>",
                  "<b>" ,selectedmeasure,": </b>", point[selectedmeasure], "<br/>")))
  )
}
