#' Functions to load stock data
##################################################################
#' @export
#' @rdname load_stocks
ADS <- function(){
  filename <- "Yahoo/Germany/Germany_ADS.DE"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EWf5tG1J6SVNmpE6jsTVk5gBam18MZEFc04M9d4NcuA3cg?download=1"
  read.csv(filename)
}


#' @export
#' @rdname load_stocks
GDAXI <- function(){
  filename <- "Yahoo/Germany/Germany_GDAXI.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EVO4lOATrkFMgSvfkMML30oB0Clv4c4TQhKi7bA5wc7Q9Q?download=1"
  read.csv(filename)
}


#' @export
#' @rdname load_stocks
DOW <- function(){
  filename <- "Yahoo/USA/USA_DOW.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/Eav8KQIXUmdJq32Kw2RmU78BSVD2fRw2CAU3_ar2fTxAqA?download=1"
  read.csv(filename)
}

##########################full company datasets
#' @export
#' @rdname load_stocks
load_all_stocks_DE <- function(){
  filename <- "Yahoo/Full/Germany_full.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EagyRFS8KStClVOHtP9Nkj0BUbAwefPendK6to-1BQIZSQ?download=1"
  help <- read.csv(filename)
  help$Dates <- as.Date(help$Dates)
  return(help)
}

#' @export
#' @rdname load_stocks
load_all_stocks_US <- function(){
  filename <- "Yahoo/Full/USA_full.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EQg9PrsDenZIi4Sc0N9iKWQBnJhA2UFNu5smYWes8El6Sg?download=1"
  help <- read.csv(filename)
  help$Dates <- as.Date(help$Dates)
  return(help)
}

######################## components
#' @export
#' @rdname load_stocks
COMPONENTS_DE <- function(){
  filename <- "Yahoo/Germany/Germany_Index_Components.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EYhDUmLHadVFlHwO8KkVUuABSVgQRPS0nadLAYaRgxIYSw?download=1"
  read.csv(filename)
}


#' @export
#' @rdname load_stocks
COMPONENTS_US <- function(){
  filename <- "Yahoo/USA/USA_Index_Components.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/Ec9gayTfc09Psw5WKjtD7qYB8DUe3PGKp1uNgAFt41JRqA?download=1"
  read.csv(filename)
}
########################### controls für regression

#' @export
#' @rdname load_stocks
global_controls_test_DE <- function(){
  filename <- "Twitter/sentiment/Model/controls_DE.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EYlajGptNv5CmBepQHp4rroBWfYi-iS9R0Q5MmB9oFxQZg?download=1"
  read.csv(filename)
}

#' @export
#' @rdname load_stocks
global_controls_test_US <- function(){
  filename <- "Twitter/sentiment/Model/controls_US.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EdK7M0hgGDpAk_jOPLz-72ABDcQSPSuF32uTQFzyvsALCw?download=1"
  read.csv(filename)
  }


