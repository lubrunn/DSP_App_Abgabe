#' Test Functions

#' @export
#' @rdname load_filtered_files

En_NoFilter_0_0_yes <- function(){
  filename <- "Twitter/sentiment/Shiny_files/sentiment_aggregated/EN_NoFilter_0_0_yes.csv"
  #filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EZMnSOrMHqZPoYT5PgFiQkkBLdU6rTC8qYnoAdWKMjprrg?download=1"
  read.csv(filename)
}

En_NoFilter_0_10_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ESIAS7dm2_ZIjorGDwnELR8BsQRNNEKl1L_u64u9-9mnaw?download=1"
  read.csv(filename)
}

En_NoFilter_0_50_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ES3TE18oM29MlYLIxYnET-8Bicxta-n2BYhzy5-F48_mlg?download=1"
  read.csv(filename)
}

En_NoFilter_0_100_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EdDm7_2bxKhOm9gaISen79MBf8tvFSIWCwqRHTajnGkElQ?download=1"
  read.csv(filename)
}

En_NoFilter_0_200_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ESowqmuGdaJOqCve5f5OZA0B7QZm-MeLvP6jOu8ZPUYFDQ?download=1"
  read.csv(filename)
}

En_NoFilter_10_0_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/Edds0fspt05ApeN5GTjvEHEBYQqeR2E8GavCiF5JzkBhLw?download=1"
  read.csv(filename)
}

En_NoFilter_10_10_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EapBb5UqlOpDvBFCn_STe0wBfbK-XCisIIZ4sBXwCbeckg?download=1"
  read.csv(filename)
}

En_NoFilter_10_50_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EdTQx0wLnUdAim7M_XpDmVwBUnubgskORXQBJFJzBXNJ9w?download=1"
  read.csv(filename)
}

En_NoFilter_10_100_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EQPw9LuxYtpApa-1OJgfrYUBop-dZDqbBHgpnEtsGfjIUQ?download=1"
  read.csv(filename)
}

En_NoFilter_10_200_yes <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EfQmjl4fcN9MgY054o3A0E4BWU20401rosvse9Un9W9jyA?download=1"
  read.csv(filename)
}





#' @export
#' @rdname load_stocks

ADS.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/chunk/adidas.feather"
  arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

ALV.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/chunk/Allianz.feather"
  arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

DHER.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/chunk/Delivery Hero.feather"
  arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

DBK.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/chunk/Deutsche Bank.feather"
  arrow::read_feather(filename)
}

