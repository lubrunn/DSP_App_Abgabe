
##### here is a list that is used for the select input, it fits the folder names and shows
# nicer names to the user when folder names are not nice enough


### this is for tweets selection
company_terms <- list(
  "Unifiltered Tweets" = list("Unfiltered Tweets" = "NoFilter"),
  "DAX" = list(

                   "Adidas" = "adidas",
                   "Allianz" = "Allianz",


                  "BASF" = "BASF",
                  "Bayer" = "Bayer",
                  "Beiersdorf" = "Beiersdorf",
                  "BMW" = "BMW",

                  "Continental" = "Continental",
                  "Covestro" ="Covestro",
                  "Daimler" = "Daimler",
                  "Delivery Hero" = "Delivery Hero",
                  "Deutsche Bank" = "Deutsche Bank",
                  "Deutsche Börse" = "Deutsche Börse",
                  "Deutsche Post" = "Deutsche Post",
                  "Deutsche Telekom" = "Deutsche Telekom",
                  "Deutsche Wohnen" = "Deutsche Wohnen",

                  "EON" = "EON",
                  "Fresenius" = "Fresenius",
                  "Fresenius Medical Care" = "Fresenius Medical Care",
                  "HeidelbergCement" = "HeidelbergCement",
                  "Henkel" = "Henkel",
                  "Infineon Technologies" = "Infineon Technologies",
                  "Linde" = "Linde",
                  "Merck (German)" = "MerckDE",
                  "MTU Aero Engines" = "MTU Aero Engines",
                  "Münchener Rück" = "Münchener Rück",
                  "RWE" = "RWE",
                  "SAP" = "SAP",
                  "Siemens" = "Siemens",
                  "Volkswagen" = "Volkswagen",
                  "Vonovia" = "Vonovia"
                  ),


"Dow Jones Industrial" = list( "3M" = "3M",
              "American Express" = "American Express",
              "Amgen" = "Amgen",
              "Apple" = "Apple",
              "Boeing" = "Boeing",
              "Caterpillar" = "Caterpillar",
              "Chevron" = "Chevron",
              "Cisco Systems" = "Cisco",
              "Coca-Cola" = "Coca-Cola",
              "Dow" = "Dow",
                "Goldman Sachs" = "Goldman Sachs",
                "Home Depot" = "Home Depot",
                  "Honeywell International" = "Honeywell International",
                  "IBM" = "IBM",
                  "Intel" = "Intel",
                  "Johnson & Johnson" = "JohnsonJohnson",
                  "JPMorgan" = "JPMorgan",
                 "McDonald's" = "McDonald's",
                "Merck (American)" = "MerckEN",
              "Microsoft" = "Microsoft",
                 "NIKE" = "NIKE",
                  "Procter & Gamble" = "Procter Gamble",

                  "Salesforce" = "salesforce",
                  "Travelers Companies" = "Travelers Companies",
            "UnitedHealth" = "UnitedHealth",
                  "Verizon" = "Verizon",
                  "Visa" = "Visa",
              "Walgreens Boots Alliance" = "Walgreens Boots Alliance",
                  "Walmart" = "Walmart",
                  "Walt Disney" = "Walt Disney"
                  )
)








##### this is for stock selection
us_names <- c("Dow Jones Industrial",
              "Apple", "Amgen", "American Express", "Boeing",
              "Caterpillar", "Salesforce", "Cisco Systems", "Chevron",
              "Walt Disney", "Dow", "Goldman Sachs",
              "Home Depot", "Honeywell International", "IBM",
              "Intel", "Johnson & Johnson", "JPMorgan Chase", "Coca-Cola",
              "McDonald's", "3M", "Merck (American)", "Microsoft",
              "NIKE", "Procter & Gamble", "The Travelers Companies",
              "UnitedHealth", "Visa", "Verizon",
              "Walgreens Boots Alliance", "Walmart")

us_tickers <- c("DJI", "AAPL", "AMGN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS",
                "DOW", "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO",
                "MCD", "MMM", "MRK", "MSFT", "NKE", "PG", "TRV", "UNH", "V",
                "VZ", "WBA", "WMT")


ger_names <- c("DAX",
               "Covestro", "Adidas", "Allianz", "BASF", "Bayer", "Beiersdorf",
               "BMW", "Continental", "Daimler", "Deutsche Börse",
               "Deutsche Bank", "Delivery Hero", "Deutsche Post", "Deutsche Telekom",
               "Deutsche Wohnen", "EON", "Fresenius Medical Care", "Fresenius",
               "HeidelbergCement", "Henkel", "Infineon Technologies", "Linde",
               "Merck (German)", "MTU Aero Engines",
               "Münchener Rück", "RWE", "SAP",
               "Siemens", "Vonovia", "Volkswagen")

ger_tickers <- c("GDAXI",
                 "1COV.DE", "ADS.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE",
                   "BMW.DE", "CON.DE", "DAI.DE", "DB1.DE", "DBK.DE", "DHER.DE",
                   "DPW.DE", "DTE.DE", "DWNI.DE", "EOAN.DE", "FME.DE", "FRE.DE",
                   "HEI.DE", "HEN3.DE", "IFX.DE", "LIN.DE", "MRK.DE", "MTX.F",
                   "MUV2.DE", "RWE.DE", "SAP.DE", "SIE.DE", "VNA.DE", "VOW3.DE")


##### create named list for selectInput
company_terms_stock_us <- setNames(as.list(us_tickers), us_names)
company_terms_stock_ger <- setNames(as.list(ger_tickers), ger_names)

company_terms_stock <- list(
  "Dow Jones Industrial" = company_terms_stock_us,
  "DAX" = company_terms_stock_ger
)








########################## comparison plot

###### control variables
controls_comp_list <- list(

  "COVID-19" = list(

    "New Cases per Million" = "new_cases_per_million",
    "New Deaths per Million" = "new_deaths_per_million",
    "Reproduction Rate" = "reproduction_rate",
    "Google coronavirus searches" = "coronavirus"
  ),


  "Other country-level controls" = list(
                  "Volatility index" = "VIX"

  ),
  "Other global controls" = list(

    "Financial distress index" = "OFR.FSI",
    "Economic uncertainty index" = "WLEMUINDXD"
  )
)


