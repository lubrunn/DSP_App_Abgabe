library(tidyverse)
#install.packages("rnaturalearth")

querry_str <- "select * from coords where company = 'De_NoFilter' "





con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/clean_database.db")

df_need <- DBI::dbGetQuery(con, querry_str)

#disconnect
DBI::dbDisconnect(con)



world <- map_data("world")


ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(aes(df_need$long, df_need$lat), alpha = 0.1)

