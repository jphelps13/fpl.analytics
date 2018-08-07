# script for generating the data in the /data folder

remove.packages("fpl.analytics")
devtools::install_github("jphelps13/fpl.analytics")

library(fpl.analytics)
library(devtools)

fpl_season_json <- pullFplSeasonJson()
devtools::use_data(fpl_season_json, overwrite = TRUE)

fpl_player_hist <- pullFplHistoryDt(fpl_season_json)
devtools::use_data(fpl_player_hist, overwrite = TRUE)
