library(gtools)
library(data.table)
library(magrittr)
library(MASS)
library(rvest)
library(ggplot2)

library(fpl.analytics)

dt <- setDT(fpl_season_json$elements)

dt[, num_games := minutes/38]
dt <- dt[num_games > 0,]

# check team game time
check_time <- dt[, .(minutes = sum(minutes)), keyby = team]
# remove any anomaly
check_time <- check_time[minutes > 10000,]
check_time[, prop := minutes/(38*90*11)]
dt <- dt[team %in% check_time$team]

hist(dt$bps/dt$num_games, probability = T)
f <- mean(dt$bps/dt$num_games)
lines(density(rpois(1000, f)))
# create some fake data
