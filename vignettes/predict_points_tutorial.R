

library(jsonlite)
library(ggplot2)
library(data.table)
library(neuralnet)
library(fastDummies)
library(magrittr)
library(glue)
library(parallel)



root <- getwd()
data_path <- file.path(root, "data")
code_path <- file.path(root, "R")




from_url <- TRUE
file1 <- "fpl_2017_json.RData"
file2 <- "fpl_player_history_dt.RData"


library(fpl.analytics)
library(devtools)

fpl_season_json <- pullFplSeasonJson()
devtools::use_data(fpl_season_json)


fpl_player_hist <- pullFplHistoryDt(fpl_season_json)
devtools::use_data(fpl_player_hist)

# Read data
if(from_url){
  fpl_season_json <- pullFplSeasonJson(data_path, file1)
}else{
  list_dt <- get(load(file.path(data_path, file1)))
}

players <- list_dt$elements

dummies <- c("element_type", "team")
dt <- cleanPlayerData(dt = list_dt$elements, dummies)
saveRDS(dt, "clean_last_year_data.rds")


if(from_url){
  player_api <- "https://fantasy.premierleague.com/drf/element-summary"
  all_player_ids <- unique(dt$id)
  
  ff <- dt[, .(id, name)]
  ff 
  
  hist_dt <- lapply(all_player_ids, function(id){
    print(round(100*match(id, all_player_ids)/length(all_player_ids),1))
    out <- tryCatch(jsonlite::fromJSON(glue("{player_api}/{id}")), error = function(e){
      NULL
    })$history_past
    if(!is.null(out)){
      out$player_key <- id
    }
    out
  })
  hist_dt <- rbindlist(hist_dt)
  
  fpl_player_hist <- file2
  save(fpl_player_hist, file = file.path(data_path, file2))
}else{
  hist_dt <- get(load(file.path(data_path, file2)))
}


setdiff(names(dt), names(hist_dt))
player_dt <- dt[, .(player_key = id, name, team, element_type,
                    new_price = now_cost)]

hist_dt <- merge(hist_dt, player_dt, by = "player_key")
hist_dt[, now_cost := start_cost]

hist_dt <- hist_dt[minutes > 0,]
hist_dt <- hist_dt[total_points > 0,]
dummies <-  c("season", "element_type")
hist_dt <- dummy_cols(hist_dt, dummies)
hist_dt <- addMinMaxCols(hist_dt, c("minutes", "now_cost", "total_points"))

# set dt to hist_dt, as main set now
dt <- copy(hist_dt)


all_dummies <- lapply(dummies, FUN = function(i){
  grep(sprintf("^%s_", i), names(dt), fixed = FALSE, value = TRUE)
})
variables <- c(unlist(all_dummies), c("mm_now_cost", "mm_minutes"))
variables <- setdiff(variables, "season_name")
response <- "mm_total_points"

model_formula <- as.formula(
  sprintf("%s ~ %s", 
          response,
          paste(variables, collapse = " + "))
)


train <- FALSE
if(train){
  set.seed(3)
  index <- sample(1:nrow(dt), floor(nrow(dt))*0.6)
  
  # creating training and test set
  trainNN = dt[index , ]
  testNN = dt[-index , ]
  
  # fit neural network
  set.seed(2)
  N <- 5
  H <- 4
  NN = neuralnet(formula = model_formula, 
                 data = trainNN,
                 hidden = H, 
                 rep = N,
                 linear.output = T)
  
  for(i in 1:N){
    predict_testNN = compute(NN, testNN[, variables, with = FALSE], rep = i)
    predict_testNN = (predict_testNN$net.result * (dt[, max(total_points)] - dt[, min(total_points)])) + dt[, min(total_points)]
    print(mean(abs(testNN$total_points - predict_testNN)))
  }
  print(which.min(NN$result.matrix[1,]))
  plot(NN, rep = "best")
  
  predict_testNN = compute(NN, testNN[, variables, with = FALSE], rep = which.min(NN$result.matrix[1,]))
  predict_testNN = (predict_testNN$net.result * (dt[, max(total_points)] - dt[, min(total_points)])) + dt[, min(total_points)]
  print(mean(abs(testNN$total_points - predict_testNN)))
  
  par(mfrow = c(1,1))
  plot(testNN$total_points, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
  abline(0,1)
}


## try https://cran.r-project.org/web/packages/monmlp/monmlp.pdf
## for monotonicity

# fit neural network on all
set.seed(2)
N <- 5
H <- 3
NN_ALL = neuralnet(formula = model_formula, 
                   data = dt,
                   hidden = H, 
                   rep = N,
                   linear.output = T)

# plot neural network
# plot(NN_ALL)

dt[, mm_now_cost_orig := mm_now_cost]
dt[, mm_now_cost := mm_now_cost_orig]
predict_allNN = compute(NN_ALL, dt[, variables, with = FALSE],
                        rep = which.min(NN_ALL$result.matrix[1,]))
predict_allNN = (predict_allNN$net.result * (dt[, max(total_points)] - dt[, min(total_points)])) + dt[, min(total_points)]
dt[, fit := predict_allNN]
dt[fit < 0, fit := 0]
dt[, fit := round(fit, 1)]

# dt <- addMinMaxCols(dt, "new_price")
dt[, mm_new_price := (new_price-min(now_cost))/(max(now_cost)-min(now_cost))]
dt[, mm_now_cost := mm_new_price]
predict_allNN = compute(NN_ALL, dt[, variables, with = FALSE],
                        rep = which.min(NN_ALL$result.matrix[1,]))
predict_allNN = (predict_allNN$net.result * (dt[, max(total_points)] - dt[, min(total_points)])) + dt[, min(total_points)]
dt[, fit2 := predict_allNN]
dt[fit2 < 0, fit2 := 0]
dt[, fit2 := round(fit2, 1)]

par(mfrow = c(1,1))
plot(dt$fit, dt$total_points, 
     col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)


dt[, residual := total_points - fit]
dt[, residual2 := total_points - fit2]
dt[, price := now_cost/10]
dt[, price2 := new_price/10]
dt[, price_diff := price2-price]
dt[, num_games := round(minutes/90, 1)]
dt[, residual_per_cost := residual/price]
# plot(jitter(dt$now_cost), dt$residual)
# abline(h=0)
# plot(dt$minutes, dt$residual)
# abline(h=0)


report_cols <- c("season_name", "team", "name", "price", "price2", "price_diff", "num_games", "total_points", "element_type",
                 "fit", "fit2", "residual", "residual2")
out <- dt[, report_cols, with = FALSE]

last_out <- out[season_name == "2017/18",]
last_out[, season_name := NULL]

# merge back with
out[name == "David De Gea"]
out[grepl("bark", tolower(name)),]

setorder(last_out, -price2, -total_points)

pos_out <- split(last_out, last_out$element_type)
View(pos_out[[1]])
View(pos_out[[2]])
View(pos_out[[3]])
View(pos_out[[4]])

lapply(pos_out, head, 10)
lapply(pos_out, tail, 10)


## simulation
boxplot(dt$mm_minutes ~ dt$element_type)
boxplot(dt$mm_now_cost ~ dt$element_type)

minutes_vals <- c(0.25, 0.5, 0.75)
cost_vals <- seq(0, 1, 0.01)
elment_vales <- unique(dt$element_type)

sim_dt <- CJ(mm_minutes = minutes_vals
             ,mm_now_cost = cost_vals
             ,element_type = elment_vales )

sim_dt <- dummy_cols(sim_dt, "element_type")


predict_allNN = compute(NN_ALL, sim_dt[, variables, with = FALSE],
                        rep = which.min(NN_ALL$result.matrix[1,]))
predict_allNN = (predict_allNN$net.result * (dt[, max(total_points)] - dt[, min(total_points)])) + dt[, min(total_points)]

sim_dt[, fit := predict_allNN]
sim_dt[fit < 0, fit := min(dt$total_points)]

sim_dt[, price := (mm_now_cost * (dt[, max(now_cost)] - dt[, min(now_cost)])) + dt[, min(now_cost)]]
sim_dt[, minutes := (mm_minutes * (dt[, max(minutes)] - dt[, min(minutes)])) + dt[, min(minutes)]]
sim_dt[, price := price * 0.1]
sim_dt[, games := minutes/ 90]

caps <- dt[, .(min(now_cost/10), max(now_cost/10)), keyby = element_type]
sim_dt <- merge(sim_dt, caps, by = "element_type")
sim_dt <- sim_dt[between(price, V1, V2),]

fwrite(sim_dt, file = "play.csv")





