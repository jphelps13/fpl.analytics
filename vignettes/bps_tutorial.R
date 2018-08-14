library(gtools)
library(data.table)
library(magrittr)
library(MASS)
library(rvest)
library(ggplot2)

# beta binomial
library(HRQoL)
library(rmutil)
library(VGAM)

library(fpl.analytics)
# 
# dt <- setDT(fpl_season_json$elements)
# 
# dt[, num_games := minutes/38]
# dt <- dt[num_games > 0,]
# 
# # check team game time
# check_time <- dt[, .(minutes = sum(minutes)), keyby = team]
# # remove any anomaly
# check_time <- check_time[minutes > 10000,]
# check_time[, prop := minutes/(38*90*11)]
# dt <- dt[team %in% check_time$team]
# 
# hist(dt$bps/dt$num_games, probability = T)
# f <- mean(dt$bps/dt$num_games)
# lines(density(rpois(1000, f)))
# # create some fake data



# creating data for each gw, for a season

season <- "2017-18"
season_loc <- file.path(getwd(), sprintf("data/%s/", season))

files <- list.files(file.path(season_loc, "players"), full.names = TRUE)
players <- list.files(file.path(season_loc, "players"), full.names = FALSE)

pb <- txtProgressBar(min = 0, max = length(players), style = 3)
combined_list <- lapply(seq_along(files), function(i){
  setTxtProgressBar(pb, i)
  dt <- fread(file.path(files[i], "gw.csv"))
  dt <- dt[minutes > 0, ]
  set(dt, j = "player_name", value = players[i])
  dt[, c("kickoff_time", "kickoff_time_formatted", "transfers_balance",
         "loaned_in", "loaned_out", "ea_index", "element") := NULL]
  dt
})
combined <- rbindlist(combined_list)
combined[bps < 0, bps := 0]
setkey(combined, player_name)
check <- combined[minutes >= 60, .N, by = player_name]
check <- check[N >= 3, ]
combined <- combined[check[, .(player_name)], on = "player_name"]
combined[, player_name := stringi::stri_trans_general(player_name, "Latin-ASCII")]
combined[, simple_name := gsub(" ", "", trimws(tolower(player_name)))]

# lookups
player_dt <- fread(file.path(season_loc, "players_raw.csv"), encoding = "UTF-8")
player_dt[, player_name := paste(first_name, second_name, sep = "_")]
player_dt[, player_name := stringi::stri_trans_general(player_name, "Latin-ASCII")]
player_dt[, simple_name := gsub(" ", "", trimws(tolower(player_name)))]
# player_dt[, element_type := as.character(element_type)]
player_dt <- merge(player_dt, fpl_season_json$element_types[, c("id", "singular_name_short")],
                   by.x = "element_type", by.y = "id")
setnames(player_dt, c("singular_name_short"), c("position"))

players <- unique(combined$simple_name)


i <- match("jack_wilshere", players)
i <- match("moussa_sissoko", players)

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

pb <- txtProgressBar(min = 0, max = length(players), style = 3)

theta_calc_list <- lapply(seq_along(players), function(i){
  print(i)
  # setTxtProgressBar(pb, i)
  sdt <- combined[simple_name == players[i]]
  mod1 <- glm.nb(bps ~ offset(log(minutes)), data = sdt, weights = minutes)
  all_fits1 <- fitted(mod1)
  
  mins <- sdt$minutes
  mins <- mins[mins >= 45]
  set.seed(1)
  mins <- mins + runif(length(mins), -2, 2)

  pmins <- mins/90
  pmins[pmins < 1e-10] <- 1e-10
  pmins[pmins > (1-1e-10)] <- (1-1e-10) 
  
  fit2 <- vglm(pmins ~ 1, betaR, trim = 0, trace = FALSE)
  
  
  mins <- as.integer(round(mins, 0))
  fit <- vglm(cbind(mins, rep(max(mins), length(mins))-mins) ~ 1, betabinomial, trace = FALSE)
  
  # 
  out <- data.table(simple_name = players[i],
                    mu = mean(all_fits1/(sdt$minutes/90)),
                    theta = theta.ml(sdt$bps, all_fits1),
                    b_alpha = Coef(fit2)[1],
                    b_beta = Coef(fit2)[2],
                    bb_mu = Coef(fit)[1],
                    bb_rho = Coef(fit)[2],
                    p_start = length(mins)/38)
  if(F){

    hist(sdt$bps/(sdt$minutes/90), freq = FALSE)
    lines(density(rnegbin(1000, mu = out$mu, theta = out$theta/2)))
    
    hist(mins/max(mins), freq = F, breaks = 10)
    mins <- as.integer(mins)
    fit <- vglm(cbind(mins, rep(max(mins), 38)-mins) ~ 1, betabinomial, trace = FALSE)
    x <- rbetabinom(10000, 900, Coef(fit)[1], Coef(fit)[2])/900
    lines(density(x))
    x2 <- rbeta(10000, Coef(fit2)[1], Coef(fit2)[2])
    lines(density(x2), col = "blue")
    fits <- estBetaParams(mean(mins/90), var(mins/90))
    y <- rbeta(1000, fits$alpha, fits$beta)
    lines(density(y), col = "red")
  }

  out
})

theta_calc <- rbindlist(theta_calc_list)

theta_calc <- merge(theta_calc, player_dt[, .(simple_name, player_name, team, position)],
                    by = "simple_name")
theta_calc <- merge(theta_calc, combined[, .(minutes = sum(minutes),
                                             bps = sum(bps),
                                             bonus = sum(bonus)), by = simple_name],
                    by = "simple_name")

theta_calc[team == 11,]


# ggplot() + geom_density(data=theta_calc, aes(x=theta, group=position, fill=position),
#                         alpha=0.5, adjust=2) + theme_minimal() +coord_cartesian(xlim = c(0, 40))
# 


dt <- copy(theta_calc)
dt[, player_key := simple_name]
dt[, num_matches := p_start*38]
sdt <- dt


## simulation

N <- 100
subs <- 3
pitch <- 11
L <- pitch + subs
L2 <- 2*L
S <- 3
L2S <- S:1

# sample for each team some set formations
teams <- sort(unique(dt$team))

max_pos_select <- data.table(position = c("GKP", "DEF", "MID", "FWD"),
                             cap = c(1, 5, 5, 3), key = "position")


formations <- lapply(teams, function(x){
  print(x)
  gg <- sdt[team == x, .(player_key, position, p_start)]
  setorder(gg, -p_start)
  players <- gg[, player_key]
  probs   <- gg[, p_start]
  
  gg <- merge(gg, max_pos_select, by = "position")
  setkey(gg, player_key)
  
  out <- lapply(1:N, function(iteration){
    # print(iteration)
    picked  <- c()
    dropped <- c()
    set <- copy(gg)
    # main 11
    while(length(picked) < pitch){
      
      # pick player and make decision on selection
      k <- sample(1:nrow(set), 1, F, set$p_start)
      select <- sample(c(0,1), 1, F, p = c(1-set$p_start[k], set$p_start[k]))
      if(select == 1){
        picked <- c(picked, set$player_key[k])
        set <- set[-k,]
        check_pos <- gg[J(picked), .(.N, cap[1]), keyby = position][N >= V2,]
        dropped <- c(dropped, set[position %in% check_pos$position, player_key])
        set <- set[!player_key %in% dropped]
      }
    }
    # subs
    set <- copy(gg)[!player_key %in% picked,]
    while(length(picked) < L){
      k <- sample(1:nrow(set), 1, F, set$p_start)
      select <- sample(c(0,1), 1, F, p = c(1-set$p_start[k], set$p_start[k]))
      if(select == 1){
        picked <- c(picked, set$player_key[k])
        set <- set[-k,]
      }
    }
    
    picked
  })
  out <- t(do.call(rbind, out))
  # replicate(N, sample(players, L, FALSE, probs))
})
names(formations) <- as.character(teams)

# check how many matches participated and merge back
check_formations <- lapply(teams, function(x){
  f <- formations[[x]]
  f <- as.data.table(table(f[1:11,]))
  setnames(f, c("player_key", "num_matches"))
  f[, exp_matches := 38*num_matches/N]
  f[, num_matches := NULL]
  f
}) %>% rbindlist(.)

sdt[, exp_matches := NULL]
sdt <- merge(sdt, check_formations, by = "player_key")

plot(sdt$num_matches, sdt$exp_matches)
abline(0,1)

matches <- as.data.table(combinations(length(teams), 2, teams))
setkey(sdt, player_key)
NM <- nrow(matches)
print(NM)

x <- matches[, which(V1 == 1 & V2 == 11)]

all_matches <- lapply(1:NM, function(x){
  print(x)
  m1 <- matches[x, 1][[1]]
  m2 <- matches[x, 2][[1]]
  
  out <- lapply(1:N, function(i){
    f1 <- formations[[m1]][,i]
    f2 <- formations[[m2]][,i]
    both  <- c(f1,f2)
    input <- sdt[J(both), .(position, mu, theta, bb_mu, bb_rho, player_key)]
    bps   <- rnegbin(L2, mu = input$mu, theta = input$theta)
    # mins <- rbeta(L2, input$bb_alpha, input$bb_beta)
    mins1 <- VGAM::rbetabinom(pitch, 90, input$bb_mu[1:pitch], input$bb_rho[1:pitch])
    mins2 <- VGAM::rbetabinom(pitch, 90, input$bb_mu[(L+1):(L+pitch)], 
                              input$bb_rho[(L+1):(L+pitch)])
    subbed1 <- frank(mins1, ties.method = "random")
    subbed2 <- frank(mins2, ties.method = "random")
    mins1 <- c(rep(90, pitch), 90 - mins1[subbed1[1:3]])
    mins2 <- c(rep(90, pitch), 90 - mins2[subbed1[1:3]])
    v <- round(bps * c(mins1, mins2)/90, 0)
    
    if(F){
      plot(mins)
      abline(v = 11.5)
    }
    
    set(input, j = "v", v = v)
    set(input, j = "rank", v = frankv(-v, ties.method = "dense"))
    set(input, j = "score", v = 0)
    
    # assign score
    remaining <- S
    i <- 1
    n <- input[rank == i, .N]
    
    while(remaining > 0){
      input[rank == i, score := remaining]
      remaining <- remaining - n
      i <- i + 1
      n <- input[rank == i, .N]
    }
    
    input[score > 0, .(player_key, score)]
  }) 
  out <- as.data.table(do.call(rbind, out))
  out <- out[, .(score = sum(score)/N), keyby = player_key]
  out
})

summary <- rbindlist(all_matches)
summary <- summary[, .(fit_sim = sum(score)*2), keyby = player_key]
summary <- summary[order(-fit_sim)]

sdt[, fit_sim := NULL]
sdt <- merge(sdt, summary, by = "player_key", all.x = TRUE)
sdt[is.na(fit_sim), fit_sim := 0]

sum(sdt$fit_sim)
sum(sdt$bonus)

sdt[, residual := bonus - fit_sim]
sdt <- sdt[order(-residual),]
sdt[]

plot(sdt$bonus, sdt$fit_sim)
abline(0,1)

mean(abs(sdt$bonus - sdt$fit_glm))
mean(abs(sdt$bonus - sdt$fit_sim))

median(abs(sdt$bonus - sdt$fit_glm))
median(abs(sdt$bonus - sdt$fit_sim))

var(abs(sdt$bonus - sdt$fit_glm))
var(abs(sdt$bonus - sdt$fit_sim))

setorder(sdt, - bonus)
tdt <- sdt[, .(team, position, player_key, player_name, mu, theta, p_start, bps, bonus, fit_sim, fit_glm, residual)]
# tdt[, num_matches := round(num_matches, 1)]

tdt[team == 11,]

f <- tdt[, .(sum(bonus), sum(fit_bonus)), by = team][order(-V1),]
plot(f$V1, f$V2)
abline(0,1)

sdt[, .(sum(bonus)/38), by = team]


# MODEL

combined[, c("player_name", "team", "position") := NULL]
combined <- merge(combined, player_dt[, .(simple_name, player_name, team, position)],
                  by = "simple_name")
combined <- combined[bps > 0,]

combined[, bonus_frac := bonus/max(bonus)]
combined[, team := as.character(team)]
combined[, bps_frac := (bps - min(bps))/(max(bps) - min(bps))]
myglm <- glm(bonus_frac ~ bps_frac + team + position, data = combined, 
             family = quasibinomial('logit'))
summary(myglm)

# predict
combined$fit_pois <- round(predict(m_pois, type = "response"), 2)
combined$fit_logi <- round(predict(myglm, type = "response"), 2)
combined$fit_logi_t <- combined$fit_logi * 3

glm_result <- combined[, .(fit_glm = sum(fit_logi_t)), by = simple_name]
sdt[, fit_glm := NULL]
sdt <- merge(sdt, glm_result, by = "simple_name")

sdt[, residual_glm := bonus - fit_glm]
plot(sdt$bonus, sdt$fit_glm)
abline(0,1)

pl <- ggplot(combined, aes(x = bps_frac, y = bonus_frac)) + facet_wrap(~position, scales = "free_y") + 
  geom_point(col = "darkgrey") +
  scale_size_continuous(range = c(0,2)) + theme_minimal() + 
  geom_line(aes(x = bps_frac, y = fit_logi, col = team))
pl




# COMPARE RESIDUALS