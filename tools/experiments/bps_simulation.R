library(gtools)
library(data.table)
library(magrittr)
library(MASS)
library(rvest)
library(ggplot2)
dt <- readRDS("clean_last_year_data.rds")

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

set.seed(1)
fdt <- data.table(team = c(1,1,1,1,1,2,2,2,2,2),
                  id = 1:10,
                  p = runif(10, 0, 1),
                  bpg = rpois(10, f)
                  )

fdt <- unique(dt[, .(team, name, id, element_type,
                     num_games, bps, bonus, now_cost)])
fdt[, p := num_games/sum(num_games), keyby = team]
fdt[, bpg := bps/(num_games)]
fdt[, bpgp := bpg/sum(bpg), by = team]
fdt[, p := p/sum(p), by = team]
fdt[, id := as.character(id)]
fdt[, play_p := num_games/38]
fdt$element_type <- as.factor(fdt$element_type)
# fdt$easy_score <- (sum(1:S) * fdt$p * fdt$bpgp)/sum(fdt$p * fdt$bpgp) 

N <- 100
L <- 11
L2 <- 2*L
S <- 3
L2S <- (L2-S+1):L2

# sample for each team some set formations
teams <- as.character(sort(unique(as.numeric(fdt$team))))
formations <- lapply(teams, function(x){
  print(x)
  gg <- fdt[team == x,]
  setorder(gg, -play_p)
  players <- gg[, id]
  probs   <- gg[, play_p]
  
  out <- lapply(1:N, function(iteration){
    # print(iteration)
    picked <- c()
    set <- copy(gg)
    while(length(picked) < L){
      if(nrow(set) == 0){
        set <- copy(gg)[!id %in% picked,]
      }
      k <- sample(1:nrow(set), 1, F, set$play_p)
      select <- sample(c(0,1), 1, F, p = c(1-set$play_p[k], set$play_p[k]))
      if(select == 1){
        picked <- c(picked, set$id[k])
      }
      set <- set[-k,]
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
  f <-as.data.table(table(f))
  setnames(f, c("id", "n_games"))
  f[, exp_games := 38*n_games/N]
  f[, n_games := NULL]
  f
}) %>% rbindlist(.)

fdt[, exp_games := NULL]
fdt <- merge(fdt, check_formations, by = "id")
fdt[, diff := num_games - exp_games]
setorder(fdt, -diff)

plot(fdt$num_games, fdt$exp_games)
abline(0,1)

matches <- as.data.table(combinations(length(teams), 2, teams))
setkey(fdt, id)
NM <- nrow(matches)
print(NM)

# problem with the simulation one is that every player is assumed to follow
# a poisson process. However, bps differs by player positions, so probably
# need each to follow a NB distribution. Need to figure out how to estimate
# the parameters of the NB model by position, and then feed that in here instead
# of the Poisson ones. This should hopefully fix the "Salah" error.
# 

quine_nb <- glm.nb(Days ~ .^2, data = quine)

summary(quine_nb)

positions <- unique(fdt$element_type)

theta_list <- lapply(positions, FUN = function(x){
  
  # w <- which(fdt$element_type == x)
  
  quine_nb <- glm.nb(bps ~ offset(log(num_games)), data = fdt[element_type == x])
  all_fits <- fitted(quine_nb)

  out <- list(
   "a" = theta.md(fdt[element_type == x]$bps, all_fits, dfr = df.residual(quine_nb))
  ,"b" = theta.ml(fdt[element_type == x]$bps, all_fits)
  ,"c" = theta.mm(fdt[element_type == x]$bps, all_fits, dfr = df.residual(quine_nb))
  )
})

thetas <- pluck(theta_list, "a")
names(thetas) <- positions

for(i in positions){
  hist(fdt[element_type == i]$bpg, freq = FALSE, main = i)
  dd <- rnegbin(1000, mu = mean(fdt[element_type == i]$bpg),
                theta = thetas[[i]])
  lines(density(dd))
}

theta_dt <- melt(as.data.table(thetas))
setnames(theta_dt, c("element_type", "theta"))
fdt <- merge(fdt, theta_dt, by = "element_type")

generateNb <- function(player, fdt, N){
  rnegbin(n = N, mu = fdt[name == player, bpg], 
          theta = fdt[name == player, theta])
}

players <- c("Mohamed Salah"
             ,"Roberto Firmino")
fdt[name %in% players,]

# salah v firmino
ggplot(fdt, aes(x=bpg)) + 
  geom_histogram(data=data.table(bpg = generateNb(players[1], fdt, 10000)), 
                 fill = "red", alpha = 0.4, binwidth = 3) +
  geom_histogram(data=data.table(bpg = generateNb(players[2], fdt, 10000)), 
                 fill = "blue", alpha = 0.4, binwidth = 3) + 
  geom_vline(xintercept = fdt[name == players[1], bpg], color = "red") + 
  geom_vline(xintercept = fdt[name == players[2], bpg], color = "blue") + 
  theme_minimal()
  # geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)

player  <- "Roberto Firmino"
print(fdt[name == player, theta])
new_theta <- 1


dist <- rnegbin(10000, 10, 10000)
mean(dist)
var(dist)

ggplot(fdt, aes(x=bpg)) + 
  geom_histogram(data=data.table(bpg = rnegbin(10000, mu = fdt[name == player, bpg],
                                               theta = fdt[name == player, theta])), 
                 fill = "red", alpha = 0.4, binwidth = 3) +
  geom_histogram(data=data.table(bpg = rnegbin(10000, mu = fdt[name == player, bpg],
                                         theta = new_theta)), 
                 fill = "blue", alpha = 0.4, binwidth = 3) + 
  geom_vline(xintercept = fdt[name == player, bpg], color = "black") + 
  theme_minimal()



fdt[, .(mean(bpg), var(bpg)), by = element_type]

setkey(fdt, id)

all_matches <- lapply(1:NM, function(x){
  print(x)
  m1 <- matches[x, 1][[1]]
  m2 <- matches[x, 2][[1]]
  
  out <- lapply(1:N, function(i){
    f1 <- formations[[m1]][,i]
    f2 <- formations[[m2]][,i]
    both <- c(f1,f2)
    posv <- fdt[J(both), element_type]
    thetav <- unlist(thetas[posv])
    # bps <- rpois(L2, fdt[J(both), bpg])
    bps <- rnegbin(L2, mu = fdt[J(both), bpg], theta = thetav)
    both[match(L2S, frankv(bps, ties.method = "random"))]
  }) 
  out <- as.data.table(do.call(rbind, out))
  
  scores <- lapply(1:S, function(i){
    out[, .N*i, by = get(names(out)[i])]
  }) %>% rbindlist
  setnames(scores, c("id", "score"))
  out <- scores[, .(score = sum(score)/N), keyby = id]
  out
})

summary <- rbindlist(all_matches)
summary <- summary[, .(fit_bonus0 = sum(score)*2), keyby = id]

fdt[, fit_bonus0 := NULL]
fdt <- merge(fdt, summary, by = "id", all.x = TRUE)
fdt[is.na(fit_bonus0), fit_bonus0:= 0]

# hack: missing 3 games for each team..
adjustment <- sum(fdt$bonus)/sum(fdt$fit_bonus0)
fdt[, fit_bonus := fit_bonus0 * adjustment]
fdt[, residual := bonus - fit_bonus]

setorder(fdt, - residual)
head(fdt)
# fdt[, fit_poisson := copy(fit_bonus)]

plot(jitter(fdt$bonus), fdt$fit_bonus)
abline(0,1)
cor(fdt$bonus, fdt$fit_bonus)
mean(abs(fdt$residual))
median(abs(fdt$residual))

boxplot(fdt$residual ~ fdt$element_type,
        ylim = quantile(fdt$residual, c(0.05,0.95)))
abline(0,0)


boxplot(fdt$bpg ~ fdt$element_type,
        ylim = quantile(fdt$bpg, c(0.025,0.975)))

boxplot(fdt$bonus/fdt$bps ~ fdt$element_type,
        ylim = quantile(fdt$bonus/fdt$bps, c(0.025,0.975)))

xx <- fdt[, .(bps = sum(bps),
              bonus = sum(bonus),
              rate = sum(bonus)/sum(bps)),
          keyby = team]
setorder(xx, - bps)
fdt[, name[bonus == max(bonus)], by = team]


# plot the data
types <- sort(unique(fdt$element_type))
par(mfrow = c(2,2))
t = types[1]
for(t in types){
  extract <- fdt[element_type == t,]  
  plot(extract$bps, extract$bonus)
  plot(log(extract$bps), log(extract$bonus))
  plot(extract$num_games, extract$bps)
  plot(jitter(extract$now_cost), extract$bpg)
}

## try a zero-inflated poisson/NB model for the glm approach below
table(fdt$bonus == 0) # too many 0 counts I think is causing the residual pattern
# when you look at bonus v residual


fdt[, team_bps := quantile(bps, 0.75), by = team]
plot(fdt$bps/fdt$team_bps, fdt$bonus)

fdt[bonus > 18 & bps/team_bps < 1]

m1 <- glm.nb(bonus ~ offset(log(bps)) + element_type*now_cost + team, data = fdt)

z1 <- zeroinfl(bonus ~ offset(log(bps)) + element_type*now_cost + team | num_games,
               data = fdt,
               dist = "negbin")
summary(z1)

vuong(m1, z1)

# m1 <- glm(bonus ~ bps + element_type, data = fdt,
#           family = poisson(link = "log"))
summary(m1)
m2 <- glm(bonus ~ offset(log(bps)) + element_type*now_cost + team
          , data = fdt
          , family = poisson(link = "log"))
summary(m2)

fdt$fit_bonus2 <- predict(m1, fdt, type = "response")
fdt$residual2 <- fdt$bonus - fdt$fit_bonus2
fdt$fit_bonus3 <- predict(z1, fdt, type = "response")
fdt$residual3 <- fdt$bonus - fdt$fit_bonus3

res_metric <- "residual3"

par(mfrow = c(1,1))
boxplot(fdt[[res_metric]] ~ fdt$team, 
        ylim = quantile(fdt[[res_metric]], c(0.025,0.975)))
abline(h=0)
boxplot(fdt[[res_metric]] ~ fdt$element_type, 
        ylim = quantile(fdt[[res_metric]], c(0.025,0.975)))
abline(h=0)
plot(jitter(fdt$now_cost), fdt[[res_metric]])
abline(h=0)
plot(fdt$bonus, fdt[[res_metric]])
abline(h=0)

mean(abs(fdt$residual))
mean(abs(fdt$residual2))
mean(abs(fdt$residual3))

var(abs(fdt$residual))
var(abs(fdt$residual2))
var(abs(fdt$residual3))

median(abs(fdt$residual))
median(abs(fdt$residual2))
median(abs(fdt$residual3))

setorder(fdt, -residual)
head(fdt)
tail(fdt)

fdt[team == 12]
par(mfrow = c(1,1))

plot(jitter(fdt$bonus), fdt$fit_bonus)
abline(0,1)

plot(jitter(fdt$bonus), fdt$fit_bonus2)
abline(0,1)

plot(jitter(fdt$bonus), fdt$fit_bonus3)
abline(0,1)





setorder(fdt, - residual)
head(fdt)
