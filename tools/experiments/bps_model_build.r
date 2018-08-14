
# rmarkdown::render("vignettes/bps_tutorial.Rmd")

# build the model with first just position

ggplot(dt, aes(x = bps, y = bonus, size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal()

ggplot(dt, aes(x = bps^2, y = bonus, size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal()

ggplot(dt, aes(x = log(bps), y = bonus, size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal()


ggplot(dt, aes(x = bps, y = bonus/num_matches, size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal()


dt[, team_scaled_bps := (bps-min(bps))/(max(bps) - min(bps)), 
   by = .(season_name, team)]
dt <- dt[!is.na(dt$team_scaled_bps), ]
ggplot(dt, aes(x = team_scaled_bps, y = bonus, size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal()


m_null <- glm(bonus ~ log(bps):position + bps:position, data = dt, 
              family = poisson(link = "log"))


# m_null <- glm(bonus ~ bps:position, data = dt, 
#               family = poisson(link = "log"))




summary(m_null)

dt$fit_null <- predict(m_null, type = "response")
dt$res_null <- dt$bonus - dt$fit_null

ggplot(dt, aes(x = bps, y = log(bonus), size = num_matches)) + facet_wrap(~position) + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal() +
  geom_line(aes(x = bps, y = log(fit_null)))


m_pois_z <- zeroinfl(bonus ~ log(bps):position + bps:position | 1, data = dt, dist = "poisson")
summary(m_pois_z)

dt$fit_pois_z <- predict(m_pois_z, type = "response")
dt$res_pois_z <- dt$bonus - dt$fit_pois_z


ggplot(dt, aes(x = bps, y = bonus)) + facet_wrap(~position) + 
  geom_point(col = "darkgrey") +
  scale_size_continuous(range = c(0,2)) + theme_minimal() + 
  geom_line(aes(x = bps, y = fit_null), col = "red") + 
  geom_line(aes(x = bps, y = fit_pois_z), col = "blue") 



ggplot(dt, aes(x = bonus, y = fit_pois_z)) + facet_wrap(~position) + 
  geom_point(col = "darkgrey") +
  scale_size_continuous(range = c(0,2)) + theme_minimal() + 
  geom_abline(intercept = 0, slope = 1)


mean(abs(dt$res_null))
mean(abs(dt$res_pois_z))

var(abs(dt$res_null))
var(abs(dt$res_pois_z))

xmet <- "bps"

ggplot(dt, aes_string(x = xmet, y = "res_null", size = "num_matches")) + 
  facet_wrap(~position, scales = "free_y") + geom_point() +
  scale_size_continuous(range = c(0,2)) + theme_minimal() + 
  geom_hline(yintercept = 0)



## simulatio

N <- 500
L <- 11
L2 <- 2*L
S <- 3
L2S <- (L2-S+1):L2

sdt <- dt[season_name == "2017/18",]
sdt[, play_p := num_matches/38]


# sample for each team some set formations
teams <- sort(unique(dt$team))

formations <- lapply(teams, function(x){
  print(x)
  gg <- sdt[team == x,]
  setorder(gg, -play_p)
  players <- gg[, player_key]
  probs   <- gg[, play_p]
  
  out <- lapply(1:N, function(iteration){
    # print(iteration)
    picked <- c()
    set <- copy(gg)
    while(length(picked) < L){
      if(nrow(set) == 0){
        set <- copy(gg)[!player_key %in% picked,]
      }
      k <- sample(1:nrow(set), 1, F, set$play_p)
      select <- sample(c(0,1), 1, F, p = c(1-set$play_p[k], set$play_p[k]))
      if(select == 1){
        picked <- c(picked, set$player_key[k])
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
  f <- as.data.table(table(f))
  setnames(f, c("player_key", "num_matches"))
  f[, exp_matches := 38*num_matches/N]
  f[, num_matches := NULL]
  f
}) %>% rbindlist(.)
check_formations[, player_key := as.integer(player_key)]

sdt[, exp_matches := NULL]
sdt <- merge(sdt, check_formations, by = "player_key")
sdt[, diff := num_matches - exp_matches]
setorder(sdt, -diff)

plot(sdt$num_matches, sdt$exp_matches)
abline(0,1)

matches <- as.data.table(combinations(length(teams), 2, teams))
setkey(sdt, player_key)
NM <- nrow(matches)
print(NM)

# problem with the simulation one is that every player is assumed to follow
# a poisson process. However, bps differs by player positions, so probably
# need each to follow a NB distribution. Need to figure out how to estimate
# the parameters of the NB model by position, and then feed that in here instead
# of the Poisson ones. This should hopefully fix the "Salah" error.

positions <- unique(sdt$position)

theta_list <- lapply(positions, FUN = function(x){
  
  # w <- which(sdt$position == x)
  
  mod <- glm.nb(bps ~ offset(log(num_matches)), data = sdt[position == x])
  all_fits <- fitted(mod)
  
  out <- list(
    "a" = theta.md(sdt[position == x]$bps, all_fits, dfr = df.residual(mod))
    ,"b" = theta.ml(sdt[position == x]$bps, all_fits)
    ,"c" = theta.mm(sdt[position == x]$bps, all_fits, dfr = df.residual(mod))
  )
})

thetas <- pluck(theta_list, "c")
names(thetas) <- positions

for(i in positions){
  hist(sdt[position == i]$bps_match, freq = FALSE, main = i)
  dd <- rnegbin(1000, mu = mean(sdt[position == i]$bps_match),
                theta = thetas[[i]])
  lines(density(dd))
}

theta_dt <- melt(as.data.table(thetas))
setnames(theta_dt, c("position", "theta"))
sdt <- merge(sdt, theta_dt, by = "position")

generateNb <- function(player, sdt, N){
  rnegbin(n = N, mu = sdt[player_name == player, bps_match], 
          theta = sdt[player_name == player, theta])
}

nb_check <- sdt[, .(mean(bps_match), var(bps_match)), by = position]



estBetaParams(0.2, 0.5)

nb_check[, .(estBetaParams(V1, V2)), by = position]

setkey(sdt, id)


library(HRQoL)
library(rmutil)

f <- dbetabinom(seq(0,90,1), 90, 0.2, 12)
plot(f)

f <- combined[simple_name == "moussa_sissoko"]
mins <- as.integer(c(rep(0, 38 - nrow(f)), f$minutes))
fit <- BBest(mins, 90, "MM")

hist(mins, freq = F)
lines(density(rbetabinom(1000, 90, fit$p, fit$phi)))


library(LaplacesDemon)

n1 <- dgpois(x, lambda=0, omega=0, log=FALSE)


all_matches <- lapply(1:NM, function(x){
  print(x)
  m1 <- matches[x, 1][[1]]
  m2 <- matches[x, 2][[1]]
  
  out <- lapply(1:N, function(i){
    f1 <- formations[[m1]][,i]
    f2 <- formations[[m2]][,i]
    both <- c(f1,f2)
    posv <- sdt[J(both), position]
    thetav <- unlist(thetas[posv])
    # bps <- rpois(L2, sdt[J(both), bps_match])
    bps <- rnegbin(L2, mu = sdt[J(both), bps_match], theta = thetav)
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

sdt[, fit_bonus0 := NULL]
sdt <- merge(sdt, summary, by = "id", all.x = TRUE)
sdt[is.na(fit_bonus0), fit_bonus0:= 0]

# hack: missing 3 games for each team..
adjustment <- sum(sdt$bonus)/sum(sdt$fit_bonus0)
sdt[, fit_bonus := fit_bonus0 * adjustment]
sdt[, residual := bonus - fit_bonus]






players <- c("Mohamed Salah"
             ,"Roberto Firmino")
sdt[player_name %in% players,]

# salah v firmino
ggplot(sdt, aes(x=bps_match)) + 
  geom_histogram(data=data.table(bps_match = generateNb(players[1], sdt, 10000)), 
                 fill = "red", alpha = 0.4, binwidth = 3) +
  geom_histogram(data=data.table(bps_match = generateNb(players[2], sdt, 10000)), 
                 fill = "blue", alpha = 0.4, binwidth = 3) + 
  geom_vline(xintercept = sdt[player_name == players[1], bps_match], color = "red") + 
  geom_vline(xintercept = sdt[player_name == players[2], bps_match], color = "blue") + 
  theme_minimal()
# geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)

player  <- "Roberto Firmino"
print(sdt[player_name == player, theta])
new_theta <- 1


dist <- rnegbin(10000, 10, 10000)
mean(dist)
var(dist)

ggplot(sdt, aes(x=bps_match)) + 
  geom_histogram(data=data.table(bps_match = rnegbin(10000, mu = sdt[player_name == player, bps_match],
                                                     theta = sdt[player_name == player, theta])), 
                 fill = "red", alpha = 0.4, binwidth = 3) +
  geom_histogram(data=data.table(bps_match = rnegbin(10000, mu = sdt[player_name == player, bps_match],
                                                     theta = new_theta)), 
                 fill = "blue", alpha = 0.4, binwidth = 3) + 
  geom_vline(xintercept = sdt[player_name == player, bps_match], color = "black") + 
  theme_minimal()

