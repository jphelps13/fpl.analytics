require(ggplot2)
require(pscl)
require(boot)

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)


ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()


m1 <- zeroinfl(count ~ child + camper | persons, data = zinb)
summary(m1)

p1 <- glm(count ~ child + camper, data = zinb, family = poisson("log"))
summary(p1)


mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)

vuong(p1, m1)




f <- function(data, i, start_count, start_zero) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
                start = list(count = start_count, zero = start_zero))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(zinb, f, R = 1200, parallel = "snow", ncpus = 4,
            start_count = coef(m1, "count"),
            start_zero = coef(m1, "zero"))
res


## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

confint(m1)


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms



newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")


zinb$fit1 <- predict(m1, type = "response")
zinb$fit2 <- predict(p1, type = "response")

plot(zinb$count, zinb$fit1)
plot(zinb$count, zinb$fit2)
