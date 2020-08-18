# 08/13/2020
# Exercise Lectures 

# load the quantmod package 

require(quantmod)

gold_ret <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE) # get the data from fred 

margin.table(is.na(gold_ret))

str(gold_ret)

gold_ret <- na.omit(gold_ret)# check your work 
gold_ret <- gold_ret["1979-12-31/2017-12-31"]
names(gold_ret) <- "GR"# stands for Gold Return 
head(gold_ret); tail(gold_ret)
logret_gold.d <- diff(log(gold_ret)) # see If i can remove NA now 

logret_gold.d <- logret_gold.d[-1,]
head(logret_gold.d)

# getting retun on different time horizons 

logret_gold.w <- apply.weekly(logret_gold.d, sum); head(logret_gold.w)
logret_gold.m <- apply.monthly(logret_gold.d, sum); head(logret_gold.m)
logret_gold.q <- apply.quarterly(logret_gold.d, sum); head(logret_gold.q)
logret_gold.y <- apply.yearly(logret_gold.d, sum); head(logret_gold.y)

#getting actual return on different time horizons 

ac.ret.d <- exp(logret_gold.d) - 1 ; head(ac.ret.d)
ac.ret.w <- exp(logret_gold.w) - 1 ; head(ac.ret.w)
ac.ret.m <- exp(logret_gold.m) - 1 ; head(ac.ret.m)
ac.ret.q <- exp(logret_gold.q) - 1 ; head(ac.ret.q)
ac.ret.y <- exp(logret_gold.y) - 1 ; head(ac.ret.y)

# getting the mean and sd of gold return (I could do it on diff time horizon)

mu.gold.d <- round(mean(logret_gold.d), 8); mu.gold.d
sd.gold.d <- round(sd(logret_gold.d), 8); sd.gold.d

# getting the var for our data 

VaR.gold <- qnorm(0.05, mean = mu.gold.d, sd = sd.gold.d) ; VaR.gold

VaR.Value <- 1000 * (exp(VaR.gold) - 1); VaR.Value

# Expected Shortfall 

Es.gold <- mu.gold.d - sd.gold.d * dnorm(qnorm(0.05, 0, 1), 0, 1) / 0.05; Es.gold

Es.gold.value <- 1000 * (exp(Es.gold) - 1); Es.gold.value

# simulation to calculate the VAr and ES 
# below is all from a normal distribution 
set.seed(123789)
rvec <- rnorm(100000, mean = mu.gold.d, sd = sd.gold.d)

VaR_G <- quantile(rvec, 0.05); VaR_G
Es_G <- mean(rvec[rvec < VaR_G]); Es_G

# sample from actual return 

set.seed(123789)
rvec.act.dist <- sample(as.vector(logret_gold.d), 100000, replace = TRUE)
VaR.g.act.dist <- quantile(rvec.act.dist, 0.05); VaR.g.act.dist
Es.g.act.dist <- mean(rvec.act.dist[rvec.act.dist < VaR.g.act.dist]); Es.g.act.dist


# getting different measures of skewness 
require(moments)

rvec.Gskew <- as.vector(logret_gold.d$GR)
round(skewness(rvec.Gskew), 6)
round(kurtosis(rvec.Gskew), 6)
jarque.test(rvec.Gskew)

# getting the student-t distribution and the function fitdistr

require(MASS)
rvec.t.dist <- as.vector(logret_gold.d)
t.fit <- fitdistr(rvec.t.dist, "t")
round(t.fit$estimate, 6)

t.fit

# estimating VaR and ES using Student-t 

alpha <- 0.05
set.seed(123789)
require(metRology)
rvec.t.dist.act <- rt.scaled(n = 100000, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3]); rvec.t.dist.act

Var.t.test.act <- quantile(rvec.t.dist.act, alpha); Var.t.test.act
es.t.test.act <- mean(rvec.t.dist.act[rvec.t.dist.act < Var.t.test.act]); es.t.test.act

round(Var.t.test.act, 6) ; round(es.t.test.act, 6)


# estimating three different meathod of stimulation 
# method one, 10 oneday estimulation 

require(MASS)
rvec.for.all <- as.vector(logret_gold.d); head(rvec.for.all)
t.fit.for.all <- fitdistr(rvec.for.all, "t")
round(t.fit.for.all$estimate, 6)


# below is one day stimulation 
alpha <- 0.05 
set.seed(123789)
rvec.10.oneday <- rep(0, 100000)
for (i in 1:10) {
  rvec.10.oneday <- rvec.10.oneday + rt.scaled(100000, 
                                               mean = t.fit.for.all$estimate[1],
                                               sd = t.fit.for.all$estimate[2],
                                               df = t.fit.for.all$estimate[3])
  
}
var.10.oneday <- quantile(rvec.10.oneday, alpha)
es.10.oneday <- mean(rvec.10.oneday[rvec.10.oneday < var.10.oneday])

round(var.10.oneday, 6) ; round(es.10.oneday, 10)


# Method two taking 1day from actual sample 

alpha <- 0.05 
set.seed(123789)
rvec.iid.dist <- rep(0, 100000)
for (i in 1:10) {
  rvec.iid.dist <- rvec.iid.dist + sample(as.vector(logret_gold.d), 100000, replace = TRUE)
}

var.iid.dist <- quantile(rvec.iid.dist, alpha)
es.iid.dist <- mean(rvec.iid.dist[rvec.iid.dist < var.iid.dist])

round(var.iid.dist, 6); round(es.iid.dist, 6)


# Method three, 10days in a row (block sampling) the first day of the 10 days is random but the rest of 10 days are not 

alpha <- 0.05
set.seed(123789)
rdata <- as.vector(logret_gold.d)
rvec.10day.b <- rep(0, 100000)
posn <- seq(from = 1, to = length(rdata) - 9, by = 1)
rpos <- sample(posn, 100000, replace = TRUE)
for (i in 1:10) {
  rvec.10day.b <- rvec.10day.b + rdata[rpos]
  rpos <- rpos + 1
}

var.10day.b <- quantile(rvec.10day.b, alpha)
es.10day.b <- mean(rvec.10day.b[rvec.10day.b < var.10day.b])

round(var.10day.b, 6); round(es.10day.b, 6)


# Seeing the ARCH and GARCH of the data 
# graphing the auto correlation of return 
acf(logret_gold.d)

# graphing the absolute value of auto correlation graphing 

acf(abs(logret_gold.d)) # clearly the graphs shows an auto correlation 

# Estimating the GARCH(1,1), load the rugarch package 
require(rugarch)

g.re.spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                        distribution.model = "std")

# save some of the work 

g.fit.garch <- ugarchfit(spec = g.re.spec, data = logret_gold.d[,1])

# the estimated parameters are in g.fit.garch@fit#scope 

g.fit.garch@fit$coef

# saving some ofthe data together 

g.save.garch <- cbind(logret_gold.d[,1], g.fit.garch@fit$sigma, g.fit.garch@fit$z)
names(g.save.garch) <- c("Log.return.daily", "S", "Z")
View(g.save.garch)

# graph some of the ARCH and GARCH to see auto correlations 

acf(g.save.garch$Z)
acf(abs(g.save.garch$z)) # I don't know why I can't create the same graph 



acf(abs(as.vector(g.save.garch$z)))
x <- as.vector(g.save.garch$Z)
class(x)

acf(abs(x))

