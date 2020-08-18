# 08/13/2020
# Lecture 3 
# 
# load packages and datas    

require(quantmod)
require(moments)
require(tidyverse)

wilsher5000 <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
margin.table(is.na(wilsher5000))

wilsher5000 <- na.omit(wilsher5000)

logret_wilsher <- wilsher5000
head(logret_wilsher); tail(logret_wilsher)
logret_wilsher$logret <- 0
logret_wilsher$logret <- diff(log(wilsher5000$WILL5000IND))
logret_wilsher[1,2] <- 0
hist(logret_wilsher$logret, breaks = 15, col = "sky blue", border = FALSE)

mean(logret_wilsher$logret)
sd(logret_wilsher$logret)

# getting the skewnwss 
rvecskew <- as.vector(logret_wilsher$logret) # remeber to change the log colums 
head(rvecskew)


round(skewness(rvecskew), 4)

# finding the kurtosis of a vector 

rvecKur <- as.vector(logret_wilsher$logret)
round(kurtosis(rvecKur), 4)

# getting the jarque test 
rvecjarque <- as.vector(logret_wilsher$logret)
jarque.test(rvecjarque)

# get the qqplot 
# qqplot(rvecjarque, qnorm(0,1))

# distribution that explains that non-normality 
# student T distribution 

require(MASS)
rvec.fitdis <- as.vector(logret_wilsher$logret)
t.fit <- fitdistr(rvec.fitdis, "t")
round(t.fit$estimate, 6)


# estimating VaR and ES for Student-t Distribution 

alpha <- 0.05
set.seed(123789)
?metRology
require(metRology)
rvec.tdist <- rt.scaled(100000, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3])
Var.tdist <- quantile(rvec.fitdis, alpha)
ES.tdist <- mean(rvec.tdist[rvec.tdist < Var.tdist])

round(Var.tdist, 6)
round(ES.tdist, 6)


# estimating VaR and Es for multi-day horizon 
# generating 10 one day log return (if adding them up it will be retun over 10 days)
# Method A 

alpha <- 0.05
set.seed(123789)
require(metRology)
rvec.10.oneday <- rep(0, 100000)

for (i in 1:10) {
  rvec.10.oneday <- rev.10.oneday + rt.scaled(100000, mean = t.fit$estimate[1], sd = t.fit$estimate[2], df = t.fit$estimate[3])
}

var.10.oneday <- quantile(rvec.10.oneday, alpha)
es.10.oneday <- mean(rvec.10.oneday[rvec.10.oneday < var.10.oneday])

round(var.10.oneday, 6); round(es.10.oneday, 6)

# Method B IID simulation 

alpha <- 0.05
set.seed(123789)
rvec.iid.dist <- rep(0, 100000)
for (i in 1:10) {
  rvec.iid.dist <- rvec.iid.dist + sample(as.vector(logret_wilsher), 100000, replace = TRUE)
}

var.iid.dist <- quantile(rvec.iid.dist, alpha)
es.iid.dist <- mean(rvec.iid.dist[rvec.iid.dist < var.iid.dist])

round(var.iid.dist, 6); round(es.iid.dist, 6)


# method c, block, 10 consecutive returns (the first choice of oneday return is independent but the rest of 9 days are not)

alpha <- 0.05
set.seed(123789)
rvec.10day.block <- rep(0, 100000)
rdata <- as.vector(logret_wilsher$logret); head(rdata)
posn <- seq(from = 1, to = length(rdata) - 9, by = 1)
rpos <- sample(posn, 100000, replace = TRUE)
for (i in 1:10) {
  rvec.10day.block <- rvec.10day.block + rdata[rpos]
  rpos <- rpos + 1
}

var.10day.block <- quantile(rvec.10day.block, alpha)
es.10day.block <- mean(rvec.10day.block[rvec.10day.block < var.10day.block])

round(var.10day.block, 6); round(es.10day.block, 6)
