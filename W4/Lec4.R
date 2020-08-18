# 08/14/2020
# Lecture 4 
# 
# 
# upload the data and packages 


require(quantmod)
require(tidyverse)
require(MASS)
require(metRology)
require(moments)

wilsher.5000 <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)

margin.table(is.na(wilsher.5000))
wilsher.5000 <- na.omit(wilsher.5000); head(wilsher.5000)

names(wilsher.5000) <- "WR"; head(wilsher.5000)

logret.wils.d <- diff(log(wilsher.5000)); head(logret.wils.d)
logret.wils.d[1,1] <- 0 ; head(logret.wils.d) # wonder if not removing the first row (NA row), will the result be impacted 

# test determining whether the serial correlation is important 
# graphing the return
# read the documentation for ACF to get more detials 
? acf
acf(logret.wils.d)

# volatility clustering Arch #auto regressive conditional heterscdascity# Model (auto regression ) and GARCH 

acf(abs(logret.wils.d)) # the large returns are followed by large returns auto clustering 


# GARCH(1,1)

require(rugarch) # this is the package that helps with the ARCH and GARCH of the data 

garch.N <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                      distribution.model = "norm")
fit.garch.n <- ugarchfit(spec = garch.N, data = logret.wils.d)
fit.garch.n

save1 <- cbind(logret.wils.d, fit.garch.n@fit$sigma, fit.garch.n@fit$z)
names(save1) <- c("logret.wilsh.daily", "s", "z")
head(save1)

mean(save1$z)
sd(save1$z)
skewness(save1$z)
kurtosis(save1$z)


# GARCH with a different distribution 

require(rugarch)
garch.t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std")
fit.garch.t <- ugarchfit(spec = garch.t, data = logret.wils.d)

# save the variables 
save1.t <- cbind(logret.wils.d, fit.garch.t@fit$sigma, fit.garch.t@fit$z)
names(save1.t) <- c("logret.wilsh.daily", "s", "z"); head(save1.t)
parm1 <- fit.garch.t@fit$coef; head(parm1)


# running some diagnostic test for ARCH and GARCH 

# Getting the VaR and ES using garch function 

set.seed(123789)
boot.garch <- ugarchboot(fit.garch.t, method = "Partial", sampling = "raw", n.ahead = 1, n.bootpred = 100000, solver = "solnp")

view(boot.garch@fseries)

# getting the VaR and Es 

wilsher.rvec <- boot.garch@fseries

alpha <- 0.05
wilsher.VaR <- quantile(wilsher.rvec, alpha)
wilsher.ES <- mean(wilsher.rvec[wilsher.rvec < wilsher.VaR])

round(wilsher.VaR, 6); round(wilsher.ES, 6)

# getting VaR and ES from 1980 to sep 15, 2008 


logret.wils.d.FC <- logret.wils.d["1979-12-31/2008-9-15"]
view(logret.wils.d.FC)
garch.t.fc <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                         distribution.model = "std")
fit.garch.t.fc <- ugarchfit(spec = garch.t.fc, data = logret.wils.d.FC)
parm1.fc <- fit.garch.t.fc@fit$coef; head(parm1.fc)

set.seed(123789)
boot.garch.fc <- ugarchboot(fit.garch.t.fc, method = "Partial", sampling = "raw", n.ahead = 1, n.bootpred = 100000, solver = "solnp")
wilsher.rvec.fc <- boot.garch.fc@fseries
wilsher.VaR.fc <- quantile(wilsher.rvec.fc, alpha)
wilsher.ES.fc <- mean(wilsher.rvec.fc[wilsher.rvec.fc < wilsher.VaR.fc])
round(wilsher.VaR.fc, 6); round(wilsher.ES.fc, 6)
