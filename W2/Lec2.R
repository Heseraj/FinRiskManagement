# 08/13/2020
# Lectrue 2 
# 
# load quantmod package 

require(quantmod)

# talking about CLT (central limit theorem)
# two issues are very important (Value at Risk and Expected Shortfall)  VAR and ES 

wilsh_ret <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
margin.table(is.na(wilsh_ret))

wilsh_ret <- na.omit(wilsh_ret)
wilsh_ret <- wilsh_ret["1979-12-31/2017-12-31"]
names(wilsh_ret) <- "WR"

w_logret <- diff(log(wilsh_ret$WR))
w_logret <- w_logret[-1,]

round(mean(w_logret), 8); round(sd(w_logret), 8)

mu <- round(mean(w_logret), 8)
stdev <- round(sd(w_logret), 8)

plot(dnorm(w_logret, mean = mu, sd = stdev))
plot(rnorm(w_logret, mean = mu, sd = stdev))
plot(qnorm(w_logret, mean = mu, sd = stdev))


# VAR 
a <- 0.05
round(qnorm(a, mu, stdev), 6) # we can use qnorm to calculate the var at different values of alphas 

# expected Shortfalls 

# Other names of ES 
# Conditional value at risk  
# Average value at risk   
# Expected Tails loss 

# formual to calculate ES 

ES <- mu - stdev * dnorm(qnorm(a, 0,  1),0,1) / a # this is a very important formula 
ES
# What does it mean for the hedge fund manger  

Es_val <- 1000*(exp(ES) - 1)
Es_val


# simulation to calculate the VAr and ES 

alpha <- 0.05
set.seed(123789)
rvec <- rnorm(100000, mu, stdev)
# View(rvec); 
VaR_W <- quantile(rvec, alpha)
VaR_W
Es_w <- mean(rvec[rvec < VaR_W])
Es_w

# Observaton from actual population of return 
set.seed(123789)
rvec <- sample(as.vector(w_logret), 100000, replace = TRUE)
VaR_w <- quantile(rvec, probs = alpha)
Es_w <- mean(rvec[rvec<VaR_w])
round(VaR_w, 6)
round(Es_w, 6)
