# 8/13/2020
# Lecture1.1

require()

# understadning risk factors some of which is diversifiable 
# good working with data from FRED

# retreiving data from FRED 

 # load the package "quantmod" 

require(quantmod)

wilsh <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
# is.na(wilsh) # check the data for na
margin.table(is.na(wilsh))
wilsh <- na.omit(wilsh) # omit.na 


wilsh1 <- wilsh
wilsh1 <- wilsh1["1979-12-31/2017-12-31"]
colnames(wilsh1)
names(wilsh1) <- "TR" # total return = TR
wilsh1
# is.na(wilsh1)
# margin.table(is.na(wilsh1))

head(wilsh1);tail(wilsh1)

logret_w <- apply.weekly(wilsh1, FUN = sum) # getting the log weekly return 

head(round(logret_w, 3),5); tail(round(logret_w, 3), 5)

head(ret_w)
logret_m <- apply.monthly(wilsh1, FUN = sum)
head(logret_m)
logret_q <- apply.quarterly(wilsh1, FUN = sum)
logret_y <- apply.yearly(wilsh1, FUN = sum)

ret_w <- exp(logret_w) - 1 # to get the discrete return from log function 
ret_m <- exp(logret_m) - 1
ret_q <- exp(logret_q) - 1
ret_y <- exp(logret_y) - 1
# ts.plot(wilsh1, gpars = "scatter") # I couldn't do the trick 

# calculating the log return 
# see if I can crate a new colum 
# 
# wilsh1$logReturn <- 0
# wilsh1$logReturn <- diff(log(wilsh1$TR))[-1,] # It seems the NA will be created, It seems the command doesn't work and I coldn't remove NA 

wilsh1 <- diff(log(wilsh1$TR))[-1]
head(round(wilsh1, 6))
# calculate the discret return 
invreturn <- exp(wilsh1) - 1
head(round(invreturn, 4))

?ts.plot()

# Exercise, Getting Data from FRED

Goldmpg <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
margin.table(is.na(Goldmpg))

Goldmpg1 <- na.omit(Goldmpg)
margin.table(is.na(Goldmpg1))

Goldmpg1 <- Goldmpg1["1979-12-31/2017-12-31"]
head(Goldmpg1)
margin.table(is.na(Goldmpg1))
logret <- diff(log(Goldmpg1$GOLDPMGBD228NLBM))[-1]
margin.table(is.na(logret))
head(logret); tail(logret)


