# get yahoo GSPC data
#install.packages("quantmod")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("astsa")
install.packages("ggplot2")
install.packages("lubridate")

library(quantmod)
library(lubridate)
SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = "1980-01-01")
SPX = as.data.frame(SPX, stringsAsFactors=FALSE)
SPX$TimeStamp = row.names(SPX)
SPX$TimeStamp = ymd(SPX$TimeStamp)

str(SPX)
plot(SPX$GSPC.Open)

# ACF & PACF
library(tseries)
library(forecast)

acf(SPX$GSPC.Close, plot=TRUE, lag.max = NULL)
adf.test(SPX$GSPC.Close) # Augmented Dickey-Fuller Test
plot(SPX$GSPC.Close)

library(astsa)
acf2(SPX$GSPC.Close) # PACF
acf2(diff(SPX$GSPC.Close)) # PACD after diff

# Ljung Box Q
Box.test(SPX$GSPC.Close) # not stationary
Box.test(diff(SPX$GSPC.Close)) # not stationary, but X-square decrease

# Polynomial Regression
SPX = as.data.frame(SPX, stringsAsFactors=FALSE)
SPX$TimeStamp = row.names(SPX)
SPX$index = 1:length(SPX$TimeStamp)
model = lm(GSPC.Close ~ poly(index, 3), data = SPX)
summary(model)

ggplot(data=SPX, aes(x = TimeStamp))+
  geom_line(aes(y=predict(model), color="red")) +
  geom_line(aes(y = GSPC.Close))
