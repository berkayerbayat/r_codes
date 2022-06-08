library(TTR)
library(tseries)
library(lmtest)
library(forecast)
library(tidyverse)


exp <- read.csv2("/datasets/exp-imp.csv")

exp <- exp[-c(2,3,4,7,25,45,46,81,82,103,104,137,138,184,185,227,228,231,
              232,233,234,235,236,237,238),]
row.names(exp) <- exp[,1]
exp <- exp[,-1]
exp <- na.omit(exp)

new_df <- t(exp)
row.names(new_df) <- NULL


exports <- new_df[1:82,]
imports <- new_df[83:164,]

exports_ts <- ts(exports, start = (2014), frequency = 12)


d <- c()
m <- data.frame(row.names = c('AR', 'MA', 'SAR', 'SMA', 'FRE', 'DIF','SDIF'))
a <- 1

# Auto arima loop for our dataset
while (a<212) {
  d <- auto.arima(exports_ts[,a])
  m[,a] <- d$arma
  d <- c()
  a <- a+1
}

m <- t(m)

row.names(m) <- colnames(new_df)
head(m)


SEAS <- (m[,'SAR'] == 0 ) * (m[,'SMA'] == 0) * (m[,'SDIF'] == 0)
m <- cbind(m,SEAS)

c <- subset(as.data.frame(m),(SEAS==1))
dim(c)
head(c)

imf <- exports_ts[,'Aruba, Kingdom of the Netherlands']


plot.ts(imf)
plot(decompose(imf))
ggseasonplot(imf)


imf_SMA_5 = SMA(imf, n = 5)
plot.ts(imf_SMA_5)

imf_SMA_12 = SMA(imf, n = 12)
plot.ts(imf_SMA_12)

imf_dc = decompose(imf, type = "multiplicative")
plot(imf)
plot(imf_dc)

par(mfrow=c(2,1))
imf_dc_new = imf/(imf_dc$trend)
plot(imf_dc_new)

imf_dc_new = imf/(imf_dc$trend)*(imf_dc$seasonal)
plot(imf_dc_new)

imf_hw = HoltWinters(imf, gamma = F)
plot(imf_hw)
imf_hw

imf_hw$SSE

# Forecast
imf_hw_fc = forecast(imf_hw,h=5)
plot(imf_hw_fc)


par(mfrow=c(2,2))
acf(imf)
pacf(imf)
adf.test(imf)
imf_1 <- diff(imf,1)
acf(imf_1)
pacf(imf_1)
adf.test(imf_1)

arima_3.1.2
coeftest(arima_3.1.2)


par(mfrow=c(1,2))
qqnorm(residuals(arima_3.1.2)); qqline(residuals(arima_3.1.2))

acf(residuals(arima_3.1.2))
par(mfrow=c(1,1))

tsdiag(arima_3.1.2)

checkresiduals(arima_3.1.2, test=FALSE)

shapiro.test(arima_3.1.2$residuals)

Box.test(residuals(arima_3.1.2), lag = 6, type = "Ljung")
Box.test(residuals(arima_3.1.2), lag = 12, type = "Ljung")
Box.test(residuals(arima_3.1.2), lag = 18, type = "Ljung")
Box.test(residuals(arima_3.1.2), lag = 24, type = "Ljung")


plot(forecast(arima_3.1.2, h =5))
forecast(arima_3.1.2, h = 5)
accuracy(arima_3.1.2)
