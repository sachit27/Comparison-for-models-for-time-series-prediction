setwd("/Users/sachitmahajan/Desktop/untitled")
dat <-read.csv("best3.csv")
library(forecast)
attach(dat)
newdata <- dat[1:620,] #only when subsetting data
pm25.ts <-ts(pm74DA3895C48C)

moving_average = forecast(ma(pm25.ts[1:690], order=2), h=5)
moving_average_accuracy = accuracy(moving_average, pm25.ts[691:721])
moving_average; moving_average_accuracy

plot(moving_average, ylim=c(0,100))
lines(tdat[1:690])

train = pm25.ts[1:600]
test = pm25.ts[601:620]
arma_fit <- auto.arima(train)
arma_forecast <- forecast(arma_fit, h = 5)
arma_fit_accuracy <- accuracy(arma_forecast, test)
 
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_fit$residuals)


legend('topright', c('observed signal', 'fitted signal'), col = 1:2, lty = 1)
(ann <-nnetar(train))
ann_forecast <- forecast(ann, h = 5)
ann_fit_accuracy <- accuracy(ann_forecast, test)
ann; ann_forecast; ann_fit_accuracy

holtw <-HoltWinters(train, gamma=FALSE)
hw_forecast <- forecast(holtw, h = 5)
holt_fit_accuracy <- accuracy(hw_forecast, test)
holtw; hw_forecast; holt_fit_accuracy
plot.forecast(hw_forecast)





