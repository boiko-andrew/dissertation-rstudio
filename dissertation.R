
# Удаление всех переменных
rm(list = ls())

# Установка рабочей директории

# на домашнем компьютере
setwd("F:/Мои проекты/Диссертация/R Studio")

# на рабочем компьютере
#setwd("E:/Разное/Мои проекты/Диссертация/R Studio")


fileName <- "RC_F01_01_2009_T24_09_2020.xlsx"

install.packages("xlsx")
install.packages("xts")
install.packages("forecast")
install.packages("dplyr")

library(xlsx)
library(xts)
library(tseries)
library(forecast)
library(dplyr)
library(scatterplot3d)

dailyCourse <- read.xlsx(file = fileName, sheetName = "RC",
                         encoding = "UTF-8")

colnames(dailyCourse)[colnames(dailyCourse) == "data"] <- "date"
colnames(dailyCourse)[colnames(dailyCourse) == "curs"] <- "rate"

dailyCourse$nominal <- NULL
dailyCourse$cdx <- NULL

# Ежедневный курс
dailyCourseXts <- xts(x = dailyCourse$rate,
                      order.by = dailyCourse$date)
plot(dailyCourseXts)

# Ежемесячный курс (среднее значение по дням)
monthlyCourseXts <- apply.monthly(dailyCourseXts, mean)
index(monthlyCourseXts) <- as.yearmon(index(monthlyCourseXts))
plot(monthlyCourseXts)

monthlyCourse <- ts(monthlyCourseXts, frequency = 12,
                    start = index(monthlyCourseXts)[1])
monthlyCourse <- monthlyCourse[ ,1]

plot(monthlyCourse)


# Ежегодный курс (среднее значение по годам)
yearlyCourseXts <- apply.yearly(dailyCourseXts, mean)
plot(yearlyCourseXts)

yearlyCourse <- ts(yearlyCourseXts, frequency = 1,
                   start = as.numeric(format(index(yearlyCourseXts)[1], "%Y"))) 
plot(yearlyCourse)


monthsQty <- as.integer(length(monthlyCourse))
yearsQty <- floor(length(monthlyCourse) / 12)
errorsQty <- 7 # ME, RMSE, MAE, MPE, MAPE, MASE, ACF1




# Сезонная декомпозиция

# Количество дифференцирований для превращения ряда в стационарный
# с точки зрения сезонности
nsdiffs(monthlyCourse)

# Количество дифференцирований для превращения ряда в стационарный
ndiffs(monthlyCourse)

# Собственно сезонная декомпозиция
# Построение графика
seasonplot(monthlyCourse, 12, col=rainbow(12), year.labels=TRUE,
           main="Seasonal plot: USD/RUB")

monthlyCourseStl <- stl(monthlyCourse, s.window = "periodic")
par(mfrow = c(1,1))
plot(monthlyCourseStl)













# Наивный прогноз
monthlyCourseNaive <- lag.xts(monthlyCourse, k = 1, na.pad = TRUE)

# Вычисление MAPE
errorsNaive <- matrix(rep(0, yearsQty * errorsQty),
                    nrow = yearsQty, ncol = errorsQty)

# ARIMA
windowWidth = 12  # ширина окна - 12 месяцев (1 год)

coefs <- c(0,0,0,0)
for (i in 1:(monthsQty - windowWidth + 1)){
  usd <- subset(monthlyCourse, start = i, end = (i + windowWidth - 1))
  arimaUsd <- auto.arima(usd)
  arimaCoefs <- arimaorder(arimaUsd)
  names(arimaCoefs) <- NULL
  arimaCoefs <- c(i, arimaCoefs)
  
  if (i == 1){
    coefs <- arimaCoefs
  }
  else{
    coefs <- rbind(coefs, arimaCoefs)
  }
}
rownames(coefs) <- NULL

coefs <- as.data.frame(coefs)

names(coefs)[1] <- "n"
names(coefs)[2] <- "p"
names(coefs)[3] <- "d"
names(coefs)[4] <- "q"

coefs$sum <- coefs$p + coefs$d + coefs$q

head(coefs)


par(mfrow = c(3,1))

plot(coefs[ ,1], coefs[ ,2])
plot(coefs[ ,1], coefs[ ,3])
plot(coefs[ ,1], coefs[ ,4])

# Частота встречаемости отдельных моделей
count(coefs, coefs$p, coefs$d, coefs$q)

# Частота встречаемости моделей с суммой порядков коэффициентов
count(coefs, coefs$sum)


par(mfrow = c(1,1))
scatterplot3d(coefs[ ,2:4])




my.nottem <- 














usd2009 <- subset(monthlyCourse, start =   1, end =  12)
arimaUsd2009 <- auto.arima(usd2009)

summary(arimaUsd2009)


as.integer(arimaorder(arimaUsd2009)["p"])

test["p"]
test["d"]
test["q"]


for (i in 1:yearsQty) {
  begin <- (i - 1) * 12 + 1
  end <- i * 12
  accuracyNaive <- accuracy(monthlyCourseNaive, monthlyCourse,
                            test = begin:end)
  errorsNaive[i, ] <- accuracyNaive
}

errorsNaive <- ts(errorsNaive, frequency = 1,
                  start = as.numeric(format(index(yearlyCourseXts)[1], "%Y")))
plot(errorsNaive[ , 5], xlab = "Years", ylab = "MAPE, %")










# Экспоненциальное сглаживание
forecastHorizon = 12

errorsES <- matrix(rep(0, yearsQty * errorsQty),
                   nrow = yearsQty, ncol = errorsQty)

alpha <- rep(0, yearsQty)

for (i in 1:yearsQty) {
  begin <- (i - 1) * 12 + 1
  end <- i * 12
  
  trainTS <- subset(monthlyCourse, start = begin, end = end)
  sesModel <- ses(trainTS)
  alpha[i] <- sesModel$model$par['alpha']
}
  
alpha  
  
for (i in 1:length(monthlyCourse)) {
  beg
}




  
  autoES <- ets(trainTS)
  
  if (i == 1) {
    monthlyCourseES <- trainTS
  } else {
    monthlyCourseES <- cbind(monthlyCourseES,
                             forecast(autoES, h = forecastHorizon)$)
  }
}

begin = 1
end = 12

trainTS <- subset(monthlyCourse, start = begin, end = end)

sesModel <- ses(trainTS)$model$par['alpha']


sesModel$model$par['alpha']







autoES <- ets(trainTS)

monthlyCourseES <- forecast(autoES, h = forecastHorizon)

plot(monthlyCourseES)

monthlyCourseES

  
  
  

# Экспоненциальное сглаживание
autoES <- ets(monthlyCourse)

# АРПСС
usd2009 <- subset(monthlyCourse, start =   1, end =  12)
usd2010 <- subset(monthlyCourse, start =  13, end =  24)
usd2011 <- subset(monthlyCourse, start =  25, end =  36)
usd2012 <- subset(monthlyCourse, start =  37, end =  48)
usd2013 <- subset(monthlyCourse, start =  49, end =  60)
usd2014 <- subset(monthlyCourse, start =  61, end =  72)
usd2015 <- subset(monthlyCourse, start =  73, end =  84)
usd2016 <- subset(monthlyCourse, start =  85, end =  96)
usd2017 <- subset(monthlyCourse, start =  97, end = 108)
usd2018 <- subset(monthlyCourse, start = 109, end = 120)

usd2009to2010 <- subset(monthlyCourse, start =  1, end =  24) 
usd2011to2012 <- subset(monthlyCourse, start = 25, end =  48)
usd2013to2014 <- subset(monthlyCourse, start = 49, end =  72)
usd2015to2016 <- subset(monthlyCourse, start = 73, end =  96)
usd2017to2018 <- subset(monthlyCourse, start = 97, end = 120)


arimaUsd2009 <- auto.arima(usd2009)
arimaUsd2009

arimaUsd2010 <- auto.arima(usd2010)
arimaUsd2010

arimaUsd2011 <- auto.arima(usd2011)
arimaUsd2011

arimaUsd2012 <- auto.arima(usd2012)
arimaUsd2012

arimaUsd2013 <- auto.arima(usd2013)
arimaUsd2013

arimaUsd2014 <- auto.arima(usd2014)
arimaUsd2014

arimaUsd2015 <- auto.arima(usd2015)
arimaUsd2015

arimaUsd2016 <- auto.arima(usd2016)
arimaUsd2016

arimaUsd2017 <- auto.arima(usd2017)
arimaUsd2017

arimaUsd2018 <- auto.arima(usd2018)
arimaUsd2018


arimaUsd2009to2010 <- auto.arima(usd2009to2010)
arimaUsd2009to2010

arimaUsd2011to2012 <- auto.arima(usd2011to2012)
arimaUsd2011to2012

arimaUsd2013to2014 <- auto.arima(usd2013to2014)
arimaUsd2013to2014





