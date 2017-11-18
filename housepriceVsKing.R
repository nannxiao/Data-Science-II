install.packages("zoo")
install.packages("xts")
library(zoo)
library(xts)
data <- read.csv("InfoTechEmployment_3Counties.csv", header = T,as.is = T)
data <- data[-2]
class(data)

data <- data.frame(t(data))
head(data)


data <- data[-1,]
tail(data)
data <- cbind(as.numeric(as.character(data[,1])),as.numeric(as.character(data[,2])),as.numeric(as.character(data[,3])))
######
dates <- seq(as.Date("2012-01-01"),as.Date("2016-06-01"), by="months")
dates <- as.yearmon(dates)
head(dates)
ts <- xts(data,dates)
colnames(ts) <- c("King","Snohomish","Pierce")

install.packages("forecast")
install.packages("lattice")
install.packages("quantmod")

# King County
king <- ts[,1]
fdiff <- diff(king)
sdiff <- diff(king,12)
fsdiff <- diff(diff(king),12)
Acf(fdiff)
plot(stl(fdiff,s.window = "periodic"), main = "King ")

# Snohomish
snoho <- ts[,2]
fdiff <- diff(snoho)
sdiff <- diff(snoho,12)
fsdiff <- diff(diff(snoho),12)
Acf(sdiff)
plot(stl(snoho,s.window = "periodic"), main = "Snohomish")


# Pierce
pierce <- ts[,3]
fdiff <- diff(pierce)
sdiff <- diff(pierce,12)
fsdiff <- diff(diff(pierce),12)
Acf(sdiff)
plot(stl(pierce,s.window = "periodic"), main = "Pierce")

house <- read.csv("County_Zhvi_AllHomes.csv", header = T, as.is = T)
sub1 <- house[house$State=="WA",]

sub2 <- sub1[sub1$RegionName=="King",]
head(sub2)
sub3 <- sub2[,-1:-7]
ave <- sub3

names(ave)
ave["X2012.01"]
ave <- t(ave)
head(ave)
as.numeric(ave)
dates <- seq(as.Date("1996-04-01"),as.Date("2016-12-01"), by="months")
dates <- as.yearmon(dates)
dates <- as.Date(as.yearmon(dates,"%Y-%m-%d"))
class(dates)
ts1 <- ts(ave,start = c(as.numeric(format(dates[1],"%Y")),as.numeric(format(dates[1],"%m"))),frequency = 12)
class(ts1)
house.price <- window(ts1,2012,c(2012,60))

house.dates <- seq(as.Date("2012-01-01"),as.Date("2016-12-01"), by="months")
house.dates <- as.yearmon(house.dates)


house.fdiff <- diff(house.price1)
house.sdiff <- diff(house.price1,12)
house.fsdiff <- diff(diff(house.price1),12)
Acf(house.fsdiff)


house.price1 <- xts(as.numeric(house.price),house.dates)

plot(stl(house.price1,s.window = "periodic"), main = "King County Home Value")

con <- cbind(as.numeric(house.price),as.numeric(king))
class(con)
mts <- ts(con,start = c(2012,1),frequency = 12)
fdiff.house <- diff(house.price)
sdiff.house <- diff(house.price,12)
plot(fdiff.house)
fsdiff.house <- diff(diff(house.price),12)


Acf(fdiff.house)
Acf(sdiff.house)
Acf(fsdiff.house)

sarima <- auto.arima(house.price, lambda = 0,approximation = F,stepwise = F)

Acf(sarima$residuals)
Pacf(sarima$residuals)

forecast <- forecast(sarima, h = 12, fan = T, bootstrap = T)
plot(forecast, main = "King County Home Price Forecasts from ARIMA(0,1,3)(1,0,0)[12]")
forecast$mean

merged <- merge(house.price1,king)

ccf(mts[,1],mts[,2])


fit <- Arima(house.price, order=c(0,1,3), seasonal=c(1,0,0))
tsdisplay(residuals(fit))


# Snoho House
