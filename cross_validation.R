setwd("C:/Users/Bob/Documents/Capstone_Writing/")
file <- "./index/data/english_tweets_with_sentiment.csv"

df <- read.csv(file)
colnames(df)
library(dplyr)
library(lubridate)
library(tidyr)
library(forecast)
library(caret)

min_date <- as.Date('2007-01-01')
max_date <- as.Date('2019-03-01')

bbq_sentiment <- df%>%
  filter(bbq == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

#"pasta"         
pasta_sentiment <- df%>%
  filter(pasta == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

#"salsa"         
salsa_sentiment <- df%>%
  filter(salsa == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

#"tomato"
tomato_sentiment <- df%>%
  filter(tomato == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

#Pizza
pizza_sentiment <- df%>%
  filter(pizza == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

ketchup_sentiment <- df%>%
  filter(ketchup == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

chili_sentiment <- df%>%
  filter(chili == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

spaghetti_sentiment <- df%>%
  filter(spaghetti == 1)%>%
  mutate(M = month(created_at))%>%
  mutate(Y = year(created_at))%>%
  mutate(dt = paste(Y,M,"01",sep="-"))%>%
  mutate(Date = as.Date(dt)) %>%
  mutate(retweet_count = if_else(retweet_count<1,1,retweet_count)) %>%
  mutate(Index = polarity * subjectivity * retweet_count) %>%
  group_by(Date) %>%
  summarise(Index = mean(Index)) %>%
  complete(Date = seq.Date(min_date,max_date,by="month"))%>%
  mutate(Index = if_else(is.na(Index),mean(Index,na.rm = T),Index))

bbq_ts     <- ts(bbq_sentiment$Index,start = c(2007,1),frequency = 12)
chili_ts   <- ts(chili_sentiment$Index,start = c(2007,1),frequency = 12)
ketchup_ts <- ts(ketchup_sentiment$Index,start = c(2007,1),frequency = 12)
pasta_ts   <- ts(pasta_sentiment$Index,start = c(2007,1),frequency = 12)
pizza_ts   <- ts(pizza_sentiment$Index,start = c(2007,1),frequency = 12)
salsa_ts   <- ts(salsa_sentiment$Index,start = c(2007,1),frequency = 12)
spaghetti_ts <- ts(spaghetti_sentiment$Index,start = c(2007,1),frequency = 12)
tomato_ts <- ts(tomato_sentiment$Index,start = c(2007,1),frequency = 12)


twitter_cols <- cbind(bbq = bbq_ts,
                      chili = chili_ts,
                      ketchup = ketchup_ts,
                      pasta = pasta_ts,
                      pizza = pizza_ts,
                      salsa = salsa_ts,
                      spaghetti = spaghetti_ts,
                      tomato = tomato_ts
)

preObj <- preProcess(twitter_cols, method=c("center", "scale"))
standardized_twitter <- predict(preObj, twitter_cols)
head(standardized_twitter)

df_ts <- readRDS("C:/Users/Bob/Desktop/Capstone/Consolidate_Tomato_TS.rds")
bbq_ccf <- Ccf(df_ts,standardized_twitter[,'bbq'])
chili_ccf <- Ccf(df_ts,standardized_twitter[,'chili'])
ketchup_ccf <- Ccf(df_ts,standardized_twitter[,'ketchup'])
pasta_ccf <- Ccf(df_ts,standardized_twitter[,'pasta'])
pizza_ccf <- Ccf(df_ts,standardized_twitter[,'pizza'])
salsa_ccf <- Ccf(df_ts,standardized_twitter[,'salsa'])
spaghetti_ccf <- Ccf(df_ts,standardized_twitter[,'spaghetti'])
tomato_ccf <- Ccf(df_ts,standardized_twitter[,'tomato'])

df_lags <- cbind(bbq = bbq_ccf$acf,
                 chili = chili_ccf$acf,
                 ketchup = ketchup_ccf$acf,
                 pasta = pasta_ccf$acf,
                 pizza = pizza_ccf$acf,
                 salsa = salsa_ccf$acf,
                 spaghetti = spaghetti_ccf$acf,
                 tomato = tomato_ccf$acf)
lags <- c(-24:24)
rownames(df_lags) <- lags

library(reshape2)
df_hm <- melt(df_lags)
head(df_hm)

library(ggplot2)

df_hm %>%
  filter(is.element(Var1,c(-9:-24)))%>%
  ggplot(aes(x=Var2,y=Var1))+
    geom_tile(aes(fill=value)) +
    scale_fill_distiller(palette = "Spectral")+
    xlab("Keyword")+
    ylab("Lag")+
    labs(title="Cross Correlation Analysis:Twitter Results")

row.names(df_hm_wide)<-c(-24:24)
df_hm %>%
  filter(Var1 < -9) %>%
  filter(abs(value)>0.2)
#best correlations higher than 0.2
#bbq -17
#
#pasta -18
#tomato -15


generate_lagged_ts <- function(ts,lag,st){
  ts_lag <- stats::lag(ts,lag)
  ts_lag
}

bbq_twitter_lag17   <- stats::lag(standardized_twitter[,'bbq'],-17)
#bbq_twitter_lag21   <- stats::lag(standardized_twitter[,'bbq'],-21)
#pasta_twitter_lag19 <- stats::lag(standardized_twitter[,'pasta'],-19)
pasta_twitter_lag18 <- stats::lag(standardized_twitter[,'pasta'],-18)
#tomato_twitter_lag10<- stats::lag(standardized_twitter[,'tomato'],-10)
tomato_twitter_lag15<- stats::lag(standardized_twitter[,'tomato'],-15)

####################
#Google Trends Data#
####################

googletrend<- readxl::read_excel("./index/data/Google_trend.xlsx")

googletrend[,'bbq']<-(googletrend[,'bbq']+googletrend[,'barbecue'])/2
googletrend<- googletrend[,1:9]
#normalize the columns
library(caret)
# Assuming goal class is column 10
preObj <- preProcess(googletrend[, -1], method=c("center", "scale"))
standardized_google <- predict(preObj, googletrend[, -1])
standardized_google$Month <- googletrend$Month

goog_ts <- ts(standardized_google[,-9],start = c(2004,1),frequency = 12)

bbq_goog_ccf <- Ccf(df_ts,goog_ts[,'bbq'])
chili_goog_ccf <- Ccf(df_ts,goog_ts[,'chili'])
ketchup_goog_ccf <- Ccf(df_ts,goog_ts[,'ketchup'])
pasta_goog_ccf <- Ccf(df_ts,goog_ts[,'pasta'])
pizza_goog_ccf <- Ccf(df_ts,goog_ts[,'pizza'])
salsa_goog_ccf <- Ccf(df_ts,goog_ts[,'salsa'])
spaghetti_goog_ccf <- Ccf(df_ts,goog_ts[,'spaghetti'])
tomato_goog_ccf <- Ccf(df_ts,goog_ts[,'tomato'])

df_lags_google <- cbind(bbq = bbq_goog_ccf$acf,
                        chili = chili_goog_ccf$acf,
                         ketchup = ketchup_goog_ccf$acf,
                         pasta = pasta_goog_ccf$acf,
                         pizza = pizza_goog_ccf$acf,
                         salsa = salsa_goog_ccf$acf,
                         spaghetti = spaghetti_goog_ccf$acf,
                         tomato = tomato_goog_ccf$acf)

lags <- c(-24:24)
rownames(df_lags_google) <- lags
df_lags_google
df_hm_goog <- melt(df_lags_google)
head(df_hm_goog)

df_hm_goog %>%
  filter(is.element(Var1,c(-9:-24)))%>%
  ggplot(aes(x=Var2,y=Var1))+
  geom_tile(aes(fill=value)) +
  scale_fill_distiller(palette = "Spectral")+
  xlab("Keyword")+
  ylab("Lag")+
  labs(title="Cross Correlation Analysis:Twitter Results")

df_hm_goog %>%
  filter(Var1 < -9) %>%
  filter(abs(value)>0.5)

#best correlations from each column
#google10
tail(goog_ts)
bbq_google10 <- stats::lag(goog_ts[,'bbq'],-10)
bbq_google10
#chili15
chili_google15 <- stats::lag(goog_ts[,'chili'],-15)
#ketchup16
#ketchup_google16 <- stats::lag(goog_ts[,'ketchup'],-16)
#pasta15
#pasta_google15 <- stats::lag(goog_ts[,'pasta'],-15)
#salsa12
salsa_google12 <- stats::lag(goog_ts[,'salsa'],-12)
#spaghetti9
#spaghetti_google14 <- stats::lag(goog_ts[,'spaghetti'],-14)
#google_tomato_3
tomato_google12 <- stats::lag(goog_ts[,'tomato'],-12)
tomato_google12
regressors <- cbind(bbq_twitter_lag17,pasta_twitter_lag18,tomato_twitter_lag15,
                    bbq_google10,chili_google15,salsa_google12,tomato_google12)
tail(regressors)
#earliest is sep 2002

best_arima <- auto.arima(window(df_ts,start=c(2010,1),end=c(2014,1)),seasonal = T,stepwise = F)
summary(best_arima)
fcast_best <- forecast(best_arima,h = 18)
autoplot(fcast_best)+
  autolayer(df_ts)
df_s <- as.data.frame(df_ts)
colnames(df_s)<- c('y')
df_s$y <- as.numeric(df_s$y)
df_s$DT <- seq.Date(as.Date("2010-01-01"),as.Date("2018-11-01"),by="month")
df_s$Month <- factor(lubridate::month(df_s$DT))
df_s$Year <- factor(lubridate::year(df_s$DT))
#remove DT column
df_s <- df_s[,c(1,3:4)]

r_df <- as.data.frame(window(regressors,start=c(2010,1),end=c(2018,11)))
#need to reduce the dataframe since the lagged variables only go so far forward in time
df_all <- as.data.frame(cbind(df_s,r_df))

df_all$Month <- as.factor(df_all$Month)
df_all$Year <- as.factor(df_all$Year)

str(df_all)
df_train <- df_all[1:89,c(1,4:10)]
df_test <- df_all[90:107,c(1,4:10)]

lm_all <- lm(y~.,data = df_train)
summary(lm_all)
lm_step <- step(lm_all,direction = "both")
summary(lm_step)

#months are significant and salsagoogle12 & tomatogoogle12
fcast_lm_step <- predict(lm_step,newdata = df_test)
fcast_lm_step
test <- df_s[90:107,1]
custom_smape(test,fcast_lm_step)
#38
resids <- test-fcast_lm_step
checkresiduals(resids)
resids_train <- lm_step$residuals
checkresiduals(resids_train)
str(resids_train)
#figure out the arima model that fits the residuals
aa <- auto.arima(ts(resids_train,start=c(2010,1),frequency=12),stepwise = F)
checkresiduals(aa)

k <- 12*4 # minimum data lengdth for fitting a model: 4 years
n <- length(df_ts) # Number of data points
p <- 12 ### Period
H <- 18 # Forecast Horiz
n_iters <- n-k
st <- tsp(df_ts)[1]+(k-2)/p

#sliding window save
e_arima_test      <- matrix(NA,n_iters,H)
e_arima_train     <- matrix(NA,n_iters,48)

e_arima_reg_test  <- matrix(NA,n_iters,H)
e_arima_reg_train  <- matrix(NA,n_iters,48)

smape_arima_test  <- matrix(NA,n_iters,1)
smape_arima_train <- matrix(NA,n_iters,1)

smape_arima_reg_test <- matrix(NA,n_iters,1)
smape_arima_reg_train <- matrix(NA,n_iters,1)

acc_arima_test <- matrix(NA,n_iters,1)
acc_arima_reg_test <- matrix(NA,n_iters,1)

acc_arima_train <- matrix(NA,n_iters,1)
acc_arima_reg_train <- matrix(NA,n_iters,1)

bias_arima_test <- matrix(NA,n_iters,1)
bias_arima_train <- matrix(NA,n_iters,1)

bias_arima_reg_test <- matrix(NA,n_iters,1)
bias_arima_reg_train<- matrix(NA,n_iters,1)

fcast_best_arima <- matrix(NA,n_iters,1)
fcast_best_arima_reg <- matrix(NA,n_iters,1)

#best_arima_model <- auto.arima(window(df_ts,start=c(2010,1),end=c(2017,5)),stepwise = F)
#(0,0,0),(1,1,0)[12]
#f <- forecast(best_arima_model,h=18)
#t <- window(df_ts,start=c(2017,6))
#custom_smape(t,f$mean) #35%

#window(df_ts,start=c(2017,6))

for(i in 1:n_iters){
  print(paste('iteration:',i))
  #test is re-used between both methods
  test <- window(df_ts, start=st + (i+1)/p, end=st + (i+H)/p)
  #Sliding window
  train      <- window(df_ts,start = st + (i-k+1)/p, end=st + i/p)
  #updated to best arima from the whole data set
  fit_1_arima <- Arima(train,order=c(0,0,0),seasonal = list(order=c(1,1,0),period=p),include.drift=TRUE)
  fcast_1_arima <- forecast(fit_1_arima,h=H)
  y_fcast <- as.vector(fcast_1_arima$mean)
  n_test <- length(test)
  y_fcast <- y_fcast[1:n_test]
  i_H <- i+n_test-1
  #get error
  e_arima_test[i,1:n_test] <- y_fcast - test
  e_arima_train[i,1:48] <- fit_1_arima$residuals
  #get smape
  smape_arima_test[i,1] <- custom_smape(act = test,pred = y_fcast)
  smape_arima_train[i,1]<- custom_smape(act = train,pred = fit_1_arima$fitted)
  #get accuracy
  acc_arima_test[i,1] <- (1/n_test)*sum((1-((test-y_fcast)/y_fcast)))
  acc_arima_train[i,1] <- (1/n) * sum((1-((train - fit_1_arima$fitted)/fit_1_arima$fitted)))
  #get bias
  bias_arima_test[i,1] <- sum(y_fcast>test)/n_test
  bias_arima_train[i,1] <- sum(fit_1_arima$fitted>train)/n
  #get the best prediction (overwrite with the new data)
  fcast_best_arima[i:i_H,1] <- y_fcast
}
mean(smape_arima_test,na.rm=T)#18 month window
#38.6
mean(smape_arima_train,na.rm=T)
#27.5
mean(sqrt(colMeans(e_arima_test^2,na.rm = T)))
#56929
mean(sqrt(colMeans(e_arima_train^2,na.rm = T)))
#34182
mean(bias_arima_test,na.rm = T)
#0.53
mean(bias_arima_train,na.rm=T)
#0.18
mean(acc_arima_test,na.rm=T)
#2.55
mean(acc_arima_train,na.rm=T)
#0.58

###########################
#Regression w/ Arima Error#
###########################
st <- tsp(df_ts)[1]+(k-2)/p
regressors_lm_step <-  regressors[,c('bbq_google10','chili_google15',
                                     'salsa_google12','tomato_google12')]
for(i in 1:n_iters){
  print(paste('iteration:',i))
  #test is re-used between both methods
  test <- window(df_ts, start=st + (i+1)/p, end=st + (i+H)/p)
  #need the test values for the regression as well
  test_all_reg <- window(regressors_lm_step, start=st + (i+1)/p, end=st + (i+H)/p)
  #Sliding window
  train      <- window(df_ts,start = st + (i-k+1)/p, end=st + i/p)
  train_all_reg <- window(regressors_lm_step,start = st + (i-k+1)/p, end=st + i/p)
  fit_1_arima_reg <- Arima(train,order = c(1,0,0),seasonal = list(order=c(0,1,2),period=p),xreg = train_all_reg,include.drift=TRUE)
  fcast1_arima_reg <- forecast(fit_1_arima_reg,xreg = test_all_reg,h=H)
  y_fcast <- as.vector(fcast1_arima_reg$mean)
  n_test <- length(test)
  y_fcast <- y_fcast[1:n_test]
  i_H <- i+n_test-1
  fcast_best_arima_reg[i:i_H,1] <- y_fcast
  #get error
  e_arima_reg_test[i,1:n_test] <- y_fcast - test
  e_arima_reg_train[i,1:48] <- fit_1_arima$residuals
  #get smape
  smape_arima_reg_test[i,1] <- custom_smape(act = test,pred = y_fcast)
  smape_arima_reg_train[i,1]<- custom_smape(act = train,pred = fit_1_arima_reg$fitted)
  #get accuracy
  acc_arima_reg_test[i,1] <- (1/n_test)*sum((1-((test-y_fcast)/y_fcast)))
  acc_arima_reg_train[i,1] <- (1/n) * sum((1-((train - fit_1_arima_reg$fitted)/fit_1_arima_reg$fitted)))
  #get bias
  bias_arima_reg_test[i,1] <- sum(y_fcast>test)/n_test
  bias_arima_reg_train[i,1] <- sum(fit_1_arima_reg$fitted>train)/n
  #get the best prediction (overwrite with the new data)
  
}

mean(smape_arima_reg_test,na.rm=T)#18 month window
#43.9
mean(smape_arima_reg_train,na.rm=T)
#25
mean(sqrt(colMeans(e_arima_reg_test^2,na.rm = T)))
#68342.19
mean(sqrt(colMeans(e_arima_reg_train^2,na.rm = T)))
#93857.76
mean(bias_arima_reg_test,na.rm = T)
#0.58
mean(bias_arima_reg_train,na.rm=T)
#0.20
mean(acc_arima_reg_test,na.rm=T)
#-0.93
mean(acc_arima_reg_train,na.rm=T)
#0.38

best_out_arima_ts <-ts(fcast_best_arima,start =c(2014,1),frequency = 12)
autoplot(best_out_arima_ts)

best_out_best_arima_reg_ts <- ts(fcast_best_arima_reg,start=c(2014,1),frequency = 12)
autoplot(best_out_best_arima_reg_ts)
window(df_ts,start=c(2014,1))
best_out_arima_ts
best_out_best_arima_reg_ts

plot(1:length(smape_1_arima),smape_1_arima)
plot(1:length(smape_1_arima_reg),smape_1_arima_reg)

###############
#Random Forest#
###############

library(randomForest)

e_rf_test      <- matrix(NA,n_iters,H)
e_rf_train     <- matrix(NA,n_iters,48)
smape_rf_train <- matrix(NA,n_iters,1)
smape_rf_test  <- matrix(NA,n_iters,1)
acc_rf_test    <- matrix(NA,n_iters,1)
acc_rf_train   <- matrix(NA,n_iters,1)
bias_train     <- matrix(NA,n_iters,1)
bias_test      <- matrix(NA,n_iters,1)

fcast_best_RF  <- matrix(NA,n_iters,1)


head(df_all)
k <- 12*4 # minimum data length for fitting a model: 4 years
n <- nrow(df_all) # Number of data points
p <- 12 ### Period
H <- 18 # Forecast Horiz
n_iters <- n-k #partial prediction windows
#n_iters <- n-k-H #full prediction windows
st <- 1

#full prediction window

for(i in 1:n_iters){
  #test is re-used between both methods
  print(paste('iteration:',i))
  st_test <- i+k
  end_test <- i+k+H-1
  test <- df_all[st_test:end_test,]
  print(paste('st_test',st_test))
  print(paste('end_test',end_test))
  #Sliding window
  st_train <- i
  end_train <- i + k -1 
  print(paste('st_train',st_train))
  print(paste('end_train',end_train))
  train <- df_all[st_train:end_train,]
  
  #do random forest
  rf <- randomForest(y~.,data=train,ntree=500,importance=T)
  #forecast
  fcast_rf <- predict(rf,newdata = test)
  #convert to vector dropping na's
  y_fcast <- as.vector(fcast_rf)
  n_test <- length(test)
  y_fcast <- y_fcast[1:n_test]
  i_H <- i+n_test-1
  fcast_best_RF[i:i_H,1] <- y_fcast

    #get out the x values for train & test
  train_y <- as.vector(train[,'y'])
  rf_y <- as.vector(rf$predicted)
  
  test_y <- test[,'y']
  #test_y <- as.vector(test_y[!is.na(test_y)])
  #need length of test set
  n_test <- length(test_y)
  print(length(train_y-rf_y))

  #calculate error
  e_rf_test[i,1:n_test] <- test_y-y_fcast
  e_rf_train[i,1:k] <- train_y-rf_y
  #calculate smapes
  smape_rf_test[i,1]  <- custom_smape(test_y,y_fcast)
  smape_rf_train[i,1] <- custom_smape(train_y,rf_y)
  #calculate accuracy
  acc_rf_test[i,1] <- (1/n_test) * sum((1-((test_y - y_fcast)/test_y)))
  acc_rf_train[i,1] <- (1/n) * sum((1-((train_y - rf_y)/train_y)))
  #calculate bias
  bias_test[i,1] <- sum(y_fcast>test_y)/n_test
  bias_train[i,1] <- sum(rf_y>train_y)/n
  print('fcast_rf:')
}
best_out_best_RF_ts <- ts(fcast_best_RF,start=c(2014,1),frequency = 12)
autoplot(best_out_best_RF_ts)+autolayer(df_ts)

mean(sqrt(colMeans(e_rf_train^2,na.rm = T)))
#64996.74
mean(sqrt(colMeans(e_rf_test^2,na.rm=T)))
#165087.7
mean(smape_rf_train,na.rm = T)
#37%
mean(smape_rf_test,na.rm=T)
#47%
mean(acc_rf_test,na.rm=T)
#22.6
mean(acc_rf_train,na.rm=T)
#2.85
mean(bias_test,na.rm=T)
#0.62
mean(bias_train,na.rm=T)
#0.29
# write.csv()
# write.csv(df_all,"df_all.csv")
# nrow(df_all)
# train_rf <- df_all[1:88,c(1:9)]
# train_rf
# test_rf <- df_all[89:107,c(1:9)]
# test_rf
# rf <- randomForest(y~.,data=train_rf,ntree=500,importance=T)
# y_hat_rf <- predict(rf,test_rf)
# varImpPlot(rf)

#########
#xgboost#
#########

library(xgboost)
library(caret)
dmy <- dummyVars("y ~ .",data = df_all,fullRank = T)
df_ohe <- data.frame(predict(dmy,newdata = df_all))
df_ohe$y
str(df_ohe)

e_xgb_test      <- matrix(NA,n_iters,H)
e_xgb_train     <- matrix(NA,n_iters,48)
smape_xgb_train <- matrix(NA,n_iters,1)
smape_xgb_test  <- matrix(NA,n_iters,1)
acc_xgb_test    <- matrix(NA,n_iters,1)
acc_xgb_train   <- matrix(NA,n_iters,1)
bias_xgb_train     <- matrix(NA,n_iters,1)
bias_xgb_test      <- matrix(NA,n_iters,1)

fcast_best_xgb  <- matrix(NA,n_iters,1)

k <- 12*4 # minimum data length for fitting a model: 4 years
n <- nrow(df_all) # Number of data points
p <- 12 ### Period
H <- 18 # Forecast Horiz
n_iters <- n-k #partial prediction windows
#n_iters <- n-k-H #full prediction windows
st <- 1

#full prediction window

for(i in 1:n_iters){
  #test is re-used between both methods
  print(paste('iteration:',i))
  st_test <- i+k
  end_test <- i+k+H-1
  test <- df_ohe[st_test:end_test,]
  test_m <- as.matrix(test)
  test_y <-df_all[st_test:end_test,'y']
  test_y <- test_y[!is.na(test_y)]

  #Sliding window
  st_train <- i
  end_train <- i + k -1 
  train <- df_ohe[st_train:end_train,]
  train_m <- as.matrix(train)
  train_y <- df_all[st_train:end_train,1]
  #do xgboost
  xgb <- xgboost(data=train_m,
                 label=train_y,
                 objective="reg:linear",
                 nrounds = 6,
                 verbose = 0)
  #forecast
  fcast_xgb <- predict(xgb,newdata = test_m)
  
  xgb_y <- predict(xgb,newdata=train_m)
  y_fcast <- as.vector(xgb_y)
  n_test <- length(test_y)
  y_fcast <- y_fcast[1:n_test]
  i_H <- i+n_test-1
  fcast_best_xgb[i:i_H,1] <- y_fcast
  #calculate error
  e_xgb_test[i,1:n_test] <- test_y-y_fcast
  e_xgb_train[i,1:k] <- train_y-xgb_y
  #calculate smapes
  smape_xgb_test[i,1]  <- custom_smape(test_y,y_fcast)
  smape_xgb_train[i,1] <- custom_smape(train_y,xgb_y)
  #calculate accuracy
  acc_xgb_test[i,1] <- (1/n_test) * sum((1-((test_y - y_fcast)/test_y)))
  acc_xgb_train[i,1] <- (1/n) * sum((1-((train_y - xgb_y)/train_y)))
  #calculate bias
  bias_xgb_test[i,1] <- sum(y_fcast>test_y)/n_test
  bias_xgb_train[i,1] <- sum(xgb_y>train_y)/n
}

mean(sqrt(colMeans(e_xgb_test^2,na.rm = T)))
#78894.12
mean(sqrt(colMeans(e_xgb_train^2,na.rm = T)))
#55729.68    
mean(smape_xgb_train,na.rm = T)
#16.14792
mean(smape_xgb_test,na.rm=T)
#38.04534
mean(acc_xgb_test,na.rm = T)
#2.9
mean(acc_xgb_train,na.rm = T)  
#0.53
mean(bias_xgb_train,na.rm = T) 
#12%
mean(bias_xgb_test,na.rm=T)
#44%

best_out_best_xgb_ts <- ts(fcast_best_xgb,start=c(2014,1),frequency = 12)
autoplot(best_out_best_xgb_ts)+autolayer(df_ts)


#########
#Prophet#
#########
library(prophet)
df_all$ds <- seq.Date(as.Date("2010/01/01"),as.Date("2018/11/01"),by="month")
colnames(df_for_prophet) <- c("ds","y")
df_for_prophet <- df_all[c('ds','y')]
df_for_prophet
k <- 12*4 # minimum data length for fitting a model: 4 years
n <- nrow(df_all) # Number of data points
p <- 12 ### Period
H <- 18 # Forecast Horiz
n_iters <- n-k #partial prediction windows
#n_iters <- n-k-H #full prediction windows
st <- 1

e_p_test      <- matrix(NA,n_iters,H)
e_p_train     <- matrix(NA,n_iters,48)
smape_p_train <- matrix(NA,n_iters,1)
smape_p_test  <- matrix(NA,n_iters,1)
acc_p_test    <- matrix(NA,n_iters,1)
acc_p_train   <- matrix(NA,n_iters,1)
bias_p_train     <- matrix(NA,n_iters,1)
bias_p_test      <- matrix(NA,n_iters,1)

fcast_best_p  <- matrix(NA,n_iters,1)

for(i in 1:n_iters){
  #test is re-used between both methods
  print(paste('iteration:',i))
  st_test <- i+k
  end_test <- i+k+H-1
  test <- df_for_prophet[st_test:end_test,]
  test_y <- test[,'y']
  test_y <- test_y[!is.na(test_y)]
  #Sliding window
  st_train <- i
  end_train <- i + k -1 
  train <- df_for_prophet[st_train:end_train,]
  train_y <- train[,'y']
  #do prophetlogistic
  pro <- prophet(weekly.seasonality = F,daily.seasonality = F)
  pro <- fit.prophet(pro,train)
  test_df <- make_future_dataframe(pro,periods = 18,freq='month',include_history = F)
  
  fcast_pro_all <- predict(pro,test_df)
  fcast_pro <- fcast_pro_all$yhat
  y_fcast <- as.vector(fcast_pro)
  n_test <- length(test_y)
  y_fcast <- y_fcast[1:n_test]
  i_H <- i+n_test-1
  fcast_best_p[i:i_H,1] <- y_fcast
  
  pro_y_all <- predict(pro,train)
  pro_y <- pro_y_all$yhat

  #need length of test set
  n_test <- length(test_y)
  #calculate error
  e_p_test[i,1:n_test] <- test_y-y_fcast
  e_p_train[i,1:k] <- train_y-pro_y
  #calculate smapes
  smape_p_test[i,1]  <- custom_smape(test_y,y_fcast)
  smape_p_train[i,1] <- custom_smape(train_y,pro_y)
  #calculate accuracy
  acc_p_test[i,1] <- (1/n_test) * sum((1-((test_y - y_fcast)/test_y)))
  acc_p_train[i,1] <- (1/n) * sum((1-((train_y - pro_y)/train_y)))
  #calculate bias
  bias_p_test[i,1] <- sum(y_fcast>test_y)/n_test
  bias_p_train[i,1] <- sum(pro_y>train_y)/n
}
mean(sqrt(colMeans(e_p_test^2,na.rm = T)))
#61389.38
mean(sqrt(colMeans(e_p_train^2,na.rm=T))) 
#30658.5
mean(smape_p_train,na.rm = T)
#31%
mean(smape_p_test,na.rm=T) 
#46%
mean(acc_p_test,na.rm=T)
#2.219468
mean(acc_p_train,na.rm=T)
#0.8814977
mean(bias_p_train,na.rm=T)    
#23%
mean(bias_p_test,na.rm=T)
#52%

best_out_best_p_ts <- ts(fcast_best_p,start=c(2014,1),frequency = 12)
autoplot(best_out_best_p_ts)+autolayer(window(df_ts,start=c(2014,1)))

###################
#combine forecasts#
###################
y_act       <- window(df_ts,start = c(2014,1))
y_arima     <- ts(fcast_best_arima,start=c(2014,1),frequency = 12)
y_arima_reg <- ts(fcast_best_arima_reg,start=c(2014,1),frequency = 12)
y_RF        <- ts(fcast_best_RF,start=c(2014,1),frequency = 12)
y_xgb       <- ts(fcast_best_xgb,start=c(2014,1),frequency = 12)
y_pro       <- ts(fcast_best_p,start=c(2014,1),frequency = 12)
y_agg <- cbind(arima = y_arima,
               arima_reg = y_arima_reg,
               RF = y_RF,
               xgb = y_xgb,
               pro = y_pro)
y_agg <- as.data.frame(y_agg)
y_agg['tot_avg'] = (y_agg[,'arima']+y_agg[,'arima_reg']+y_agg[,'RF']+y_agg[,'xgb']+y_agg['pro'])/5
y_agg['arima+arima_reg'] = (y_agg[,'arima']+y_agg[,'arima_reg'])/2
y_agg['arima+RF'] = (y_agg[,'arima']+y_agg[,'RF'])/2
y_agg['arima+xgb'] = (y_agg[,'arima']+y_agg[,'xgb'])/2
y_agg['arima+pro'] = (y_agg[,'arima']+y_agg[,'pro'])/2
y_agg['arima_reg+RF'] = (y_agg[,'arima_reg']+y_agg[,'RF'])/2
y_agg['arima_reg+xgb'] = (y_agg[,'arima_reg']+y_agg[,'xgb'])/2
y_agg['arima_reg+pro'] = (y_agg[,'arima_reg']+y_agg[,'pro'])/2
y_agg['RF+xgb'] = (y_agg[,'RF']+y_agg[,'xgb'])/2
y_agg['RF+pro'] = (y_agg[,'RF']+y_agg[,'pro'])/2
y_agg['xgb+pro'] = (y_agg[,'xgb']+y_agg[,'pro'])/2
y_agg['arima+arima_reg+RF'] = (y_agg[,'arima']+y_agg[,'arima_reg']+y_agg[,'RF'])/3
y_agg['arima+arima_reg+xgb'] = (y_agg[,'arima']+y_agg[,'arima_reg']+y_agg[,'xgb'])/3
y_agg['arima+arima_reg+pro'] = (y_agg[,'arima']+y_agg[,'arima_reg']+y_agg[,'pro'])/3
y_agg['arima_reg+RF+xgb'] = (y_agg[,'arima_reg']+y_agg[,'RF']+y_agg[,'xgb'])/3
y_agg['arima_reg+RF+pro'] = (y_agg[,'arima_reg']+y_agg[,'RF']+y_agg[,'pro'])/3
y_agg['RF+xgb+pro'] = (y_agg[,'RF']+y_agg[,'xgb']+y_agg[,'arima_reg'])/3

y_act_22 <- as.data.frame(cbind(y_act,y_act,y_act,y_act,y_act,
                                y_act,y_act,y_act,y_act,y_act,
                                y_act,y_act,y_act,y_act,y_act,
                                y_act,y_act,y_act,y_act,y_act,
                                y_act,y_act))
sqrt(colSums((y_agg - y_act_22)^2))
min(sqrt(colSums((y_agg - y_act_22)^2)))
#arima+RF + RF does best on all months RMSE = 374412.8
custom_smape(y_act,y_agg[,'arima+RF'])
#35.0%
custom_smape(y_act,y_agg[,'arima+arima_reg+RF'])
#34.8%
colnames(y_agg)
smape_summary = data.frame(colnames(y_agg))
smape_vals = matrix(NA,22,4)
smape_vals
for(i in 1:22){
  cname <- colnames(y_agg[i])
  cs <- custom_smape(y_act,y_agg[,i])
  rmse <- sqrt(sum((y_act-y_agg[,i])^2))
  print(paste(cname,cs,sep = ":"))
  acc <- (1/length(y_act)) * sum((1-((y_act - y_agg[,i])/y_act)))
  bias <- sum(y_agg[,i]>y_act)/length(y_act)
  vals <- c(cs,rmse,bias,acc)
  print(vals)
  smape_vals[i,1:4]<-as.vector(vals)
}
smape_summary$smape <- smape_vals[,1]
smape_summary$rmse <- smape_vals[,2]
smape_summary$bias <- smape_vals[,3]
smape_summary$acc <- smape_vals[,4]
smape_summary

autoplot(ts(y_agg['arima+RF'],start=c(2014,1),frequency = 12))+
  autolayer(window(df_ts,start=c(2014,1)))

Months <- rep(1:12,5)
y_agg['Month'] <- Months[1:59]
y_act <- as.data.frame(y_act)
y_act['Month'] <- Months[1:59]
y_agg_harvest <- y_agg[is.element(y_agg[,'Month'],c(6:10)),]
y_act_harvest <- y_act[is.element(y_act[,'Month'],c(6:10)),]
y_act_harvest_22 <- cbind(y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],
                          y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],
                          y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],
                          y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],y_act_harvest[,1],
                          y_act_harvest[,1],y_act_harvest[,1])
#all prediction months
sqrt(colSums((y_agg_harvest[,1:22] - y_act_harvest_22)^2))
min(sqrt(colSums((y_agg_harvest[,1:22] - y_act_harvest_22)^2)))
#Regresssion w/ Arima error + RF does best on just harvest months too RMSE = 335523.5
harvest_summary = data.frame(colnames(y_agg_harvest[1:22]))
harvest_vals = matrix(NA,22,4)
for(i in 1:22){
  cname <- colnames(y_agg_harvest[i])
  cs <- custom_smape(y_act_harvest[,1],y_agg_harvest[,i])
  rmse <- sqrt(sum((y_act_harvest[,1]-y_agg_harvest[,i])^2))
  print(paste(cname,cs,sep = ":"))
  acc <- (1/length(y_act_harvest[,1])) * sum((1-((y_act_harvest[,1] - y_agg_harvest[,i])/y_act_harvest[,1])))
  bias <- sum(y_agg_harvest[,i]>y_act_harvest[,1])/length(y_act_harvest[,1])
  vals <- c(cs,rmse,bias,acc)
  print(vals)
  harvest_vals[i,1:4]<-as.vector(vals)
}
harvest_summary$smape <- harvest_vals[,1]
harvest_summary$rmse <- harvest_vals[,2]
harvest_summary$bias <- harvest_vals[,3]
harvest_summary$acc <- harvest_vals[,4]
harvest_summary
#Arima + RF again performs the best - RMSE = 373956.8

sqrt(colSums((y_agg_harvest[10:25,1:14] - y_act_harvest_22[10:25])^2))
min(sqrt(colSums((y_agg_harvest[10:25,1:14] - y_act_harvest_14[10:25])^2)))

df_harvest_all <- data.frame(y_act_harvest)
df_harvest_all

#blend all predictions w/ a linear model
lm_all <- lm(y~., data=df_harvest_all)
summary(lm_all)
plot(lm_all$residuals)
y_hat_all <- lm_all$fitted.values
length(y_hat_all)
length(y_act_harvest[,1])
custom_smape(y_act_harvest[,1],y_hat_all)
#
lm_rmse <- sqrt(sum((y_act_harvest[,1]-y_hat_all)^2))
#297958.3
lm_smape <- custom_smape(y_act_harvest[,1],y_hat_all)
#10.95
lm_acc <- (1/length(y_act_harvest[,1])) * sum((1-((y_act_harvest[,1] - y_hat_all)/y_act_harvest[,1])))
lm_bias <- sum(y_hat_all>y_act_harvest[,1])/length(y_act_harvest[,1])

rbind(harvest_summary,c('Linear Combination',lm_smape,lm_rmse,lm_bias,lm_acc))
write.csv(tail(lm_all$fitted.values,15),"harvest_pred.csv")
harvest_summary$colnames.y_agg_harvest.1.22.. <- as.character(harvest_summary$colnames.y_agg_harvest.1.22..)
harvest_summary[23,1]<-"Linear Model Blend"
harvest_summary[23,2:5]<-c(lm_smape,lm_rmse,lm_bias,lm_acc)
harvest_summary
write.csv(harvest_summary,"Model_Blend_Summary.csv",row.names = F)
#linear model all data, use fitted values
#RMSE: 307232.7
lm_all$coefficients
lm_all
saveRDS(lm_all,'LM_Blender.rds')

mean(lm_all$residuals)
#2.36362e-12
var(lm_all$residuals)
#3699131576
e1071::skewness(lm_all$residuals)
#0.3419385
e1071::kurtosis(lm_all$residuals)
#-0.4658733
median(lm_all$residuals)
#-12461.53
plot(density(lm_all$residuals))
lmtest::bptest(lm_all)
#p-value = 0.06462
car::ncvTest(lm_all)
# p = 0.23261
par(mfrow=c(2,2))
plot(lm_all)
par(mfrow=c(1,1))
write.csv(tail(y_agg_harvest['arima'],15),"arima.csv")

###step LM to find the important predictors that add information
lm_step <- step(lm_all)
summary(lm_step)
