library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(grid)
library(olsrr)
library(caTools)
library(gridExtra)
library(grid)

cust1_50 <- read.csv("C:/Users/dylbe/Downloads/College/FALL WINTER 2022-23/STS 4980/Duke Energy/elon_project_data_1_50.csv")
cust51_100 <- read.csv("C:/Users/dylbe/Downloads/College/FALL WINTER 2022-23/STS 4980/Duke Energy/elon_project_data_51_100.csv")
cust101_150 <- read.csv("C:/Users/dylbe/Downloads/College/FALL WINTER 2022-23/STS 4980/Duke Energy/elon_project_data_101_150.csv")
cust151_200 <- read.csv("C:/Users/dylbe/Downloads/College/FALL WINTER 2022-23/STS 4980/Duke Energy/elon_project_data_151_200.csv")
cust_all <- bind_rows(cust1_50, cust51_100, cust101_150, cust151_200)

#demographics
demog <- read.csv("C:/Users/dylbe/Downloads/College/FALL WINTER 2022-23/STS 4980/Duke Energy/customer_demographics.csv")
cust_all_demog <- left_join(cust_all, demog, by = "customer_number")

cust_all$measurement_dttm_hb <- mdy_hm(cust_all$measurement_dttm_hb)
cust_all$date <- as.Date(cust_all$measurement_dttm_hb) 
cust_all$time <- hour(cust_all$measurement_dttm_hb) 

cust_all <- cust_all %>%  
  filter(!is.na(temperature), !is.na(kwh)) %>%  
  arrange(measurement_dttm_hb) %>%  
  mutate(date = date(measurement_dttm_hb),  
    heat = (ymd("2021-10-15")<= date & date <= ymd("2022-04-15")),  
    dayofweek = wday(date, label=TRUE),  
    weekend = (dayofweek %in% c("Sat", "Sun")),  
    hour = hour(measurement_dttm_hb),  
    working = (8 <= hour & hour<=17 & weekend==FALSE),  
    lkwh = log(kwh), 
    night = (hour > 22 | hour < 6), 
    lag1 = lag(temperature, 1)) 

cust_all$season <- ((yday(cust_all$date) >= 355 | yday(cust_all$date) <= 78) * 1) + 
  ((yday(cust_all$date) >= 79 & yday(cust_all$date) <= 170) * 2) + 
  ((yday(cust_all$date) >= 171 & yday(cust_all$date) <= 264) * 3) + 
  ((yday(cust_all$date) >= 265 & yday(cust_all$date) <= 354) *4) 
cust_all_ws <- filter(cust_all, season == 1 | season == 3)
# Specifies the season of each date as a numerical value 
# 1 = winter, 2 = spring, 3 = summer, 4 = fall

cust_all$logkwh <- log(cust_all$kwh)
cust_all$temp2 <- (cust_all$temperature)^2
cust_all$cubrtkwh <- (cust_all$kwh)^(1/3)
cust_all <- filter(cust_all, logkwh > -Inf)
cust_all <- filter(cust_all, temp2 > -Inf)



#Rsquared and MSE
allrsquares <- NULL 
allMSE <- NULL
for(i in c(1:66,68:147,149:200)){ 
  cust <- cust_all %>% 
    filter(customer_number == i)%>%
    filter(season %in% c(1,3))
  model = lm(formula = kwh ~ temperature + factor(season) +
               temperature*factor(season), 
             data = cust) #Swap Model
  allrsquares[i] = summary(model)$adj.r.squared 
  cust$yhat = predict(model, cust)
  MSEp = sum((cust$kwh - (cust$yhat))**2)
  MSEp = MSEp / nrow(cust)
  allMSE[i] = MSEp
} 
allrsquares <- allrsquares[!is.na(allrsquares)]
mean(allrsquares)
allMSE <- allMSE[!is.na(allMSE)]
mean(allMSE)

#Testing model strength
allMSE4 = NULL 
set.seed(3049)
for(i in c(1:66, 68:147, 149:200)){ 
  currHouse = filter(cust_all, customer_number== i) 
  currHouse = filter(currHouse, season %in% c(1,3)) 
  sample = sample.split(currHouse$kwh, SplitRatio = .8) 
  train = subset(currHouse, sample == TRUE) 
  test = subset(currHouse, sample == FALSE) 
  model = lm(formula = cubrtkwh ~ temperature + 
               factor(night) + factor(weekend), 
             data = train) 
  test$yhat = predict(model, test) 
  MSEp = sum((test$kwh - (test$yhat)**3)**2) 
  MSEp = MSEp / nrow(test) 
  allMSE4[i] = MSEp 
} 
allMSE4 <- allMSE4[!is.na(allMSE4)]
mean(allMSE4)

allMSE3 = NULL 
set.seed(3049)
for(i in c(1:66, 68:147, 149:200)){ 
  currHouse = filter(cust_all, customer_number== i) 
  currHouse = filter(currHouse, season %in% c(1,3)) 
  sample = sample.split(currHouse$kwh, SplitRatio = .8) 
  train = subset(currHouse, sample == TRUE) 
  test = subset(currHouse, sample == FALSE) 
  model = lm(formula = kwh ~ temperature + factor(season) +
               temperature*factor(season), 
             data = train) 
  test$yhat = predict(model, test) 
  MSEp = sum((test$kwh - test$yhat)**2) 
  MSEp = MSEp / nrow(test) 
  allMSE3[i] = MSEp 
} 
allMSE3 <- allMSE3[!is.na(allMSE3)]
mean(allMSE3)

allMSE2 = NULL 
set.seed(3049)
for(i in c(1:66, 68:147, 149:200)){ 
  currHouse = filter(cust_all, customer_number== i) 
  currHouse = filter(currHouse, season %in% c(1,3)) 
  sample = sample.split(currHouse$kwh, SplitRatio = .8) 
  train = subset(currHouse, sample == TRUE) 
  test = subset(currHouse, sample == FALSE) 
  model = lm(formula = logkwh ~ 
               temperature + factor(season) + factor(weekend), 
             data = train) 
  test$yhat = predict(model, test) 
  MSEp = sum(((test$kwh) - exp(test$yhat))**2) 
  MSEp = MSEp / nrow(test) 
  allMSE2[i] = MSEp 
} 
allMSE2 <- allMSE2[!is.na(allMSE2)]
mean(allMSE2)

#Customer graphs scatter
custfilt <- filter(cust_all_ws,customer_number%in%c(1,140,185))
custtitles <- c(
  '1'="Customer #1",
  '140'="Customer #140",
  '185'="Customer #185"
)
ggplot(data=custfilt,mapping=aes(x=temperature,y=kwh,color=factor(season))) + 
  geom_point(alpha=.3) +
  facet_wrap(~customer_number,labeller=as_labeller(custtitles)) +
  labs(title="kWh vs Outdoor Temperature",
       x="Outdoor Temperature",y="kWh Usage",color="Season") +
  scale_color_manual(labels = c("Winter","Summer"),values=c("cyan3","coral"))

#ggplot visuals model strength
MLR_Rsq <-ggplot(mapping=aes(allrsquares)) +
  geom_histogram(bins=20,color="white",
                 fill="darkseagreen") +
  labs(title=expression(Distribution~of~Adj~R^2~Across~All~Customers),
       x=expression(R^2),
       y="Customer Frequency") +
  geom_vline(aes(xintercept=mean(allrsquares)),color="darkseagreen4",
             size=1.5,lty=1) +
  annotate("text",x = mean(allrsquares) * 1.69,
       y = mean(allrsquares) * 50,
       label = paste("Mean Adj.R-Squared",round(mean(allrsquares),4), nsmall=""),
       col = "darkseagreen4",
       size=5)
MLR_MSE <- ggplot(mapping=aes(allMSE3)) +
  geom_histogram(bins=20,color="white",fill="cornsilk3") +
  labs(title="Distribution of MSE Across All Customers", 
       subtitle="80% Training 20% Testing",
       x="MSE",y="Customer Frequency") +
  geom_vline(aes(xintercept=mean(allMSE3)),color="cornsilk4",size=1.5) +
  annotate("text",x = mean(allMSE3) * 2.7,
           y = mean(allMSE3) * 50,
           label = paste("Mean MSE:", round(mean(allMSE3),4), nsmall=""),
           col = "cornsilk4",
           size=5)
  log_MSE <- ggplot(mapping=aes(allMSE2)) +
  geom_histogram(bins=20,color="white",fill="cornsilk3") +
  labs(title="Distribution of MSE Across All Customers", 
       subtitle="80% Training 20% Testing; logkwh ~ temperature + 
       factor(season) + temperature*factor(season)",
       x="MSE",y="Customer Frequency") +
  geom_vline(aes(xintercept=mean(allMSE2)),color="cornsilk4",size=1.5) +
  annotate("text",x = mean(allMSE2) * 2.7,
           y = mean(allMSE2) * 50,
           label = paste("Mean MSE:", round(mean(allMSE2),4), nsmall=""),
           col = "cornsilk4",
           size=5)
  cubrt_MSE <- ggplot(mapping=aes(allMSE4)) +
    geom_histogram(bins=20,color="white",fill="cornsilk3") +
    labs(title="Distribution of MSE Across All Customers", 
         subtitle="80% Training 20% Testing; cubkwh ~ temperature + 
         factor(weekend) + 
         factor(night)",
         x="MSE",y="Customer Frequency") +
    geom_vline(aes(xintercept=mean(allMSE4)),color="cornsilk4",size=1.5) +
    annotate("text",x = mean(allMSE4) * 3.9,
             y = mean(allMSE4) * 50,
             label = paste("Mean MSE:", round(mean(allMSE4),4), nsmall=""),
             col = "cornsilk4",
             size=5)
grid.arrange(MLR_Rsq,MLR_MSE,nrow=1)

#residual plots for models
cust_x <- filter(cust_all,customer_number%in%c(1:40),season%in%c(1,3))
m_log <- lm(formula = logkwh ~ temperature + factor(season) + 
          temperature*factor(season), 
          data = cust_x)
Resid_log <- ggplot(m_log,aes(x=temperature,y=cust_x$kwh-exp(predict(m_log)))) + 
  geom_point(alpha=.04,col="blue") +
  geom_hline(yintercept=0, col="red") + 
  labs(title="Residuals For Customers 1 - 40",
       subtitle="logkwh ~ temperature + factor(season) + 
          temperature*factor(season)",
       x="Outdoor Temperature (F)",
       y="Residual") 
m_linear <- lm(formula = kwh ~ temperature + factor(season) + 
          temperature*factor(season), 
        data = cust_x)
Resid_linear <- ggplot(m_linear,aes(x=temperature,y=.resid)) + 
  geom_point(alpha=.04,col="blue") +
  geom_hline(yintercept=0, col="red") + 
  labs(title="Residuals For Customers 1 - 40",
       subtitle="kwh ~ temperature + factor(season) + 
          temperature*factor(season)",
       x="Outdoor Temperature (F)",
       y="Residual") 
m_cubrt <- lm(formula = cubrtkwh ~ temperature + weekend + 
                         night, 
                       data = cust_x)
Resid_cubrt <- ggplot(m_cubrt,aes(x=temperature,
                                y=cust_x$kwh-(predict(m_cubrt))**3)) + 
  geom_point(alpha=.04,col="blue") +
  geom_hline(yintercept=0, col="red") + 
  labs(title="Residuals For Customers 1 - 40",
       subtitle="cubkwh ~ temperature + 
       factor(weekend) + factor(night)",
       x="Outdoor Temperature (F)",
       y="Residual") 
grid.arrange(Resid_cubrt,cubrt_MSE,nrow=1)


  

