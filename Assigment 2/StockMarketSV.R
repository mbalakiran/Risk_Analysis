library(quantmod) # get stock prices; useful stock analysis functions
library(xts)
library(rvest)# web scraping
library(tidyverse) # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr) # working with strings
library(forcats)
library(lubridate) # working with dates 
library(plotly) # interactive plots
library(corrplot)
library(dplyr)
library(PerformanceAnalytics) # evaluating the performance and  risk  characteristics  of  financial  assets  or  funds

#Loads the company stock using ticker

getSymbols("MSFT",from="2009-10-01",to="2019-08-30") # Microsoft 
getSymbols("INTC",from="2009-10-01",to="2019-08-30") #Intel
getSymbols("T",from="2009-10-01",to="2019-08-30")   # AT&T
getSymbols("KO",from="2009-10-01",to="2019-08-30")  # Coca Cola
getSymbols("WMT",from="2009-10-01",to="2019-08-30") #Walmart

#Stock returns in log
MSFT_log_returns<-MSFT%>%Ad()%>%dailyReturn(type='log')
INTC_log_returns<-INTC%>%Ad()%>%dailyReturn(type='log')
T_log_returns<-T%>%Ad()%>%dailyReturn(type='log')
KO_log_returns<-KO%>%Ad()%>%dailyReturn(type='log')
WMT_log_returns<-WMT%>%Ad()%>%dailyReturn(type='log')
MSFT_log_returns
#Mean of log stock returns 

MSFT_mean_log<-mean(MSFT_log_returns)
INTC_mean_log<-mean(INTC_log_returns)
T_mean_log<-mean(T_log_returns)
KO_mean_log<-mean(KO_log_returns)
WMT_mean_log<-mean(WMT_log_returns)
MSFT_mean_log
#round it to 4 decimal places

mean_log<-c(INTC_mean_log,T_mean_log,KO_mean_log,MSFT_mean_log,WMT_mean_log)
mean_log<-round(mean_log,4)
mean_log
#standard deviation of log stock returns

MSFT_sd_log<-sd(MSFT_log_returns)
INTC_sd_log<-sd(INTC_log_returns)
T_sd_log<-sd(T_log_returns)
KO_sd_log<-sd(KO_log_returns)
WMT_sd_Log<-sd(WMT_log_returns)
MSFT_sd_log
#round it to 4 decimal places 

sd_log<-c(INTC_sd_log,T_sd_log,KO_sd_log,MSFT_sd_log,WMT_sd_Log)
sd_log<-round(sd_log,4)
sd_log
#create data frame

graphic1<-data.frame(rbind(c("INTC",INTC_mean_log,INTC_sd_log),c("T",T_mean_log,T_sd_log),c("KO",KO_mean_log,KO_sd_log),c("MSFT",MSFT_mean_log,MSFT_sd_log),c("WMT",WMT_mean_log,WMT_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("INTC","T","KO","MSFT","WMT")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")
View(graphic1)
#Data frame contains the 5 companies with each company's average log return and standard deviation.
#Use R to observe a stock's performance
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

INTC%>%Ad()%>%chartSeries()
INTC%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2019')

T%>%Ad()%>%chartSeries()
T%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2019')

KO%>%Ad()%>%chartSeries()
KO%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2019')

MSFT%>%Ad()%>%chartSeries()
MSFT%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2019')

WMT%>%Ad()%>%chartSeries()
WMT%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2019')
#Used plotly to create a visualization of each stock's risk v reward. 
#Risk: standard deviation of log returns
#Reward: mean of log returns

xlab<-list(title="Reward", titlefont=f)
ylab<-list(title="Risk", titlefont=f)

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)
#Checking the correlation of 5 stocks: Intel, Walmart, Microsoft, AT&T, coca cola

#check correlation of different companies
data<-cbind(diff(log(Cl(INTC))),diff(log(Cl(WMT))),diff(log(Cl(MSFT))),diff(log(Cl(T))),diff(log(Cl(KO))))
View(data)
chart.Correlation(data)

#random walk: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy

mu<-MSFT_mean_log
sig<-MSFT_sd_log
testsim<-rep(NA,1000)

#generate random daily exponent increase rate using MSFT's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days

price<-rep(NA,252*4)

#most recent price
price[1]<-as.numeric(MSFT$MSFT.Adjusted[length(MSFT$MSFT.Adjusted),])
price[1]
#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data
random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Microsoft Corporation (MSFT) price simulation for 4 years")+theme_bw()

#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables

N<-300
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(MSFT$MSFT.Adjusted[length(MSFT$MSFT.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(MSFT$MSFT.Adjusted[length(MSFT$MSFT.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,300))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 301

final_mat%>%gather("Simulation","Price",2:301)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="MICROSOFT CORPORATION Stock (MSFT): 300 Monte Carlo Simulations for 4 Years")+theme_bw()
################################
#Code Question 8 here

