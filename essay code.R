install.packages("ZOO")
library(zoo)
# VAR
install.packages("VARselect")
library(VARselect)

install.packages("vars")
library(vars)


############ oil price plot (dollars per barrel)(not seasonally adjusted)
auto <- read.csv("C:/Users/eric/Desktop/my essay/data/CrudeOilPrices.csv", header=T)
oilprice<-ts(auto$DCOILWTICO,frequency=4,start=1947)
plot(oilprice, type="l", main="Crude Oil Price", lwd=2.0, ylab="")
#plot(log(oilprice), type="l", main="Log Crude Oil Price", lwd=2.0, ylab="")
plot1
plot4

############ Consumer Price Index for All Urban Consumers Fuel oil and other fuels (dollars per barrel)(not seasonally adjusted)
auto8 <- read.csv("C:/Users/eric/Desktop/my essay/data/Consumer Price Index for All Urban Consumers Fuel oil and other fuels.csv", header=T)
CPIoilprice<-ts(auto8$VALUE,frequency=4,start=1947)
plot(CPIoilprice, type="l", main="CPI oil price", lwd=2.0, ylab="")



##Consumer Price Index for All Urban Consumers Gasoline

###############consumption plot (billions of dollars)(seasonally adjusted)
auto1<- read.csv("C:/Users/eric/Desktop/my essay/data/Personal Consumption Expenditures.csv")
consumption<-ts(auto1$PCEC,frequency=4,start=1947)
plot2<-plot(consumption,type="l",lwd=2.0, main="Personal Consumption Expenditures", ylab="")
plot5<-plot(log(consumption),type="l",lwd=2.0, main="Log Personal Consumption Expenditures", ylab="")

############ Disposable Personal Income (DPI) (Billions of Dollars,Seasonally Adjusted Annual Rate)
auto7 <- read.csv("C:/Users/eric/Desktop/my essay/data/DPI.csv", header=T)
income<-ts(auto7$DPI,frequency=4,start=1947)
plot(income, type="l", main="Disposable Personal Income ", lwd=2.0, ylab="")
plot(log(income), type="l", main="Disposable Personal Income ", lwd=2.0, ylab="")




################# consumption and oilprice change graph
oilpricechange <- matrix()
t1<- matrix()
l=length(oilprice)-4      ##annual changes
for(i in 1:l)
  oilpricechange[i]=(oilprice[i+4]-oilprice[i])/oilprice[i]*100

oilpricechange<-ts(oilpricechange,frequency=4,start=1948)

consumptionchange<- matrix()
l=length(consumption)-4
for(i in 1:l)
  consumptionchange[i]=(consumption[i+4]-consumption[i])/consumption[i]*100

consumptionchange<-ts(consumptionchange, frequency=4,start=1948)

par(pch=50, col="black") # plotting symbol and color 
par(mfrow=c(2,1)) # all plots on one page 
opts = c(plot(oilpricechange, type="l", ylab="", lwd=2,main="Oil Price Annual Changes "),abline(h=0),plot(consumptionchange,type="l", lwd=2,main="Consumption Annual Changes ",ylab=""),abline(h=0))



####################consumption included durable goods plot(billions of dollars)(seasonally adjusted)
auto2<- read.csv("C:/Users/eric/Desktop/my essay/data/PersonalConsumptionDurable.csv")
consumptiondurable<-ts(auto2$PCDG,frequency=4,start=1947)
plot3<-plot(consumptiondurable,type="l",lty=1,lwd=2, main="Consumption Expenditures (Durable Goods)", ylab="")
plot6<-plot(log(consumptiondurable),type="l",lty=1,lwd=2, main="Log Consumption Expenditures (Durable Goods)", ylab="")


#####################Shares of gross domestic product Personal consumption expenditures(%)(not seasonally adjusted)
auto3<- read.csv("C:/Users/eric/Desktop/my essay/data/Shares of gross domestic product Personal consumption expenditures.csv")
consumption_gdp<-ts(auto3$DPCERE,frequency=4,start=1947)
plot7<-plot(consumption_gdp,type="l",lty=1,lwd=2, main="Personal Consumption in GDP (%)", ylab="")



plot4<-plot(log(oilprice), type="l", main="Log Crude Oil Price", lwd=2.0, ylab="")
plot5<-plot(log(consumption),type="l",lwd=2.0, main="Log Personal Consumption Expenditures", ylab="")
plot6<-plot(log(consumptiondurable),type="l",lty=1,lwd=2, main="Log Consumption Expenditures (Durable Goods)", ylab="")

par(pch=22, col="blue") # plotting symbol and color 
par(mfrow=c(3,1)) # all plots on one page 
opts = c(plot4,plot5,plot6)

###############Russell 3000 index plot (index)(not seasonally adjusted)
auto4<- read.csv("C:/Users/eric/Desktop/my essay/data/Russell 3000 Total Market Index.csv")
RU3000<-ts(auto4$RU3000TR,frequency=4,start=1979)

###stock market retrun

l=length(RU3000)-1
stockrt<-array()
for (i in 1:l)
{
  stockrt[i]=(RU3000[i+1]-RU3000[i])/RU3000[i]*100
}
stockrt<-ts(stockrt,frequency=4, start=c(1979,2))
par(mfrow=c(1,1))
plot7<-plot(stockrt,type="l",lwd=2.0, main="Russell 3000 index return ", ylab="",col="black")
plot(abline(h=0),col="black")

################All-Transactions House Price Index for the United States 2016:Q1: 369.67 Index 1980:Q1=100 (index)(not seasonally adjusted)
auto5<- read.csv("C:/Users/eric/Desktop/my essay/data/All-Transactions House Price Index for the United States.csv")
HPI<-ts(auto5$USSTHPI,frequency=4,start=1975)
plot9<-plot(HPI,type="l",lwd=2.0, main="House Price Index", ylab="")
plot10<-plot(log(HPI),type="l",lwd=2.0, main="House Price Index", ylab="")

##################Effective Federal Funds Rate(percent)(not seasonally adjusted)
auto6<- read.csv("C:/Users/eric/Desktop/my essay/data/Effective Federal Funds Rate.csv")
FFR<-ts(auto6$DFF,frequency=4,start=c(1954,3))
par(mfrow=c(1,1))
plot11<-plot(FFR,type="l",lwd=2.0, main="Federal Funds Rate", ylab="")


install.packages("CADFtest")
library(CADFtest)

##########################################################################
###################### Augmented Dickey Fuller test
"adf"<- 
  function(x, L = 2, int = T, trend = T) 
  { 
    #Construct Data for Augmented Dickey Fuller Model with L lags. 
    #This is a modified version for R, in which the command rts was substituted by ts. 
    x <- ts(x) 
    D <- diff(x) 
    if(L > 0) { 
      for(i in 1:L) 
        D <- ts.intersect(D, lag(diff(x),  - i)) 
    } 
    D <- ts.intersect(lag(x, -1), D) 
    if(trend == T) 
      D <- ts.intersect(D, time(x)) 
    y <- D[, 2] 
    x <- D[, -2] 
    if(int == T) 
      (lm(y ~ x)) 
    else (lm(y ~ x - 1)) 
  } 

aic<-function(model)
{
  n<-model$df + length(model$coeff)
  log(sum(resid(model)^2)/n)+(length(model$coeff)/n)*2
}

bic<-function(model)
{
  n<-model$df + length(model$coeff)
  log(sum(resid(model)^2)/n)+(length(model$coeff)/n)*log(n)
}
install.packages("ZOO")
library(zoo)

#########################################################################adf tests
#######################adf for consumption
adf.test(log(consumption), k=2)

dconsumption<- matrix()
l=length(consumption)-1

for(i in 1:l)
  dconsumption[i]=(log(consumption)[i+1]-log(consumption)[i])

dconsumption<-ts(dconsumption, frequency=4,start=c(1947,2))

mi<-adf(dconsumption,L=2, int=T, trend=TRUE )
summary(mi)

adf.test(dconsumption, k=2)


############################housing adf


HPrt<- matrix()
l=length(HPI)-1

for(i in 1:l)
{
  HPrt[i]=((log(HPI)[i+1]-log(HPI)[i]))/log(HPI)[i]*100
}
HPrt<-ts(HPrt, frequency=4,start=c(1975,2))

adf.test(HPrt, k=2)
mi<-adf(HPrt,L=2, int=T, trend=TRUE )
summary(mi)


dHPrt<- matrix()
l=length(HPrt)-1
for(i in 1:l)
  dHPrt[i]=(HPrt[i+1]-HPrt[i])

dHPrt<-ts(dHPrt, frequency=4,start=c(1975,3))

mi<-adf(dHPrt,L=2, int=T, trend=TRUE )
summary(mi)

adf.test(dHPrt, k=2)
#################################income adf
VARselect(income, lag.max = 10, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)

adf.test(income, k=4 )

dincome<- matrix()
l=length(income)-1
for(i in 1:l)
  dincome[i]=(income[i+1]-income[i])

dincome<-ts(dincome, frequency=4,start=c(1947,2))

VARselect(dincome, lag.max = 10, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)
mi<-adf(dincome,L=8, int=T, trend=TRUE )
summary(mi)

adf.test(dincome, k=8)

##################################stock market return adf
adf.test(stockrt, k=2 )
summary(adf(stockrt, L=2, int=T, trend=FALSE))
summary(adf(stockrt, L=2, int=T, trend=TRUE))

dstockrt<- matrix()
l=length(stockrt)-1

for(i in 1:l)
  dstockrt[i]=((stockrt)[i+1]-(stockrt)[i])

dstockrt<-ts(dstockrt, frequency=4,start=c(1979,2))

mi<-adf(dstockrt,L=2, int=T, trend=TRUE )
summary(mi)

adf.test(dstockrt, k=2)

##################################FRR adf
adf.test(FFR, k=2)


dFFR<- matrix()
l=length(FFR)-1

for(i in 1:l)
  dFFR[i]=(FFR[i+1]-FFR[i])

dFFR<-ts(dFFR, frequency=4,start=c(1954,4))

mi<-adf(dFFR,L=6, int=T, trend=TRUE )
aic(mi)
summary(mi)

adf.test(dFFR, k=2)

VARselect(dFFR, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)

install.packages("dyn")
library(dyn)

m1<-adf(log(oilprice), L=1, int=T, trend=FALSE)
m2<-adf(log(oilprice), L=1, int=T, trend=TRUE)
m3<-adf(log(oilprice), L=2, int=T, trend=TRUE)
m4<-adf(log(oilprice), L=3, int=T, trend=TRUE)
m5<-adf(log(oilprice), L=4, int=T, trend=TRUE)
m6<-adf(log(oilprice), L=5, int=T, trend=TRUE)
m7<-adf(log(oilprice), L=6, int=T, trend=TRUE)
m13<-adf(log(oilprice), L=13, int=T, trend=TRUE)
m19<-adf(log(oilprice), L=19, int=T, trend=TRUE)
m21<-adf(log(oilprice), L=21, int=T, trend=TRUE)
m50<-adf(log(oilprice), L=50, int=T, trend=TRUE)
m100<-adf(log(oilprice), L=100, int=T, trend=TRUE)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m19)

bic(m1)
bic(m2)
bic(m3)
bic(m4)
bic(m5)
bic(m6)
bic(m7)
bic(m13)
bic(m19)
bic(m21)
bic(m50)
bic(m100)





M <- c(m1,m2,m3,m4,m5)
A <- array(0,c(5,2))

for(i in 1:5){
  g <- dyn$lm(M[[i]])
  A[i,1] <- aic(g)
  A[i,2] <- bic(g)
}

A
aic_min<-which.min(A[,1])
bic_min<-which.min(A[,2])
aic_min
bic_min

#model_aic<-dyn$lm(eval(parse(text=paste("m",aic_min,sep=""))))
#summary(model_aic)

#model_bic<-dyn$lm(eval(parse(text=paste("m",bic_min,sep=""))))    
#summary(model_bic)

install.packages("forecast")
library(forecast)

urca(oilprice)


lm1<-adf(consumption_gdp, L=1, int=T, trend=FALSE)
lm2<-adf(consumption_gdp, L=1, int=T, trend=TRUE)
lm3<-adf(consumption_gdp, L=2, int=T, trend=TRUE)
lm4<-adf(consumption_gdp, L=3, int=T, trend=TRUE)
lm5<-adf(consumption_gdp, L=4, int=T, trend=TRUE)

AIC(lm1, lm2,lm3)
BIC(lm1, lm2)
aic(lm1)
aic(lm2)
aic(lm3)
bic(lm1)
bic(lm2)

m<-adf.test(oilprice, k=1)
adf.test(consumption, k=5)
aic(lm1)
AIC(lm1)
aic(lm2)
AIC(lm2)


##################################################################
###################################  quarterly oil price change
amount1<-matrix()
l=length(oilprice)
for (i in 2:l)
{
  amount1[i-1]=(oilprice)[i]-(oilprice)[i-1]
}

amount1<-ts(amount1, frequency=4, start=c(1947,2))
par(pch=50, col="black") # plotting symbol and color 
par(mfrow=c(1,1))
plot.ts(amount1, main="Quarterly oil price change", ylab="",lwd=2)
abline(h=0)


#####################################  negative decrease in oil price (one quater)
l=length(oilprice)
amount2<-array(0,l)
for (i in 2:l)
{
  t=((oilprice))[i]-((oilprice))[i-1]
  if (t<0){ 
    amount2[i-1]=t 
  }else{
    amount2[i-1]=0}
}
amount2<-ts(amount2, frequency=4, start=c(1947,2))
plot.ts(amount2, ylab="", main="Negative decrease in oil price",lwd=2)



#####################################  net oil price decrease

#1-year, 4 quaters
l=length(oilprice)
amount3<-array(0,l)
o<-array(0,dim=c(0,l))
for (i in 5:l)
{
  o[i-1]=(oilprice)[i-1]
  o[i-2]=(oilprice)[i-2]
  o[i-3]=(oilprice)[i-3]
  o[i-4]=(oilprice)[i-4]
  k=which.min(o)
  t=((oilprice))[i]-o[k]
  if (t<0){ 
    amount3[i-4]=t 
  }else{
    amount3[i-4]=0}
  o<-array(0,dim=c(0,l))
}
amount3<-ts(amount3, frequency=4, start=c(1948,1))
plot.ts(amount3,lwd=2, ylab="", main="1 year net oil price decrease")

#2-year, 8 quaters

l=length(oilprice)
amount4<-array(0,l)
o<-array(0,dim=c(0,l))
for (i in 9:l)
{
  o[i-1]=(oilprice)[i-1]
  o[i-2]=(oilprice)[i-2]
  o[i-3]=(oilprice)[i-3]
  o[i-4]=(oilprice)[i-4]
  o[i-5]=(oilprice)[i-5]
  o[i-6]=(oilprice)[i-6]
  o[i-7]=(oilprice)[i-7]
  o[i-8]=(oilprice)[i-8]
  
  k=which.min(o)
  t=((oilprice))[i]-o[k]
  if (t<0){ 
    amount4[i-8]=t 
  }else{
    amount4[i-8]=0}
  o<-array(0,dim=c(0,l))
}
amount4<-ts(amount4, frequency=4, start=c(1949,1))
plot.ts(amount4, ylab="", main="2 year net oil price decrease",lwd=2)


#3-year, 12 quaters

l=length(oilprice)
amount5<-array(0,l)
o<-array(0,dim=c(0,l))
for (i in 13:l)
{
  o[i-1]=(oilprice)[i-1]
  o[i-2]=(oilprice)[i-2]
  o[i-3]=(oilprice)[i-3]
  o[i-4]=(oilprice)[i-4]
  o[i-5]=(oilprice)[i-5]
  o[i-6]=(oilprice)[i-6]
  o[i-7]=(oilprice)[i-7]
  o[i-8]=(oilprice)[i-8]
  o[i-9]=(oilprice)[i-9]
  o[i-10]=(oilprice)[i-10]
  o[i-11]=(oilprice)[i-11]
  o[i-12]=(oilprice)[i-12]
  
  k=which.min(o)
  t=((oilprice))[i]-o[k]
  if (t<0){ 
    amount5[i-12]=t 
  }else{
    amount5[i-12]=0}
  o<-array(0,dim=c(0,l))
}
amount5<-ts(amount5, frequency=4, start=c(1950,1))
plot.ts(amount5, ylab="", main="3 year net oil price decrease",lwd=2)


par(pch=30, col="black") # plotting symbol and color 
par(mfrow=c(2,1)) # all plots on one page 
opts = c(plot.ts(amount1, main="Quarterly oil price change", ylab="",lwd=2),abline(h=0),plot.ts(amount2, ylab="", main="Negative decrease in oil price",lwd=2),abline(h=0))


par(pch=30, col="black") # plotting symbol and color 
par(mfrow=c(3,1)) # all plots on one page 
opts = c(plot.ts(amount3,lwd=2, ylab="", main="1 year net oil price decrease"),plot.ts(amount4, ylab="", main="2 year net oil price decrease",lwd=2),plot.ts(amount5, ylab="", main="3 year net oil price decrease",lwd=2))

par(pch=30, col="black") # plotting symbol and color 
par(mfrow=c(3,1))
opts = c(plot.ts(amount2, ylab="", main="Negative decrease in oil price",lwd=2),abline(h=0),plot.ts(amount3,lwd=2, ylab="", main="1 year net oil price decrease"),plot.ts(amount4, ylab="", main="2 year net oil price decrease",lwd=2))



###############################################################################         
########################################################regression model

###quarterly oil price change

install.packages("dyn")
library(dyn)

consumption = window(consumption, start=c(1980, 1), end=c(2016,1))
income=window(income, start=c(1980, 1), end=c(2016,1))
stockrt=window(stockrt, start=c(1980, 1), end=c(2016,1))
FFR=window(FFR, start=c(1980, 1), end=c(2016,1))
amount1=window(amount1, start=c(1980, 1), end=c(2016,1))
HPrt=window(HPrt, start=c(1980, 1), end=c(2016,1))
amount2=window(amount2, start=c(1980, 1), end=c(2016,1))
amount3=window(amount3, start=c(1980, 1), end=c(2016,1))
amount4=window(amount4, start=c(1980, 1), end=c(2016,1))
amount5=window(amount5, start=c(1980, 1), end=c(2016,1))


plot.ts(amount1, ylab="", main="Quarterly oil price change",lwd=2)
abline(h=0,lwd=2)

plot.ts(amount2, ylab="", main="Negative decrease in oil price",lwd=2)
plot.ts(amount3, ylab="", main="1 year oil price decrease",lwd=2)
plot.ts(amount4, ylab="", main="2 year oil price decrease",lwd=2)
plot.ts(amount5, ylab="", main="3 year oil price decrease",lwd=2)

par(pch=30, col="black") # plotting symbol and color 
par(mfrow=c(2,1)) # all plots on one page 
opts = c(plot.ts(amount1, main="Quarterly Oil Price Change", ylab="",lwd=2),abline(h=0,lwd=2),plot.ts(amount2, ylab="", main="Negative Decrease in Oil Price",lwd=2))



par(pch=10, col="black") # plotting symbol and color 
par(mfrow=c(3,1))
opts = c(plot.ts(amount3,lwd=2, ylab="", main="1 Year Net Oil Price Decrease"),plot.ts(amount4, ylab="", main="2 Year Net Oil Price Decrease",lwd=2),plot.ts(amount5, ylab="", main="3 Year Net Oil Price Decrease",lwd=2))


########### year2008
year2008<-array(0,c(1,145))
for (i in 1:145)
{
  if (i>=115)
  {
    year2008[i]=1
  
  }else{
    year2008[i]=0}
}
year2008<-ts(year2008,frequency=4, start=c(1980,1),end=c(2016,1))


##amount1
f1<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount1),-1)+lag(diff(amount1),-2)+lag(diff(amount1),-3)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)

f2<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(amount1),-1)+lag(diff(amount1),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)
f6<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
                    +lag(diff(amount1),-1)+lag(diff(amount1),-2)+lag(diff(amount1),-3)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)
f3<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(income),-2)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)+
             +lag(diff(amount1),-1)+lag(diff(amount1),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)
f7<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount1),-1)+lag(diff(amount1),-2)+lag(diff(amount1),-3)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)
##amount2
f3<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount2),-1)+lag(diff(amount2),-2)+lag(diff(amount2),-3)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)

f4<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)+
             +lag(diff(amount2),-1)+lag(diff(amount2),-2)+lag(diff(amount2),-3)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)

f5<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount2),-1)+lag(diff(amount2),-2)+lag(diff(amount2),-3)+lag(diff(amount2),-4)+lag(diff(amount2),-5)+lag(diff(amount2),-6)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)

#### f6 bic minimun
f6<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount2),-1)+lag(diff(amount2),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)
f7<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)+
             +lag(diff(amount2),-1)+lag(diff(amount2),-2)+lag(diff(amount2),-3)+lag(diff(amount2),-4)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)


##amount3

f6<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount3),-1)+lag(diff(amount3),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)


f7<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)+
             +lag(diff(amount3),-1)+lag(diff(amount3),-2)+lag(diff(amount3),-3)+lag(diff(amount3),-4)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)

f5<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount3),-1)+lag(diff(amount3),-2)+lag(diff(amount2),-5)+lag(diff(amount2),-6)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)


##amount4
f6<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount4),-1)+lag(diff(amount4),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)


##amount5
f6<-dyn$lm(diff(consumption)~(lag(consumption,-1)+lag(income,-1)+lag(HPrt,-1)+lag(stockrt,-1)) +lag(diff(income),-1)+lag(diff(HPrt),-1)+lag(diff(HPrt),-2)+lag(diff(HPrt),-3)+lag(diff(HPrt),-4)+lag(diff(stockrt),-1)+lag(diff(stockrt),-2)+lag(diff(consumption),-1)+lag(diff(consumption),-2)+lag(diff(consumption),-3)++lag(diff(consumption),-4)++lag(diff(consumption),-5)++lag(diff(consumption),-6)+
             +lag(diff(amount5),-1)+lag(diff(amount5),-2)+lag(diff(FFR),-1)+lag(diff(FFR),-2)+lag(diff(FFR),-3)+year2008)




VARselect(stockrt, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)
VARselect(HPrt, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)
VARselect(consumption, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)
VARselect(oilprice, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)
VARselect(FFR, lag.max = 20, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL)



m1<- lm(dconsumption~dincome+dstockrt+dHPrt+dFFR+amount1+lag((dconsumption),-1)+lag((income),-1)+lag((HPrt),-1)+lag((stockrt),-1))
m2<-lm(dconsumption~dincome+dstockrt+dHPrt+dFFR+amount1+lag((dconsumption),-1)+lag((income),-1)+lag((HPrt),-1)+lag((stockrt),-1)+lag((dconsumption),-2)+lag(dconsumption,-3))

m2 <- log(consumption) ~ (lag(log(consumption),-1) + log(oilprice))
m3 <- log(consumption) ~ (lag(log(consumption),-1) + log(oilprice) + diff(log(oilprice)))
m4 <- log(consumption) ~ (lag(log(consumption),-1) + log(oilprice) + diff(log(oilprice)) + lag(diff(log(oilprice)),-1))


M <- (c(f1,f2))

A <- array(0,c(2,2))

AIC(f1)
AIC(f2)
BIC(f1)
BIC(f2)
for(i in 1:4){
  g <- dyn$lm(M[[i]])
  A[i,1] <- AIC(g)
  A[i,2] <- BIC(g)
}


aic_min<-which.min(A[,1])
bic_min<-which.min(A[,2])


model_aic<-dyn$lm(eval(parse(text=paste("m",aic_min,sep=""))))
summary(model_aic)


model_bic<-dyn$lm(eval(parse(text=paste("m",bic_min,sep=""))))    
summary(model_bic)








########################### example
f<-dyn$lm(gas~lag(gas,-1)+lag(diff(gas),-1)+price+diff(price)+lag(diff(price),-1)+income+diff(income)+lag(diff(income),-1))




########################################################
###########test for structural change
install.packages("strucchange")
library("strucchange")


bp.amount1 <- breakpoints(amount1 ~ 1)
summary(bp.amount1)
plot(bp.amount1)

bp.amount2 <- breakpoints(amount2 ~ 1)
summary(bp.amount2)
plot(bp.amount2)

bp.amount3 <- breakpoints(amount3 ~ 1)
summary(bp.amount3)
plot(bp.amount3)
## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(bp.amount3)
## confidence intervals
ci.amount3 <- confint(bp.amount3)
breakdates(ci.amount3)
ci.amount3
#plot(amount3)
lines(ci.amount3)



bp.amount4 <- breakpoints(amount4 ~ 1)
summary(bp.amount4)
plot(bp.amount4)
## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(bp.amount4)
## confidence intervals
ci.amount4 <- confint(bp.amount4)
breakdates(ci.amount4)
ci.amount4
#plot(amount3)
lines(ci.amount4)


bp.amount5 <- breakpoints(amount5 ~ 1)
summary(bp.amount5)
plot(bp.amount5)


par(pch=15, col="black") # plotting symbol and color 
par(mfrow=c(2,1))
opts = c(plot(bp.amount1),plot(bp.amount2))

par(pch=15, col="black") # plotting symbol and color 
par(mfrow=c(3,1))
opts = c(plot(bp.amount3),plot(bp.amount4),plot(bp.amount5))
