library(forecast)
library(ggplot2)
library(ggfortify)
library(xlsx)
str(covid_19_india)
covid_19_india$ConfirmedIndianNational
plot(covid_19_india)
a<-covid_19_india$ConfirmedIndianNational
plot(a)
covid_19_india$State.UnionTerritory

Kerala=covid_19_india[covid_19_india$State.UnionTerritory=="Kerala",]
ts_Kerala=ts(Kerala$ConfirmedIndianNational)
autoplot(ts_Kerala,main="Kerala Outbreak",
     ylab="No. of confirmed indian nationals",xlab="No. of days since 30 Jan 2020",size=1.2,colour="blue")
#Maha=covid_19_india[covid_19_india$State.UnionTerritory=="Maharashtra",]
#ts_Maha=ts(Maha$ConfirmedIndianNational)
#str(ts_Maha)
#autoplot(ts_Maha,main="Kerala Outbreak",
         ylab="No. of confirmed indian nationals",xlab="No. of days since 30 Jan 2020",size=1.2,colour="red")
Kerala_predict <- ses(ts_Kerala,h=10,level=c(80,95),alpha = 0.999)
summary(Kerala_predict)
autoplot(Kerala_predict,size=1.2) + autolayer(fitted(Kerala_predict),series="Fitted")

Kerala_holt <- holt(ts_Kerala,seasonal="multiplicative",h=25)
Kerala_holt2 <- holt(ts_Kerala,damped=TRUE ,phi=0.9,h=25)
summary(Kerala_holt)
autoplot(Kerala_holt,size=1.2,colour="red") + autolayer(Kerala_holt2,size=1.2,colour="blue")
autoplot(Kerala_holt2,size=1.2,colour="blue")
