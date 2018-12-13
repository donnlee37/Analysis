setwd("C:/Users/Ben-Eazy/Downloads")
library(foreign)
MMR<-read.dta("Data2.dta")
library(readxl)
EconFree<-read_excel("Economic Freedom Index.xls")
head(EconFree)
head(MMR)
Gender<-read_excel("ILO_Unemployment_rate.xls")
Employment<-read_excel("Employment.xls")

#Combined the MMR and Economic Freedom Index data 
Econ_LAC<-read_xlsx("Econ_Freedom_Index_LAC.xlsx")
mydata <- merge(MMR, Econ_LAC,
                by.x=c("country","year"),
                by.y=c("name","index year"), all = TRUE)
mydata2<-merge(mydata, Gender,
               by.x=c("country"),
               by.y=c("Country"), all = TRUE)
mydata3<-merge(mydata, Employment, 
               by.x=c("country"),
               by.y=c("Country"), all = TRUE)

#Note: Scatterplot on trade freedom and trade reform: both measure tariff barriers
#MMR and Economic Freedom data
plot(mydata$`trade freedom`, mydata$trade)
plot(mydata$trade, mydata$`trade freedom`,
     xlab = "Trade Reform",
     ylab = "Trade Freedom",
     las = 0.5)

#Note: Scatterplot on financial freedom: MMR and Economic Freedom data
plot(mydata$fin_reform, mydata$`financial freedom`,
     xlab = "Financial Reform",
     ylab = "Financial Freedom",
     las = 0.5)

#Add the data on gender inequality (female unemployment rate, general reform, labor freedom)
plot(mydata2$gen_reform, mydata2$`1990`,
     xlab = "General Reform",
     ylab = "Female Unemployment, 1990",
     las = 0.5)
plot(mydata2$gen_reform, mydata2$`1985`,
     xlab = "General Reform",
     ylab = "Female Unemployment, 1985",
     las = 0.5)
plot(mydata2$`labor freedom`, mydata2$`1985`,
     xlab = "Labor Freedom",
     ylab = "Female Unemployment, 1985",
     las = 0.5)

#Add data on female employment (Scatterplot on female employment, labor freedom, trade freedom, general reform)
plot(mydata3$gen_reform, mydata3$`1985`,
     xlab = "General Reform",
     ylab = "Female Employment, 1985",
     las = 0.5)
plot(mydata3$gen_reform, mydata3$`1990`,
     xlab = "General Reform",
     ylab = "Female Employment, 1990",
     las = 0.5)
plot(mydata3$`labor freedom`, mydata3$`1990`,
     xlab = "Labor Freedom",
     ylab = "Female Employment, 1990",
     las = 0.5)
plot(mydata3$`trade freedom`, mydata3$`1985`,
    xlab = "Trade Freedom",
    ylab = "Female Employment, 1985",
    las = 0.5)
plot(mydata3$trade, mydata3$`1990`,
     xlab = "Trade Reform",
     ylab = "Countries, Female Employment in 1990",
     las = 0.5)
plot(mydata3$priv, mydata3$`1995`,
     xlab = "Privatization",
     ylab = "Female Employment in 1995",
     las = 0.5)

#Regression model of trade reform and female employment
mod<-lm(mydata3$trade ~ mydata3$`1990`)
summary(mod)
abline(mod)

mod1<-lm(mydata3$priv ~ mydata3$`1995`)
summary(mod1)
abline(mod1)
