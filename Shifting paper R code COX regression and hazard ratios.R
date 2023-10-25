#install.packages("ggfortify")
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library(tab)

library(readxl)
mydata<-read_excel(file.choose())
#mydata<-read.csv(file.choose(), header=TRUE)
mydata
mydata_df<-as.data.frame(mydata) #x being the tibble to be converted
colnames(mydata_df)[4]<-"flyname"
head(mydata_df)

mydata_df$flyname <- mydata_df$flyname %>% as.factor() %>% forcats::fct_rev()

head(mydata_df)

mySurv<-Surv(time=mydata_df$`age in days`, event = mydata_df$`total Males`==1)
class(mySurv)
head(mySurv)
summary(mySurv)


## single survival curve: no comparisons
myfit<-survfit(mySurv~1) ## single curve for all 
myfit
median(mydata_df$`age in days`)


### Median survival is the time at which the survivorship 
### function equals 0.5.
plot(myfit)
plot(myfit, conf.int = "none")
abline(h=0.5)
abline(v=49)


## specify predictor variable in the formula
myfit<-survfit(mySurv~mydata_df$`flyname`)
myfit
#autoplot(myfit)
plot(myfit)
table(mydata_df$`flyname`)
summary(myfit, times= c(seq(11,115, by =1)))
plot(myfit, col=c("red","blue")) 
plot(myfit, conf.int = TRUE, col=c("red","blue"))
plot(myfit, col=c("red","blue"))
plot(myfit, col=c("red","blue"), mark=3)  ## mark.time=T marked at 
## each censoring time
plot(myfit, col = c("red","blue"), mark.time = T)
legend("bottomleft", c("CS 0.5 LS F ","CS 3.0-->0.5 10 F"), col=c("red","blue"), lty=1)
abline(h=0.5)
abline(v=88, col="red")
abline(v=79, col="blue")

###  plot the inverse of a survival function

plot(myfit, fun="event", col=c("red","blue"), mark=3)


#cox regression analysis

cox1<-coxph(mySurv~mydata_df$`flyname`)
#coxph(mySurv~mydata$`Total Females`)
summary(cox1)
#install.packages("gtsummary")
library (gtsummary)
tbl_regression_ex1 <-
  coxph(mySurv ~ mydata_df$flyname) %>%
  tbl_regression(exponentiate = TRUE)

#coxph(mySurv~mydata$Fly.Line+mydata$Dead.females)

km_trt_fit <- survfit(Surv(mydata_df$`age in days`, mydata_df$`total Males`) ~ mydata_df$`flyname`, data=mydata_df)
summary(km_trt_fit)
autoplot(km_trt_fit)

autoplot(survfit(cox1))



cox1<-coxph(mySurv ~ flyname, data=mydata_df)


#ggforest3(cox1,data=mydata_df, fontsize = 1.5, cpositions = c(0.02, 0.10, 0.42), font.x.size = 20)

summary(cox1)

tabcoxph(cox1)


#ggforest(cox1, data=mydata_df, fontsize = 1.5, cpositions = c(0.02, 0.10, 0.42))

#p + theme_survminer(
# font.x= c(14, "bold", "darkgreen")
#)

#  ggpar(p,xlab = "Hazard ratio", font.tickslab = c(20,"bold"))
#p+theme(axis.text.x = element_text(face="bold")) 

summary(cox1)

ftest <- cox.zph(cox1)
ftest
ggcoxzph(ftest)

ggcoxdiagnostics(cox1, type="dfbeta", ox.scale = "time")
