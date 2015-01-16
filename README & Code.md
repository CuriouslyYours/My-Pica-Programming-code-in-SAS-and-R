# My-Pica-Programming-code-in-SAS-and-R
This file contains all my SAS &amp; R codes. Please contact me at partha2v@gmail.com for further clarification, I am aspiring to be a Analyst and have more than 2 years of experience in Marketing Analytics, Predictive Modeling and other statistical analysis

SAS code for Regression and ARIMA for Kaggle Walmart data
*By Partha Chowdhury, Partha2v@gmail.com, Coding for BEST project on 'Walmart sales prediction', Section-B, Group-8;

PROC IMPORT DATAFILE ='c:\SASData\W6.csv'  OUT = walmart6 REPLACE;
RUN;
*Print data;
PROC PRINT DATA = walmart6;    
TITLE 'walmart6 data with missing value as 0'; 
RUN;


*Summarize data;
proc means data=walmart6;
var  week Temperature Fuel_Price MarkDown1 MarkDown2 MarkDown3 MarkDown4 MarkDown5 CPI Unemployment;
run;


*plot;
PROC GPLOT DATA=walmart6;
PLOT Salest*week;
RUN;

*with nlag=4;
PROC ARIMA DATA=walmart6;
IDENTIFY VAR=salest(1) nlag=4;
RUN;
*With estimation of p=1;
PROC ARIMA DATA=walmart6;
IDENTIFY VAR=salest(1);
estimate p=1; 
RUN;

*ARIMA with estimation of p=1 q=1;
PROC ARIMA DATA=walmart6;
IDENTIFY VAR=salest(1);
estimate p=1 q=1; 
RUN;

*FORECASTING on walmart6 dataset for LEAD=30;
PROC ARIMA DATA=walmart6;
IDENTIFY VAR=salest(1);
estimate p=1 q=1;  
FORECAST LEAD=30 INTERVAL=week ID=week OUT=RESULTS;
RUN;

-----
R coding by me--
##Regression

library("car")
library("QuantPsyc")
library("boot")
library(foreign)
install.packages("lmtest")
library("lmtest")

setwd("/Users/parthachowdhury/Documents/Partha-Nov-backup/Term-6-imt/PDM-C-AKC/Rdata/Regression analysis")

data<- read.spss("Demand.sav", use.value.labels=TRUE, to.data.frame=TRUE) 
data

reg1<-lm(Y ~ X2, data=data)
summary(reg1)
reg2<-lm(Y ~ X3, data=data)
summary(reg2)
reg3<-lm(Y ~ X4, data=data)
summary(reg3)
reg4<-lm(Y ~ X5, data=data)
summary(reg4)
reg5<-lm(Y ~ X6, data=data)
summary(reg5)
#Individually each variable is significant.

reg6<-lm(Y ~ X2 + X3 + X4 + X5 + X6, data=data)
summary(reg6) #only X3 & X4 is significant.
lm.beta(reg6) 
# X2        X3        X4        X5        X6 
# 0.409723 -0.982940  1.162069  0.728654 -0.384003 
#lm.beta Computes the standardized regression coeffients (beta) from an object of class (lm).
vif(reg6)
mean(vif(reg6))
kappa(reg6)

# > vif(reg6)
# X2       X3       X4       X5       X6 
# 52.76145 21.10261 55.84667 74.30336 86.13143 
# > mean(vif(reg6))
# [1] 58.0291
# > kappa(reg6)
# [1] 8546.087


reg7<-lm(Y ~ X2 + X3 + X4 + X5, data=data)
summary(reg7)
vif(reg7)
mean(vif(reg7))
kappa(reg7)
lm.beta(reg7)

# > vif(reg7)
# X2       X3       X4       X5 
# 52.70104 18.90128 29.05099 39.76141 
# > mean(vif(reg7))
# [1] 35.10368
# > kappa(reg7)
# [1] 8604.743
# > lm.beta(reg7)
# X2         X3         X4         X5 
# 0.4198926 -0.9215501  0.9478850  0.4854744 

reg8<-lm(Y ~ X3 + X4 + X5, data=data)
summary(reg8)
vif(reg8)
mean(vif(reg8))
kappa(reg8)
lm.beta(reg8)

#all significant.

confint(reg8)
anova(reg1)
coefficients(reg1)
fitted(reg1)
residuals(reg1)
influence(reg1)

vif(reg6)
mean(vif(reg6))
kappa(reg6)

dwt(reg8)
bptest(reg8) #not working., package lmtest required


round(data, digits=3)

plot(reg1)
hist(residuals(reg1))


*7th Iteration: ARIMA Identification;
PROC ARIMA DATA=walmart6;
IDENTIFY VAR=salest STATIONARITY=(adf);
RUN;

----
##Dummy regression code

# Load the required Packages

library("car")
library("QuantPsyc")
library("boot")

# Set the default Folder

setwd("/Users/parthachowdhury/Documents/Partha-Nov-backup/Term-6-imt/PDM-C-AKC/Rdata/Dummy")
getwd()

# Dummy Regression model

dummy<- read.csv ("dummy1.csv", header=TRUE)
dummy
names(dummy)

reg1<-lm(RoE ~ Industry, data=dummy)
reg1
summary(reg1)
#Wrong regression as Industry representation doesnt have any mathematical sense. 
#It is just a representatin of industry and dont have any numerical value.

reg2<-lm(RoE ~ 0 + Banking + Auto + Realty, data=dummy)
summary(reg2)
#forced 0 intercept as it makes sense. all significant coeff.

reg3<-lm(RoE ~ Auto + Realty, data=dummy) #Intercept & Auto not significant.
summary(reg3)

# Seasonal Anomaly: Day Effects

days <- read.csv ("days_ret.csv", header=TRUE)
days
names(days)
reg4 <- lm( BSE ~ 0+ DummyMon+DummyTues+DummyWed+DummyThur+DummyFri, data=days)
summary(reg4)

reg5 <- lm( BSE ~ DummyTues+DummyWed+DummyThur+DummyFri, data=days)
summary(reg5)

reg6 <- lm( Sq_ret ~ 0+ DummyMon+DummyTues+DummyWed+DummyThur+DummyFri, data=days)
summary(reg6)

reg7 <- lm( Sq_ret ~ DummyTues+DummyWed+DummyThur+DummyMon, data=days)
summary(reg7)


# Seasonal Anomaly: Month Effects

month<- read.csv ("months_ret.csv", header=TRUE)
month
names(month)

reg8<-lm(CNX_NIFTY ~ 0+ DummyJan+DummyFab+ DummyMar+DummyApr+DummyMay+DummyJun +DummyJul+DummyAug+DummySept+DummyOct+DummyNov+DummyDec , data=month)
summary(reg8)

reg9<-lm(CNX_NIFTY ~DummyJan+DummyFab+ DummyApr+DummyMay+DummyJun +DummyJul+DummyAug+DummySept+DummyOct+DummyNov+DummyDec , data=month)
summary(reg9)
#Significance changed with March as reference


# Identifying and removing the seasonality

iip<- read.csv ("iip.csv", header=TRUE)
iip
names(iip)
plot(iip)

reg10<- lm(iip~Time, data=iip)
summary(reg10)
residuals(reg10)
iip$DT<- residuals(reg10) #save residual--. detrended series, seaonality is removed
#by taking residual out of dummy variale model.
write.csv(iip, file="iip_r.csv") 

iip_r<- read.csv ("iip_r.csv", header=TRUE)
names(iip_r)

reg11<-lm(DT ~ 0+Djan + Dfeb+ Dmar+Dapril+Dmay+Djun +Djul+Daug+Dsep+Doct+Dnov+Ddec , data=iip)
summary(reg11)
iip$DTS<- residuals(reg11)
write.csv(iip, file="iip_rs.csv") 


# Two Way ANOVA

dummy2<- read.csv ("dummy2.csv", header=TRUE)
dummy2
names(dummy2)
reg1<-lm(RoE ~ Auto + Realty + Old, data=dummy2)
summary(reg1)
#????
# Two way ANCOVA

dummy3<- read.csv ("dummy3.csv", header=TRUE)
dummy3
names(dummy3)
reg13<-lm(PE ~ Auto + Realty + Old + RoE, data=dummy3)
summary(reg13)

# Analysisng Structural Breaks ??

structuralbreak<- read.csv ("structural breaks.csv", header=TRUE)
structuralbreak
names(structuralbreak)
reg1<-lm(Sales ~ Dummy_year + Advertisement + int_variable, data=structuralbreak)
summary(reg1)

# Efficiency Modelling

efficiency<- read.csv ("efficiency.csv", header=TRUE)
efficiency
names(efficiency)
reg1<-lm(Sales ~ d2 + d3 + d4 + Promotion_expenses+RandD_expences+d2x1+d2x2+d3x1+d3x2+d4x1+d4x2, data=efficiency)
summary(reg1)
-----
##Logistic Regression
install.packages("car")
install.packages("mlogit")

library("car")
library("mlogit")


setwd("/Users/parthachowdhury/Documents/Partha-Nov-backup/Term-6-imt/PDM-C-AKC/Rdata/Logistic")
getwd()

# Case 1: home loan
# Linear Probability model 1

home<- read.csv("lpm.csv", header=TRUE)
home
names(home)

head(home) # shows first 6 rows of the dataframe
attach(home)
plot(Income, Prob)

model0<-lm(Prob~Income, data=home)
summary(model0)
fitted.values(model0)
plot(fitted.values(model0))
library("lmtest")
bptest(model0)
residuals(model0)	
plot(residuals(model0))


#Case 2: Migration Data

mydata<- read.csv("migration.csv", header=TRUE)
mydata
names(mydata)

head(mydata) # shows first 6 rows of the dataframe
attach(mydata)
plot(agri_field_size, migration)
plot(members_family, migration)


# Linear Probability model 2

model0<-lm(migration~agri_field_size, data=mydata)
summary(model0)
fitted.values(model0)
plot(fitted.values(model0))
library("lmtest")
bptest(model0)

residuals(model0)	
plot(residuals(model0))


# Logit model

model1<-glm(migration~agri_field_size, data=mydata, family=binomial())
summary(model1)
exp(-0.30438)

modelchi<- model1$null.deviance - model1$deviance
#Null Deviance- model without an independent variable
#Deviance-- badness of fit-- higher is worst
modelchi
chidf <- model1$df.null - model1$df.residual 
chidf
chisq.prob <- 1 - pchisq(modelchi,chidf)
chisq.prob

summary(agri_field_size)

plot(agri_field_size, fitted.values(model1)) #Sigmoid function.

model2<-glm(migration~members_family, data=mydata, family=binomial())
summary(model2)
exp(0.23872)
plot(members_family, fitted.values(model2))


model3<-glm(migration~agri_field_size + alt_occu, data=mydata, family=binomial())
summary(model3)
plot(agri_field_size, fitted.values(model3))

model4<-glm(migration~members_family + alt_occu, data=mydata, family=binomial())
summary(model4)
plot(members_family, fitted.values(model4))
summary(members_family)

model5<-glm(migration~agri_field_size+ members_family + alt_occu, data=mydata, family=binomial())
summary(model5)
plot(agri_field_size+members_family, fitted.values(model5))


model6<-glm(migration~agri_field_size+ members_family + alt_occu + Caste, data=mydata, family=binomial())
summary(model6)
plot(agri_field_size+members_family, fitted.values(model6))

model7<-glm(migration~agri_field_size+ members_family + alt_occu , data=mydata, family=binomial(link="logit"))
summary(model7)
plot(agri_field_size+members_family, fitted.values(model7))


y <- cbind(migration)
x <- cbind(agri_field_size, members_family, alt_occu)
model7<-glm(y~x , family=binomial(link="logit"))
summary(model7)


# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(model7, type = "link")))
LogitScalar * coef(model7)

# Logit model predicted probabilities
plogit<- predict(model7, type="response")
summary(plogit)

# Percent correctly predicted values

table(true = Y, pred = round(fitted(model7))) 

====Theory====
#   R objects that reside in other R objects can require a lot of typing to access. For example, to refer to a variable x in a dataframe df, one could type df$x. This is no problem when the dataframe and variable names are short, but can become burdensome when longer names or repeated references are required, or objects in complicated structures must be accessed.
# 
# The attach() function in R can be used to make objects within dataframes accessible in R with fewer keystrokes. As an example:
#   
#   ds = read.csv("http://www.math.smith.edu/r/data/help.csv")
# names(ds)
# attach(ds)
# mean(cesd)
# [1] 32.84768

---
## Factor Analysis
 # Packages required for the session

 install.packages("FAiR")
 install.packages("psych")
 install.packages("FactoMineR")
 install.packages("corpcor")
 install.packages("GPArotation")
 install.packages("lmtest")

 library(FAiR)
 library(psych)
 library(FactoMineR)
 library(corpcor)
 library(GPArotation)
 library("car")
 library("QuantPsyc")
 library("boot")
 library("lmtest")

 # Set the default folder

 setwd("/Users/parthachowdhury/Documents/Partha-Nov-backup/Term-6-imt/PDM-C-AKC/Rdata/Reg Diagnostics")
 getwd() 

 # Open the data file 

 mydata <- read.csv("Data_lifestyle.csv", header=TRUE)
 mydata
 names(mydata)
 
 # Applying Regression Model
 
 Reg1<- lm( Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21, data =mydata)
 summary(Reg1)

 # Testing for Multicollinearity

 vif(Reg1)
 mean(vif(Reg1))
 kappa(Reg1) # gives the condition Index of the model 

 # Testing for Autocorrelation

 dwt(Reg1)

 # Testing for Hateroscedasticity

 plot(Reg1)
 residuals(Reg1)
 plot(residuals(Reg1))
 bptest(Reg1) # Package lmtest is required for this


 # Creating the R dataframe
 
 mydata.df <- data.frame(mydata)
 mydata
 names(mydata)
 mydata1 <- mydata.df[,c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11", "X12", "X13", "X14","X15", "X16", "X17", "X18", "X19", "X20", "X21")]
 mydata1
 names(mydata1)
 
  
 correlationmatrix <- cor(mydata1)
 correlationmatrix
 round(correlationmatrix, 2)
 
 # KMO and Bartlett Test

 cortest.bartlett(mydata1)
 cortest.bartlett(correlationmatrix, n=400)
 KMO(mydata1)
 det(correlationmatrix)
 det(cor(mydata1))

 pc1<- principal(mydata1, nfactors=21, rotate="none")
 pc1
 plot(pc1$values, type="b") # plot Eigen Values

 pc2<- principal(mydata1, nfactors=6, rotate="none")
 pc2

 factor.model(pc2$loadings)
 factor.residuals(correlationmatrix, pc2$loadings)
 residuals <- factor.residuals(correlationmatrix, pc2$loadings)
 
 residuals <- as.matrix(residuals[upper.tri(residuals)]) 
 residuals
 hist(residuals)

 pc3 <- principal(mydata1, nfactors=6, rotate="varimax")
 pc3
 print.psych(pc3, cut=0.3, sort=TRUE)
 pc4 <- principal(mydata1, nfactors=6, rotate="varimax",scores=TRUE)
 pc4
 mydata2 <- cbind(mydata, pc4$scores)
 mydata2
 names(mydata2)

 Reg2<- lm( Y ~ RC1+RC2+RC3+RC4+RC5+RC6, data =mydata2)
 summary(Reg2)
 vif(Reg2)
 mean(vif(Reg2))
 dwt(Reg2)
 plot(Reg2)
 residuals(Reg2)
 plot(residuals(Reg2))
 bptest(Reg2) 

 # Outlier Testing

 cooks.distance(Reg2)
 plot(cooks.distance(Reg2))

 mydata2$cooks.distance<- cooks.distance(Reg2)
 mydata2
 mydata2.df <- data.frame(mydata2)
 mydata2
 names(mydata2)

 mydatafinal <- subset(mydata2.df, cooks.distance < 0.01)
 mydatafinal

 Reg3<- lm( Y ~ RC1+RC2+RC3+RC4+RC5+RC6, data =mydatafinal)
 summary(Reg3)
 
 write.csv(mydatafinal, file="mydata.csv")  
---
##ARIMA
# Load the required Packages

install.packages("forecast")
library("forecast")
library("tseries")

# Set the default Folder

setwd("/Users/parthachowdhury/Documents/Partha-Nov-backup/Term-6-imt/PDM-C-AKC/Rdata/ARIMA")
getwd()

arima<- read.csv ("budget file.csv", header=TRUE)
arima
names(arima)

attach(arima)

t <-date
plot(t,RR_Tax)
plot(t, Cap_Exp)

adf.test(Cap_Exp, alternative="stationary", k=0)
adf.test(Cap_Exp, alternative="explosive", k=0) #?



lntax=log(RR_Tax)
rtax=diff(lntax)
arima<-arima(rtax,order=c(1,0,1))
summary(arima)

arima1<-arima(rtax,order=c(2,0,1))
summary(arima1)

arima2<-auto.arima(rtax)
summary(arima2)

res<-residuals(arima2)
res
acf(res)
pacf(res)
adf.test(res,alternative="stationary")
Box.test(res)

 mypredict<-predict(arima2,n.ahead=10)
plot(mypredict) #??
mypredict


arima2<-auto.arima(rtax)
mypredict<-predict(arima2,n.ahead=10)
plot(rtax)
arima2<-arima(rtax,order=c(1,0,1))
mypredict<-predict(arima2,n.ahead=10)
plot(rtax)



plot(forecast(arima2))
plot(forecast(arima2))
arima2<-arima(rtax,order=c(1,0,1))
plot(forecast(arima2))
 



#ARIMA(1,0,1) forecasting
arima2<-arima(rtax,order=c(1,0,1))
mypredict<-predict(arima2,n.ahead=1)
plot(lntax)
lines(mypredict$pred,col="blue")
lines(mypredict$pred+2*mypredict$se,col="red")
lines(mypredict$pred-2*mypredict$se,col="red")
----
##Credit Score Modeling
matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)install.packages("car")
install.packages("mlogit")

library("car")
library("mlogit")
library("QuantPsyc")
library("boot")
library(foreign)

setwd("/Users/parthachowdhury/")
getwd()
dir()
#Reading the File

Proj = read.csv("Credit_Scoring_Training.csv", header=TRUE)
names(Proj)
which(is.na(Proj$SeriousDlqin2yrs))
which(is.na(Proj$RevolvingUtilizationOfUnsecuredLines))
which(is.na(Proj$age))
which(is.na(Proj$DebtRatio))
which(is.na(Proj$NumberOfTime30.59DaysPastDueNotWorse))
which(is.na(Proj$MonthlyIncome))
which(is.na(Proj$NumberOfOpenCreditLinesAndLoans))
which(is.na(Proj$NumberOfTimes90DaysLate))
which(is.na(Proj$NumberRealEstateLoansOrLines))
which(is.na(Proj$NumberOfTime60.89DaysPastDueNotWorse))
which(is.na(Proj$NumberOfDependents))

#Replace Missing values with mean value from the column

Proj$MonthlyIncome[is.na(Proj$MonthlyIncome)] = mean(Proj$MonthlyIncome, na.rm=TRUE) 
Proj$NumberOfDependents[is.na(Proj$NumberOfDependents)] = 
  mean(Proj$NumberOfDependents, na.rm=TRUE) 

Credit_Score = glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+                                
NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+
NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+   
NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents, 
                   data = Proj, family = "binomial")
summary(Credit_Score)

Credit_Score = as.list(Credit_Score)
vif(Credit_Score)

#Removing Correlated IVs

Credit_Score_2= glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+                                
                     NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+
                     NumberOfOpenCreditLinesAndLoans+NumberRealEstateLoansOrLines+   
                     NumberOfDependents, 
                   data = Proj, family = "binomial")
summary(Credit_Score_2)

Credit_Score_2 = as.list(Credit_Score_2)
vif(Credit_Score_2)

#Removing Insignificant Variables and preparing final model
Credit_Score_Final= glm(SeriousDlqin2yrs ~ age+                                
                      NumberOfTime30.59DaysPastDueNotWorse+MonthlyIncome+
                      NumberRealEstateLoansOrLines+   
                      NumberOfDependents, 
                    data = Proj, family = "binomial")
summary(Credit_Score_Final)

Credit_Score_Final = as.list(Credit_Score_Final)
vif(Credit_Score_Final)

Credit_Score_Final_Step= step(Credit_Score_Final,SeriousDlqin2yrs ~ age+                                
                          NumberOfTime30.59DaysPastDueNotWorse+MonthlyIncome+
                          NumberRealEstateLoansOrLines+NumberOfDependents, 
                        data = Proj, family = "binomial")

modelchi<- Credit_Score_Final$null.deviance - Credit_Score_Final$deviance
modelchi
chidf <- Credit_Score_Final$df.null - Credit_Score_Final$df.residual 
chidf
chisq.prob <- 1 - pchisq(modelchi,chidf)
chisq.prob
#Since the p-value is <0.05 hence model is significant

# Calculate average marginal effects

LogitScalar <- mean(dlogis(predict(Credit_Score_Final, type = "link")))
MaginalEffects = LogitScalar * coef(Credit_Score_Final)
MaginalEffects   

# Classification Table

CalcProbs <- fitted(Credit_Score_Final_Step)
ClassTable <- data.frame(response = Proj$SeriousDlqin2yrs, 
                      predicted = round(CalcProbs,0))
xtabs(~ predicted + response, data = ClassTable)
---
