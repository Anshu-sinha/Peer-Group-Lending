
#Clearing the global Enviornment
rm(list = ls())
#------------------------------------------------------------------------------------------

#Loading the data into R
setwd("F://BA-R programming//STAT case study//1. PEER GROUP LENDING - REGRESSION")
LoansData <- read.csv("LoansData.csv")


#Understanding the data

str(LoansData)
View(LoansData)
head(LoansData)
names(LoansData)

summary1 <- summary(LoansData)
write.csv(summary(LoansData),"summary1.csv")
View(summary1)

require(psych)
describe(LoansData)
summary2 <- str(LoansData) #showing null why????
#------------------------------------------------------------------------------------------

#Data Preparation
require(dplyr)
require(tidyr)

#changing the names of the variables having dots to underscore
names(LoansData) <- gsub(".","_",names(LoansData),fixed = T)



#now we will change the numerical values into numeric which is shown as factor
#Interest_Rate, Debet_to_Income_Ratio, Fico_range

LoansData$Interest_Rate <-as.numeric(gsub("%","",as.character(LoansData$Interest_Rate)))
class(LoansData$Interest_Rate)

#Same will be done for Debt_to_Income_ratio
LoansData$Debt_To_Income_Ratio <- as.numeric(gsub("%","",as.character(LoansData$Debt_To_Income_Ratio)))
class(LoansData$Debt_To_Income_Ratio)

#NOW FOR FICO RANGE WE WILL SEPARATE THE ranges and we will take the average of the ranges and after that we will remove the low and high fico values
#LoansData1 <- LoansData
LoansData <- LoansData %>% separate(FICO_Range, c("fico_low","fico_high"),"-")

                            #is.na(LoansData1$fico_high)          just to check the Na's
                            #LoansData1[!complete.cases(LoansData1$fico_high),]
LoansData <- LoansData %>% mutate(fico_low = as.numeric(fico_low),
                                  fico_high = as.numeric(fico_high),
                                  FICO_avg = (fico_low + fico_high)/2)

#Deleting the not required data
LoansData$fico_high <- NULL
LoansData$fico_low <- NULL
#------------------------------------------------------------------------------------------------


#User defined func to create audit report for numerical variables

mydata_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_perct = mean(is.na(x))
  sum = sum(x,na.rm = T)
  mean = mean(x,na.rm = T)
  median = median(x,na.rm = T)
  std_dev = sd(x,na.rm = T)
  cv = sd(x,na.rm = T)/mean(x,na.rm = T)
  var = var(x,na.rm = T)
  range = max(x,na.rm = T)-min(x,na.rm = T)
  perctl = quantile(x, p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n,NMISS=nmiss,NMiss_Perct=nmiss_perct,Sum=sum,Avg=mean,Median=median,std_dev=std_dev,Cv=cv,Variance= var, Range=range,perctl=perctl))
}

#User defined func to create audit report for categorical variables

mydata_cat =function(x){
  Var_Type = class(x)
  n = length(x)
  nmiss = sum(is.na(x))
  freq = list(table(x))
  prop = list(prop.table(table(x)))
  return(c(Var_Type=Var_Type,n = n, miss = nmiss,frequency= freq,proportion= prop))
} 
  

#Applying the function to the variable
mydata_num(LoansData$Interest_Rate) 
mydata_cat(LoansData$Loan_Purpose)


#we will separte the numerical and factor variable to treat them
numeric_Vars <- names(LoansData)[sapply(LoansData,FUN = is.numeric)]
Char_Vars <- names(LoansData)[sapply(LoansData,FUN = is.factor)]


#Data audit report for numerical variable
Summary_stats_num <- t(apply(LoansData[numeric_Vars], 2, FUN = mydata_num))
View(Summary_stats_num)  

#Data audit report for factor variable
Summary_stats_Char <- t(apply(LoansData[Char_Vars],2,FUN = mydata_cat))


#con <- file("stats_cat_vars.txt")
#sink(con, append=TRUE)
##sink(con, append=TRUE, type="message")        #what does this mean and why is it used??
#summary_stats_cat
#sink()

#------------------------------------------------------------------------------------------------

#Outlier Treatment

outlier_treat <- function(x){
  Uc1 = quantile(x, p= 0.99,na.rm = T)
  LC1 = quantile(x, p= 0.01,na.rm = T)
  x= ifelse(x>Uc1,Uc1,x)
  x= ifelse(x<LC1,LC1,x)
  return(x)
}

#applying the outlier treatment and making a new table of the numerical variables.

LoansData_num <- data.frame(apply(LoansData[numeric_Vars],2,FUN = outlier_treat))

#number of missings each variable at once
#but still have doubts that what it does
sapply(LoansData,FUN = function(x) sum(is.na(x))) 
#here sum(is.na(x)) is defined as function so that it can be used all together


#missing treatment of numerical variable
miss_treat_num = function(x){
  x[is.na(x)]= mean(x,na.rm = T) #replacing the missing values
  return(x)
}

LoansData_num <- data.frame(apply(LoansData_num,2,FUN = miss_treat_num))


#missing treatment of Factor variable

miss_treat_char <- function(x){
  x[is.na(x)] = x[which.max(prop.table(table(x)))] 
  return(x)
}

LoansData_char <- data.frame(apply(LoansData[Char_Vars],2,FUN = miss_treat_char))


#Number of missing in each variable
sapply(LoansData_char,FUN = function(x) sum(is.na(x)))
sapply(LoansData_num,FUN = function(x) sum(is.na(x)))

#------------------------------------------------------------------------------------------------

#Assumption check

hist(LoansData$Interest_Rate)
Cor_mat <- data.frame(cor(LoansData_num))
write.csv(Cor_mat,"cor_mat.csv")

require(corrplot)
corrplot(cor(LoansData_num), method = "number",number.font = 1,type = "full") #To increase the plot size we had to include type="full"

#to identify which categorical variable is significant

library(data.table)
LoansData <- as.data.table(LoansData) #it is used to check whether the object is data.table or coerce if possible
LoansData <- LoansData[!(is.na(Interest_Rate)),]
names(LoansData)


summary(aov(LoansData$Interest_Rate~LoansData$Loan_Length)) #as p value is very less then this is significant
summary(aov(LoansData$Interest_Rate~LoansData$Loan_Purpose)) #significant
summary(aov(LoansData$Interest_Rate~LoansData$Home_Ownership))#significant
summary(aov(LoansData$Interest_Rate~LoansData$Employment_Length))#Not significant as it shoul be less than 0.1
summary(aov(LoansData$Interest_Rate~LoansData$State)) #not significant
#why we have chosen this loan length, loan purpose, home ownership,employment length, state only????

#converting categorical into dummy for dividing it into development and training
require("caret")
names(LoansData_char)


#Loan length
LoansData_char$Loan_Length <- factor(LoansData_char$Loan_Length)
dv1 <- dummyVars(~Loan_Length,data = LoansData_char)
dummy_Loan_length <- data.frame(predict(dv1,LoansData_char))[-1]

#loan purpose

LoansData_char$Loan_Purpose <- factor(LoansData_char$Loan_Purpose)
dv2 <- dummyVars(~Loan_Purpose , data = LoansData_char)
dummy_loan_purpose <-data.frame(predict(dv2,LoansData_char))[-1]

#Home ownership

LoansData_char$Home_Ownership <- factor(LoansData_char$Home_Ownership)
dv3 <- dummyVars(~Home_Ownership,data = LoansData_char)
dummy_home_ownership <- data.frame(predict(dv3,LoansData_char))[-1]
#levels are 14 but only 13 are showing in which CAR is not included why????

#combining all the numerical and categorical data together

LoansData1 <- data.frame(cbind(LoansData_num,dummy_Loan_length,dummy_loan_purpose,dummy_home_ownership))

#changing the names of variable in which dots are there so it will be replaced by underscore
names(LoansData1) <- gsub(".","_",names(LoansData1),fixed = T)
str(LoansData1)

#-------------------------------------------------------------------------------------------------

#Now we will divide the data into training and testing or development and validation

library(car)
samp <- sample(1:nrow(LoansData1),floor(nrow(LoansData1)*0.7))
dev <- LoansData1[samp,]
Val <- LoansData1[-samp,]
nrow(LoansData1)
nrow(dev)
nrow(Val)
#-------------------------------------------------------------------------------------------------

#Develope or train the data
names(LoansData1)
fit <- lm(Interest_Rate~.,data = dev)
summary(fit) 
vif(fit)

cor(dev$Amount_Requested,dev$Amount_Funded_By_Investors)


#diagnostic plots
layout(matrix(c(1,2,3,4),2,2))  #optional 4 graphs/page
plot(fit)
hist(residuals(fit))


#reducing the variables in step function
step(fit,direction = "both") #It helps in reducing the variables based on AIC value(low value good model)

fit2<-lm(formula = Interest_Rate ~ Amount_Requested + Amount_Funded_By_Investors + 
                   Monthly_Income + Open_CREDIT_Lines + Inquiries_in_the_Last_6_Months + 
                   FICO_avg + Loan_Length_60_months + Loan_Purpose_credit_card + 
                   Loan_Purpose_debt_consolidation + Loan_Purpose_moving + Loan_Purpose_other + 
                   Loan_Purpose_small_business + Loan_Purpose_wedding + Home_Ownership_NONE + 
                   Home_Ownership_OWN + Home_Ownership_RENT, data = LoansData1)
summary(fit2)
car::vif(fit2) #Two of the variable is having high value so we will drop the 1 and we will continue with the rest

fit3 <- lm(formula = Interest_Rate ~  Amount_Funded_By_Investors + 
             Monthly_Income + Open_CREDIT_Lines + Inquiries_in_the_Last_6_Months + 
             FICO_avg + Loan_Length_60_months + Loan_Purpose_credit_card + 
             Loan_Purpose_debt_consolidation + Loan_Purpose_moving + Loan_Purpose_other + 
             Loan_Purpose_small_business + Loan_Purpose_wedding + Home_Ownership_NONE + 
             Home_Ownership_OWN + Home_Ownership_RENT, data = LoansData1)
summary(fit3)
car::vif(fit3)  #all the variable is having vif value less than 5 
#-------------------------------------------------------------------------------------------------

#scoring the data i.e. we will predict the value

dev1 <- data.frame(cbind(dev,Pred_int_rate = predict(fit3,newdata = dev)))
val1 <- data.frame(cbind(Val,pred_int_rate = predict(fit3,newdata = Val)))


#Comparing the matrix between Development and Validation

#Mape
dev_mape <- mean(abs(dev1$Interest_Rate-dev1$Pred_int_rate)/dev1$Interest_Rate)
val_mape <- mean(abs(val1$Interest_Rate-val1$pred_int_rate)/val1$Interest_Rate)

print(dev_mape)
print(val_mape)


#RMSE

dev_rmse <- sqrt(mean((dev1$Interest_Rate-dev1$Pred_int_rate)**2))
val_rmse <- sqrt(mean((val1$Interest_Rate-val1$pred_int_rate)**2))

print(dev_rmse)
print(val_rmse)


#correlation
cor(dev1$Interest_Rate,dev1$Pred_int_rate)
cor(val1$Interest_Rate,val1$pred_int_rate)
#-------------------------------------------------------------------------------------------------------

#Creating Decile 
#Finding decile location

decLocation <- quantile(dev1$Pred_int_rate,probs = seq(0.1,0.9,by = 0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
val1$decile <- findInterval(val1$pred_int_rate,c(-Inf,decLocation, Inf))


# Decile analysis report
val_DA <- sqldf::sqldf("select decile, count(decile) as count, avg(pred_int_rate) as avg_pre_int_rate,  
                avg(Interest_Rate) as avg_Int_Rate
                from val1
                group by decile
                order by decile desc")
View(val_DA)









