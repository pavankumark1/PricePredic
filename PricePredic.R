
#WORKING DIRECTORY
setwd("~/Analysis")


#reading IPPS data into a variable ipps
install.packages("xlsx")
library(xlsx)

ipps <- read.xlsx("IPPSdata.xlsx", sheetName = "FY 2014",header = TRUE)

colnames(ipps)

#removing pnemonia variables from the data
ipps <- ipps[,-c(2,3,4)]

#changing colname of ipps[,1] from "PROV" to "Provider.ID"
colnames(ipps)[1] <- "Provider.ID"

#reading inpatient data into variable inpatient 
inpatient <- read.csv("inpatientdata.csv", header= T,sep = ",")

colnames(inpatient)

#inpatient <- inpatient[,-c(4,5)]

#taking out the levels that are needed for the analysis. 
takerows <- list( "280 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W MCC",
                  "281 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W CC",
                  "282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC",
                  "291 - HEART FAILURE & SHOCK W MCC",
                  "292 - HEART FAILURE & SHOCK W CC", 
                  "293 - HEART FAILURE & SHOCK W/O CC/MCC")

#taking out the rows with DRG definitions AMI & HF
ami_hf <- inpatient[inpatient$DRG.Definition %in% unlist(takerows),]

colnames(ami_hf)

colnames(ami_hf)[2] <- "Provider.ID"

#taking the average of medicare payments.
#this step is done because we thought it would be appropriate to 
# take all payments into consideration 
ave <- NULL

for(i in nrow(ami_hf))
{
  ave <- (ami_hf$Average.Covered.Charges + ami_hf$Average.Total.Payments
          + ami_hf$Average.Medicare.Payments)/ 3
   
  ave<- data.frame(ave)
}
colnames(ave) <- "TOTAL_AVERAGE_PAYMENT"

ami_hf <- cbind(ami_hf,ave)

#removing the unwanted variables 
ami_hf <- ami_hf[,-c(10,11,12)]

#merging ipps data and inpatient data by provider ID 
ami_hf_ipps <- merge(ami_hf,ipps, by.x = "Provider.ID")

#reading hospital general data into variable general
hospital <- read.csv("hospitalgeneraldata.csv", header = T, sep = ",")

colnames(hospital)

#keeping only variables needed for the analysis
hospital1 <- hospital[,c(1,9,10,13)]

#merging inpatient, ipps and hospital general data. 
thedata <- merge(ami_hf_ipps,hospital1, by.y = "Provider.ID")

#zip code to GEOID transformation 
zip <- read.table("zip.txt", header = T, sep = ",")

#to keep the format I've added unwanted row and deleted it for the analysis
zip <- zip[,c(1:4)]

#reading ACS data
ACS <- read.csv("ACS.csv", skip = 1, header =  T, sep = ",")

# eliminating unwanted variables like percent and margin of error 
#this is done so as to make the analysis less comprehensive 
acs2 <-  ACS[, -grep("Margin", colnames(ACS), ignore.case = T)]
acs2 <- acs2[, -grep("Percent", colnames(acs2), ignore.case = T)]
acs2 <- acs2[, -grep("One.race", colnames(acs2), ignore.case = T)]
acs2 <- acs2[, -grep("Two.or.more.races", colnames(acs2), ignore.case = T)]
acs2 <- acs2[, -grep(".years.and.over", colnames(acs2), ignore.case = T)]

acs2 <- acs2[,-c(19,20,21,22,23)]

#changing the name of column 2 to "GEOID" for merging 
colnames(acs2)[2] <- "GEOID"

#Merging acs and zip data by geoID 
zip_acs <- merge(zip,acs2, by.y = "GEOID")

#merging the data and zipacs by zipcodes.
colnames(thedata)[7] <- "ZIPCODE"
colnames(zip_acs)[2] <- "ZIPCODE"

analysisdata <- merge(thedata,zip_acs, by.x = "ZIPCODE")

colnames(analysisdata)

#remving variables which I hypothesize that they have nothing to 
#do with the prediction of prices
analysisdata <- analysisdata[,-c(2,4,5,7,8,18:22,38,38,46:58)]

#taking our rows with AMI and HF into seperate tables
amirows  <- list( "280 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W MCC",
                  "281 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W CC",
                  "282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC")

#amidata is the data for acute myocardial infration
amidata <- analysisdata[analysisdata$DRG.Definition %in% unlist(amirows),]

amidata<- amidata[,-c(6,7)] #removing HF columns 

amidata <- amidata[,-c(26)] #removing columns which are confusing to use 

#write.csv(amidata,"amidata2.csv", row.names = F)

#taking out rows with HF into seperate table
hfrows <- list( "291 - HEART FAILURE & SHOCK W MCC",
                "292 - HEART FAILURE & SHOCK W CC", 
                "293 - HEART FAILURE & SHOCK W/O CC/MCC")

#hfdata is the data for heart failure 
hfdata <- analysisdata[analysisdata$DRG.Definition %in% unlist(hfrows),]

hfdata<- hfdata[,-c(8,9)] #removing AMI columns

hfdata<- hfdata[,-26] #removing columns which are confusing to use

#write.csv(hfdata, "hfdata2.csv",row.names = F)

#### Done with Merging of the data  ####
## here amidata has all the data about acute myocardial infraction 
## hfdata has all data about heart failure

#histogram for amidata 
hist(amidata$TOTAL_AVERAGE_PAYMENT ,main = "MEDICARE PAYMENTS",
    ylab = "counts",xlab = "Average Medicare Payments ",col = "blue" )

#linear regression of amidata 
#plot(amidata$Average.Covered.Charges~., data = amidata )

str(amidata)

amidata$ZIPCODE <- as.factor(amidata$ZIPCODE) 
amidata$DRG.Definition <- as.factor(as.vector(amidata$DRG.Definition))

amidata$Estimate..RACE...Black.or.African.American <- as.numeric(amidata$Estimate..RACE...Black.or.African.American)
amidata$Estimate..RACE...American.Indian.and.Alaska.Native <- as.numeric(amidata$Estimate..RACE...American.Indian.and.Alaska.Native)
amidata$Estimate..RACE...Asian <- as.numeric(amidata$Estimate..RACE...Asian)
amidata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander <- as.numeric(amidata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander)
amidata$Estimate..RACE...Some.other.race <- as.numeric(amidata$Estimate..RACE...Some.other.race)
amidata$Hospital.overall.rating <- as.numeric(amidata$Hospital.overall.rating)

str(amidata)
#linear regression is used here for variable selection 
#I have eliminated certain variables which don't serve any purpose in linear regression
# like ZIpcode,provider city etc. 
lmfit <- lm(amidata$TOTAL_AVERAGE_PAYMENT~., data = amidata[,-c(1,3,8)])

summary(lmfit)

#plots for linear model (lmfit)
#plot residual vs fitted plot 
plot(resid(lmfit) ~ fitted(lmfit), xlab = "fitted", ylab = "residual",
     main = "Residual VS fitted  for AMI")

#qqplot for AMI  
qqnorm(fitted(lmfit), ylab = " ")

# based on the results of summary, the potential variables for the analysis
# are used in the 2nd linear regression, the R squared value suggests that
# these are the potential variables for the analysis. 

lmfit2 <- lm(amidata$TOTAL_AVERAGE_PAYMENT~ amidata$DRG.Definition+
             + amidata$Hospital.Ownership + amidata$Total.Discharges
             + amidata$Estimate..HISPANIC.OR.LATINO.AND.RACE...Total.housing.units
             + amidata$Hospital.overall.rating 
             + amidata$Estimate..RACE...American.Indian.and.Alaska.Native
             + amidata$Estimate..RACE...Asian
             + amidata$Estimate..RACE...Black.or.African.American
             + amidata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander
             + amidata$Estimate..RACE...Some.other.race
             + amidata$Estimate..RACE...White
             + amidata$Estimate..SEX.AND.AGE...10.to.14.years
             + amidata$Estimate..SEX.AND.AGE...15.to.19.years
             + amidata$Estimate..SEX.AND.AGE...Under.5.years
             + amidata$Estimate..SEX.AND.AGE...5.to.9.years
             + amidata$Estimate..SEX.AND.AGE...20.to.24.years
             + amidata$Estimate..SEX.AND.AGE...25.to.34.years
             + amidata$Estimate..SEX.AND.AGE...35.to.44.years
             + amidata$Estimate..SEX.AND.AGE...45.to.54.years
             + amidata$Estimate..SEX.AND.AGE...55.to.59.years
             + amidata$Estimate..SEX.AND.AGE...60.to.64.years
             + amidata$Estimate..SEX.AND.AGE...65.to.74.years
             + amidata$Estimate..SEX.AND.AGE...75.to.84.years
             + amidata$Estimate..SEX.AND.AGE...Female
             + amidata$Estimate..SEX.AND.AGE...Male,
               data = amidata )

summary(lmfit2)

#plot of residul vs fitted 
plot(lmfit2)

#data for further analysis includes only selected variables from linear regression

amicols <- list( "TOTAL_AVERAGE_PAYMENT", "DRG.Definition",
                 "Hospital.Ownership ", "Total.Discharges",
                 "Estimate..HISPANIC.OR.LATINO.AND.RACE...Total.housing.units",
                 "Hospital.overall.rating" ,
                 "Estimate..RACE...American.Indian.and.Alaska.Native",
                 "Estimate..RACE...Asian",
                 "Estimate..RACE...Black.or.African.American",
                 "Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander",
                 "Estimate..RACE...Some.other.race",
                 "Estimate..RACE...White",
                 "Estimate..SEX.AND.AGE...10.to.14.years",
                 "Estimate..SEX.AND.AGE...15.to.19.years",
                 "Estimate..SEX.AND.AGE...Under.5.years",
                 "Estimate..SEX.AND.AGE...5.to.9.years",
                 "Estimate..SEX.AND.AGE...20.to.24.years",
                 "Estimate..SEX.AND.AGE...25.to.34.years",
                 "Estimate..SEX.AND.AGE...35.to.44.years",
                 "Estimate..SEX.AND.AGE...45.to.54.years",
                 "Estimate..SEX.AND.AGE...55.to.59.years",
                 "Estimate..SEX.AND.AGE...60.to.64.years",
                 "Estimate..SEX.AND.AGE...65.to.74.years",
                 "Estimate..SEX.AND.AGE...75.to.84.years",
                 "Estimate..SEX.AND.AGE...Female",
                 "Estimate..SEX.AND.AGE...Male")

amidata2 <- amidata[,colnames(amidata) %in% unlist(amicols)]


##### model analysis for amidata #####

#Dividing the data into training and testing sets
index <- sample(nrow(amidata2), nrow(amidata2)*0.75, rep= F)

set.seed(8010) #generates random numbers 

ami_train <- amidata2[index,] #training sample 3/4th of the data

ami_test <- amidata2[-index,] #testing sample 1/4th of the data 

#linear regression analysis 
amidatalm <- lm(ami_train$TOTAL_AVERAGE_PAYMENT~.,data = ami_train)

summary(amidatalm) #summary directly gives the R squared value for Linear models

#lm prediction 
lm_predic <- predict(amidatalm,newdata = ami_test)

#RMSE calculation 
lm_rmse = sqrt(mean((ami_test$TOTAL_AVERAGE_PAYMENT - lm_predic)^2))

print(lm_rmse)

#CART analysis
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

amidata.rpart1 <- rpart(ami_train$TOTAL_AVERAGE_PAYMENT~., data = ami_train
                        ,control = rpart.control(cp=0.01))

dev.new()
rpart.plot(amidata.rpart1,type = 1, uniform = T) 

plotcp(amidata.rpart1)
printcp(amidata.rpart1)

#pruning the tree
#this function extracts the minimum CP value from the CP table 
mincp <- amidata.rpart1$cptable[which.min(amidata.rpart1$cptable[,"xerror"]),"CP"]

pfit<- prune(amidata.rpart1, cp = mincp)

dev.new()
rpart.plot(pfit, uniform = T) #; text(pfit,minlength = 2)

#creating prediction using test set
rpart_predic <- predict(pfit, newdata= ami_test)

#RMSE Calculation for CART
rpart_rmse = sqrt(mean((ami_test$TOTAL_AVERAGE_PAYMENT - rpart_predic)^2))

print(rpart_rmse)
#R squared value calculation for CART 
rpart_R <- 1 - (sum((ami_test$TOTAL_AVERAGE_PAYMENT- rpart_predic)^2)/
                  sum((ami_test$TOTAL_AVERAGE_PAYMENT-mean(ami_test$TOTAL_AVERAGE_PAYMENT))^2))

print(rpart_R)
####Random forest analysis ###### 
### for random forests, I'm using same train and test datasets 
#this is for comparison purposes.

library(randomForest)

#random forests 
rf_fit <- randomForest(ami_train$TOTAL_AVERAGE_PAYMENT~., data = ami_train, ntree = 1000)

summary(rf_fit)

print(rf_fit)

print(importance(rf_fit, type = 2))

#ploting 
plot(rf_fit)

#prediction using random forests
randomforest_predic<- predict(rf_fit,newdata = ami_test)

#RSME calculation 
rmse = sqrt(mean((ami_test$TOTAL_AVERAGE_PAYMENT - randomforest_predic)^2))

print(rmse) #this shows the rmse values

#R squared for Randomforest
randomforest_R <- 1 - (sum((ami_test$TOTAL_AVERAGE_PAYMENT- randomforest_predic)^2)/
                  sum((ami_test$TOTAL_AVERAGE_PAYMENT-mean(ami_test$TOTAL_AVERAGE_PAYMENT))^2))

print(randomforest_R)

#ARTIFICIAL NEURAL NETWORKS
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

nn_fit1 <- nnet(ami_train$TOTAL_AVERAGE_PAYMENT~.,data = ami_train, size = 10)

dev.new()

plot.nnet(nn_fit1)  

#calculating the prediction of neural network. 
nnet_predic <- predict(nn_fit1,ami_test)

#RSME Value for neural net
#RMSE Calculation for CART
nnet_rmse = sqrt(mean((ami_test$TOTAL_AVERAGE_PAYMENT -nnet_predic )^2))

print(nnet_rmse)
#R squared value calculation for CART 
nnet_R <- 1 - (sum((ami_test$TOTAL_AVERAGE_PAYMENT- nnet_predic)^2)/
                  sum((ami_test$TOTAL_AVERAGE_PAYMENT-mean(ami_test$TOTAL_AVERAGE_PAYMENT))^2))

print(nnet_R)

#############################################
#####    model analysis of hfdata     #######
#############################################
hist(hfdata$TOTAL_AVERAGE_PAYMENT ,main = "MEDICARE PAYMENTS",
     ylab = "counts",xlab = "Average Medicare Payments ",col = "blue" )

#linear regression of hfdata 
str(hfdata)

hfdata$ZIPCODE <- as.factor(hfdata$ZIPCODE) 
hfdata$DRG.Definition <- as.factor(as.vector(hfdata$DRG.Definition))

hfdata$Estimate..RACE...Black.or.African.American <- as.numeric(hfdata$Estimate..RACE...Black.or.African.American)
hfdata$Estimate..RACE...American.Indian.and.Alaska.Native <- as.numeric(hfdata$Estimate..RACE...American.Indian.and.Alaska.Native)
hfdata$Estimate..RACE...Asian <- as.numeric(hfdata$Estimate..RACE...Asian)
hfdata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander <- as.numeric(hfdata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander)
hfdata$Estimate..RACE...Some.other.race <- as.numeric(hfdata$Estimate..RACE...Some.other.race)
hfdata$Hospital.overall.rating <- as.numeric(hfdata$Hospital.overall.rating)

str(hfdata)
#linear regression is used here for variable selection 
hflmfit <- lm(amidata$TOTAL_AVERAGE_PAYMENT~., data = amidata[,-c(1,3,8)])

summary(hflmfit)

#plots for linear model (lmfit)
#plot residual vs fitted plot 
plot(resid(hflmfit) ~ fitted(hflmfit), xlab = "fitted", ylab = "residual",
     main = "Residual VS fitted  for HF")

#qqplot for AMI  
qqnorm(fitted(hflmfit), ylab = " ")

# based on the results of summary, the potential variables for the analysis
# are used in the 2nd linear regression, the R squared value suggests that
# these are the potential variables for the analysis. 

hflmfit2 <- lm(hfdata$TOTAL_AVERAGE_PAYMENT~ hfdata$DRG.Definition+
               + hfdata$Hospital.Ownership + hfdata$Total.Discharges
             + hfdata$Estimate..HISPANIC.OR.LATINO.AND.RACE...Total.housing.units
             + hfdata$Hospital.overall.rating 
             + hfdata$Estimate..RACE...American.Indian.and.Alaska.Native
             + hfdata$Estimate..RACE...Asian
             + hfdata$Estimate..RACE...Black.or.African.American
             + hfdata$Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander
             + hfdata$Estimate..RACE...Some.other.race
             + hfdata$Estimate..RACE...White
             + hfdata$Estimate..SEX.AND.AGE...10.to.14.years
             + hfdata$Estimate..SEX.AND.AGE...15.to.19.years
             + hfdata$Estimate..SEX.AND.AGE...Under.5.years
             + hfdata$Estimate..SEX.AND.AGE...5.to.9.years
             + hfdata$Estimate..SEX.AND.AGE...20.to.24.years
             + hfdata$Estimate..SEX.AND.AGE...25.to.34.years
             + hfdata$Estimate..SEX.AND.AGE...35.to.44.years
             + hfdata$Estimate..SEX.AND.AGE...45.to.54.years
             + hfdata$Estimate..SEX.AND.AGE...55.to.59.years
             + hfdata$Estimate..SEX.AND.AGE...60.to.64.years
             + hfdata$Estimate..SEX.AND.AGE...65.to.74.years
             + hfdata$Estimate..SEX.AND.AGE...75.to.84.years
             + hfdata$Estimate..SEX.AND.AGE...Female
             + hfdata$Estimate..SEX.AND.AGE...Male,
             data = hfdata )

summary(hflmfit2)

#plot of residul vs fitted 
plot(hflmfit2)

#data for further analysis includes only selected variables from linear regression

hfcols <- list( "TOTAL_AVERAGE_PAYMENT", "DRG.Definition",
                 "Hospital.Ownership ", "Total.Discharges",
                 "Estimate..HISPANIC.OR.LATINO.AND.RACE...Total.housing.units",
                 "Hospital.overall.rating" ,
                 "Estimate..RACE...American.Indian.and.Alaska.Native",
                 "Estimate..RACE...Asian",
                 "Estimate..RACE...Black.or.African.American",
                 "Estimate..RACE...Native.Hawaiian.and.Other.Pacific.Islander",
                 "Estimate..RACE...Some.other.race",
                 "Estimate..RACE...White",
                 "Estimate..SEX.AND.AGE...10.to.14.years",
                 "Estimate..SEX.AND.AGE...15.to.19.years",
                 "Estimate..SEX.AND.AGE...Under.5.years",
                 "Estimate..SEX.AND.AGE...5.to.9.years",
                 "Estimate..SEX.AND.AGE...20.to.24.years",
                 "Estimate..SEX.AND.AGE...25.to.34.years",
                 "Estimate..SEX.AND.AGE...35.to.44.years",
                 "Estimate..SEX.AND.AGE...45.to.54.years",
                 "Estimate..SEX.AND.AGE...55.to.59.years",
                 "Estimate..SEX.AND.AGE...60.to.64.years",
                 "Estimate..SEX.AND.AGE...65.to.74.years",
                 "Estimate..SEX.AND.AGE...75.to.84.years",
                 "Estimate..SEX.AND.AGE...Female",
                 "Estimate..SEX.AND.AGE...Male")

hfdata2 <- hfdata[,colnames(hfdata) %in% unlist(hfcols)]


#Dividing the data into training and testing sets
index <- sample(nrow(hfdata2), nrow(hfdata2)*0.75, rep= F)

set.seed(1203) #generates random numbers 

hf_train <- hfdata2[index,] #training sample 3/4th of the data

hf_test <- hfdata2[-index,] #testing sample 1/4th of the data 

#linear regression analysis 
hfdatalm <- lm(hf_train$TOTAL_AVERAGE_PAYMENT~.,data = hf_train)

summary(hfdatalm) #summary directly gives the R squared value for Linear models

#linear model prediction 
hflm_predic <- predict(hfdatalm,newdata = hf_test)

#RMSE calculation 
hflm_rmse = sqrt(mean((hf_test$TOTAL_AVERAGE_PAYMENT - hflm_predic)^2))

print(lm_rmse)

#CART analysis
hfdata.rpart1 <- rpart(hf_train$TOTAL_AVERAGE_PAYMENT~., data = hf_train
                        ,control = rpart.control(cp=0.01))

dev.new()
rpart.plot(hfdata.rpart1,type = 1, uniform = T) 

plotcp(amidata.rpart1)
printcp(amidata.rpart1)

#pruning the tree
#this function extracts the minimum CP value from the CP table 
hfmincp <- amidata.rpart1$cptable[which.min(amidata.rpart1$cptable[,"xerror"]),"CP"]

hfpfit<- prune(hfdata.rpart1, cp = mincp)

dev.new()
rpart.plot(hfpfit, uniform = T) #; text(pfit,minlength = 2)

#creating prediction using test set
hfrpart_predic <- predict(hfpfit, newdata= hf_test)

#RMSE Calculation for CART
hfrpart_rmse = sqrt(mean((hf_test$TOTAL_AVERAGE_PAYMENT - hfrpart_predic)^2))

print(hfrpart_rmse)
#R squared value calculation for CART 
hfrpart_R <- 1 - (sum((hf_test$TOTAL_AVERAGE_PAYMENT- hfrpart_predic)^2)/
                  sum((hf_test$TOTAL_AVERAGE_PAYMENT-mean(hf_test$TOTAL_AVERAGE_PAYMENT))^2))

print(hfrpart_R)
####Random forest analysis ###### 
### for random forests, I'm using same train and test datasets 
#this is for comparison purposes.

#random forests 
hfrf_fit <- randomForest(hf_train$TOTAL_AVERAGE_PAYMENT~., data = hf_train, ntree = 1000)

summary(hfrf_fit)

print(hfrf_fit)

print(importance(hfrf_fit, type = 2))

#ploting random forest 
plot(hfrf_fit, main = "HF random forest")

#prediction using random forests
hfrf_predic<- predict(hfrf_fit,newdata = hf_test)

#RSME calculation 
hf_rmse = sqrt(mean((hf_test$TOTAL_AVERAGE_PAYMENT - hfrf_predic)^2))

print(hf_rmse) #this shows the rmse values

#R squared for Randomforest
hfrf_R <- 1 - (sum((hf_test$TOTAL_AVERAGE_PAYMENT- hfrf_predic)^2)/
                         sum((hf_test$TOTAL_AVERAGE_PAYMENT-mean(hf_test$TOTAL_AVERAGE_PAYMENT))^2))

print(hfrf_R)

#ARTIFICIAL NEURAL NETWORKS

# we have tried the size of 5 and 10 but there was no change in RMSE or R squared values
hf_nn_fit1 <- nnet(hf_train$TOTAL_AVERAGE_PAYMENT~.,data = hf_train, size = 10)

dev.new()

plot.nnet(hf_nn_fit1)  

#calculating the prediction of neural network. 
hf_nnet_predic <- predict(hf_nn_fit1,hf_test)

#RSME Value for neural net
hf_nnet_rmse = sqrt(mean((hf_test$TOTAL_AVERAGE_PAYMENT - hf_nnet_predic )^2))

print(hf_nnet_rmse)
#R squared value calculation for CART 
hf_nnet_R <- 1 - (sum((hf_test$TOTAL_AVERAGE_PAYMENT- hf_nnet_predic)^2)/
                 sum((hf_test$TOTAL_AVERAGE_PAYMENT-mean(hf_test$TOTAL_AVERAGE_PAYMENT))^2))

#prints the R squared value
print(hf_nnet_R)

### on the whole, random forest algorithm is good at predicting the prices


