## Setup Setting Directory

setwd("C:/Users/pc/Desktop/Capston Project/Equiskill Project")

## Imprting Data into R

Deepak <- read.csv("Default_On_Payment.csv")

## Exploring Data 

dim(Deepak)
names(Deepak)
head(Deepak)
tail(Deepak)
summary(Deepak)
str(Deepak)

## To Check the Data, whether we have to fix or not
summary(as.factor(Deepak$Default_On_Payment))

## Creating the summary of the Data

summary_deepak = summary(Deepak)

## Importing the file in the working Directory

write.csv(summary_deepak,"summary_deepak.csv", row.names = F)

## Removing the row which has the missing value

Deepak <- Deepak[complete.cases(Deepak),]

## To Check again for the missing value again in the modified data
summary(as.factor(Deepak$Default_On_Payment))

## Removing Costumer_ID Column 
Deepak <- Deepak[,c(-1)]

##Subset the data. Subset into 60% and 40%

d60percent <- Deepak[sample(nrow(Deepak), replace= F, size=0.60*nrow(Deepak)),]

write.csv(d60percent,"d60percent.csv", row.names = F)

## Bivariate Analysis

library(gmodels)

# Tabulating Between Dependent and Each Independent Variables

CrossTable(Deepak$Status_Checking_Acc,Deepak$Default_On_Payment,expected=FALSE, prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Duration_in_Months,Deepak$Default_On_Payment,expected = FALSE, prop.r = FALSE, prop.c=FALSE,prop.t = FALSE,Prop.chisq = FALSE,chisq=FALSE)

CrossTable(Deepak$Credit_History,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Purposre_Credit_Taken,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Job,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

##After Tabulation between Dependent variable and Independent Variable.  
##We won't able to find significant cause. It may lead bias in modelling


library(gmodels)

# Tabulating Between Dependent and Each Independent Variables

CrossTable(Deepak$Status_Checking_Acc,Deepak$Default_On_Payment,expected=FALSE, prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Duration_in_Months,Deepak$Default_On_Payment,expected = FALSE, prop.r = FALSE, prop.c=FALSE,prop.t = FALSE,Prop.chisq = FALSE,chisq=FALSE)

CrossTable(Deepak$Credit_History,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Purposre_Credit_Taken,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

CrossTable(Deepak$Job,Deepak$Default_On_Payment,expected = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = FALSE)

##After Tabulation between Dependent variable and Independent Variable. We wont able 
##We won't able to find significant cause. It may lead bias in modelling

## We need important metrics to run Logistic Regression. That will 
## help you to find significant variables and for fitting the model

library(Information)
library(riv)
library(devtools)
library(woe)
library(gridExtra)

## Ignore this line As I was trying to Create Infotable. But failed to create
create_infotables(data = Deepak, valid = FALSE, y=,bins = 10, trt = False,ncore = FALSE,parallel = TRUE)


## Generating IV(Information Value for each variable to select the Significant variables.)

stat <- create_infotables(data=Deepak,y="Default_On_Payment")
grid.table(stat$Summary,row=NULL)
write.csv(stat$Summary,"IV-Summary1.csv")

#As Checked value for variables. Subset the data and selected only some significant variables

newdata <- subset(Deepak,select = c(Status_Checking_Acc, Duration_in_Months,Credit_History, Savings_Acc,Purposre_Credit_Taken,Age,Property,Default_On_Payment)) 

## Generate WOE (Weight of Evidence) table for each independent Factor

stat <- create_infotables(data=newdata, y= "Default_On_Payment")

grid.table(stat$Tables$Status_Checking_Acc,row=NULL)

grid.table(stat$Tables$Duration_in_Months,row=NULL)

grid.table(stat$Tables$Credit_History,row=NULL)

grid.table(stat$Tables$Purposre_Credit_Taken,row=NULL)

grid.table(stat$Tables$Age,row=NULL)

grid.table(stat$Tables$Property,row=NULL)

## Transformmation/Dummy coding of variables for getting the best model

write.csv(stat$Tables$Duration_in_Months,"Duration_summary.csv",row.names=F)

newdata$Duration_Monthwise <- ifelse(newdata$Duration_in_Months %in% c("4","5","6", "7", "8", "9", "10", "11","12", "13", "14", "15", "16", "17", "18"),"lessthan20", ifelse(newdata$Duration_in_Months %in% c("20","21","22", "24", "26", "27", "28", "30","33", "36", "39"), "20to40", "morethan40"))

table(newdata$Duration_in_Months,newdata$Duration_Monthwise)

stat <- create_infotables(data=newdata, y = "Default_On_Payment")

newdata$Duration_Dummy <- ifelse(newdata$Duration_Monthwise == "lessthan20",1, ifelse(newdata$Duration_Monthwise == "20to40",2,3))

newdata$Account_Status <- ifelse(newdata$Status_Checking_Acc == "A11",1, ifelse(newdata$Status_Checking_Acc == "A12",2, ifelse(newdata$Status_Checking_Acc == "A13",3,4)))

newdata$Cedit_History_Dummy <- ifelse(newdata$Credit_History == "A30",0,ifelse(newdata$Credit_History == "A31",1,ifelse(newdata$Credit_History == "A32",2,ifelse(newdata$Credit_History == "A33",3,4))))

newdata$Purpose_for_Credit <- ifelse(newdata$Purposre_Credit_Taken == "A40",0,ifelse(newdata$Purposre_Credit_Taken == "A41",1,ifelse(newdata$Purposre_Credit_Taken=="A42",2,ifelse(newdata$Purposre_Credit_Taken=="A43",3,ifelse(newdata$Purposre_Credit_Taken=="A44",4,
                                                                                                                                                                                                                                 ifelse(newdata$Purposre_Credit_Taken=="A45",5,
                                                                                                                                                                                                                                        ifelse(newdata$Purposre_Credit_Taken=="A46",6,
                                                                                                                                                                                                                                               ifelse(newdata$Purposre_Credit_Taken=="A47",7,
                                                                                                                                                                                                                                                      ifelse(newdata$Purposre_Credit_Taken=="A48",8,
                                                                                                                                                                                                                                                             ifelse(newdata$Purposre_Credit_Taken=="A49",9,10))))))))))

newdata$Savings_Acc_Dummy <- ifelse(newdata$Savings_Acc == "A61",0,ifelse(newdata$Savings_Acc == "A62",1,ifelse(newdata$Savings_Acc == "A63",2,ifelse(newdata$Savings_Acc == "A64",3,4))))


newdata$Property_Dummy <- ifelse(newdata$Property == "A121",1,ifelse(newdata$Property == "A122",2,ifelse(newdata$Property == "A123",3,4)))

newdata$Default_On_Payment1 <- as.factor(ifelse(newdata$Default_On_Payment == 1,"1","0"))

## Build logistic regression

library(ROCR)

logreg <- glm(Default_On_Payment ~ Duration_Dummy+Account_Status+Cedit_History_Dummy+Purpose_for_Credit+Savings_Acc_Dummy+Property_Dummy+Age, family = binomial("logit"),data = newdata)

summary(logreg)

plot(predict(logreg,type = "response"))


newdata$predicted = predict(logreg,type = "response")
write.csv(newdata,"Output.csv",row.names = F)

capture.output(summary(logreg), file = "summary_logreg1.csv")
summary_residualsofmodel.csv <- residuals(logreg,type = "deviance")
write.csv(summary_residualsofmodel.csv,"summary_resdidualsofmodel.csv")


## Diagnostic the Model


## RocR

newdata$predicted = predict(logreg,type = "response")
Pred1 <- prediction(newdata$predicted,newdata$Default_On_Payment)
Perf2 <- performance(Pred1,"tpr","fpr")
plot(Perf2)
abline(a=0,b=1,col="Red")

#AUC

auc.perform = performance(Pred1, measure = "auc")
auc.perform@y.values


# Auc is 0.7744103, which is better than 0.50. It means its good model

# Creating Decile

library(dplyr)

newdata$deciles <- ntile(-newdata$predicted,10)
write.csv(newdata,"newdata.csv")

## KS

max(attr(Perf2,'y.values')[[1]]-attr(Perf2,'x.values')[[1]])

#KS is 0.4608992, which is better than 20%. It shows its good model

#LIFT

lift_object <- performance(Pred1,"lift",x.measure = "rpp")
plot(lift_object,meain="Lift Chart",xlab= "%populatio",ylab="Lift",col="blue")
abline(1,0,col="red")

## Lorenz and Gini

library(ineq)

## Gini Index= 0.3863895

ineq(newdata$predicted,type = "Gini")

## Lorenz Curve

plot(Lc(newdata$predicted),col="darkgreen",lwd=2)

