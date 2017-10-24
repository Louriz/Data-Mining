#======================== TP1: F3B Data Mining=======================================
#=======================Riahi LOURIZ =======================================
#Q.1:  Here we have supervised learning ( we have a target column (tip value). This
##     dependent variable depends on time, day, total bill, ....


#Q.2 : #- What factors can most influence the tip value ?
       #- Which factors matter when managers assign tables to food servers ?
       #- We can create from the data a classification problem: will the customer give or not the tip ( by creating a new column  basing on the tip column ( if tip<.. ==> no otheriwise ==> yes )
  
# Q.3: load data from the file

tips <- read.csv("tips.csv", header=TRUE, sep=",")

# to know the class of the object we are working on
class(tips)
# :give a description about our dataframe 
# we have 244 obs with 7 variables (2 numerical, 4 factors and 1 integer)
str(tips)
# to have a statistical descriptive about the tips dataset:
summary(tips)
# Q.4 :create a new variable names tip_rate 
tips$tip_rate<-(tips$tip)/(tips$total_bill)
# show tip_rate values
tips$tip_rate
# show the first 6 rows
head(tips)

#Q.5:  univariate summaries :some statistics about each numerical variable,
#... for factor we have the occurence of each modality
summary(tips)

#Q.6: plot the distribution of day:
barplot(table(tips$day))

#Q.7 : plot tip against the total_bill
plot(tips$total_bill,tips$tip,xlab="total_bill",ylab="tip")
# test of correlation:
cor.test(tips$tip,tips$total_bill,method = c("pearson"))
### result is : corr=0.67  and p_value<5% with a significant result===> they are correlated
#Q.8 : boxplot
## for tip
boxplot(tips$tip,data=tips, main="Distribution of tips",ylab="Tip values")
## for total_bill
boxplot(tips$total_bill,data=tips, main="Distribution of total_bill",ylab="total_bill values")

## tip V.S day :
boxplot(tips$tip~tips$day,data=tips, main="Distribution of tips vs days",ylab="tip values", xlab="Days")


#Q.9 :
hist(tips$total_bill)
hist(tips$tip, col="light blue")
## We will perform a statistic test to see if our variables are normal or not :

#### for tip :
shapiro.test(tips$tip)
###====> not normal
#### for total_bill:
shapiro.test(tips$total_bill)
###========> not normal

#### let s do some transformation with log ( because the variable is positif)
shapiro.test(log(tips$total_bill))
##====> here the sample distribution is normal because the p_value is >0.05 so we 
###### we take the null hypotheses
## to make sure with a plot:
hist(log(tips$total_bill))
#
### this restaurant is not expensive, because the tips values are not very important 
par(mfrow=c(2,3))
hist(tips$tip, breaks=7, col='light blue')
hist(tips$tip, breaks=9, col='light blue')
hist(tips$tip, breaks=11, col='light blue')
hist(tips$tip, breaks=13, col='light blue')
hist(tips$tip, breaks=15, col='light blue')
hist(tips$tip, breaks=20, col='light blue')
par(op) 


#Q.10 :
## proportion for smoker 
barplot(prop.table(table(tips$smoker)),main="Smoker proportion",xlab="smoke or not")
## proportion for sex
barplot(prop.table(table(tips$sex)),main="Sex proportion",xlab="Gender")
## Proportion of time and 
count1 <- table(tips$time, tips$day)
barplot(count1, main="proportion of time(dinner/lunch) by day", xlab="day", col=c("darkblue","red"),legend = rownames(count1))
## Proprtion of sex payer and smoking parties
count2 <- table(tips$sex, tips$smoker)
count2
barplot(count2, main="proportion of gender and smoking parties", xlab="smoking parties", col=c("darkblue","red"),legend = rownames(count2))
Q.12
library(vcd)
data(tips)
mosaicplot(~tips$day+tips$sex , data = tips, color = TRUE)
### Answer : Men pay mostly on Sunday.
===================Q.13 : Regression=============================
## create dummies :
library(dummies)
#dummies<- table(1:length(tips$day),as.factor(tips$day))  
#data_new<-data.frame(tips,dummies)
tips <- cbind(tips, dummy(tips$day, sep="_"))
tips <- cbind(tips, dummy(tips$sex, sep="_"))
tips <- cbind(tips, dummy(tips$time, sep="_"))
tips <- cbind(tips, dummy(tips$smoker, sep="_Smoke"))

col=c("tips_Fri","tips_Sat","tips_Sun","tips_Thur","tips_Female","tips_Male","tips_Dinner","tips_Lunch","tips_SmokeNo","tips_SmokeYes")
tips[col] <- lapply(tips[col], factor)




#Q.14: linear model

## with all variables
fit <- lm(tips$tip_rate ~ tips$tips_Female+tips$tips_Male+tips$tips_SmokeNo+tips$tips_SmokeYes+tips$tips_Dinner+tips$tips_Lunch+tips$size+tips$tips_Fri+tips$tips_Sat+tips$tips_Sun+tips$tips_Thur, data=tips)
summary(fit) # show results
coefficients(fit) # model coefficient
#Q.15 :with the size only
fit_size <- lm(tips$tip_rate ~ tips$size,data=tips)
summary(fit_size)# show result
anova(fit,fit_size)
# Q.16 :
library(MASS)
fit_AIC <- lm(tips$tip_rate ~ tips$tips_Female+tips$tips_Male+tips$tips_SmokeNo+tips$tips_SmokeYes+tips$tips_Dinner+tips$tips_Lunch+tips$size+tips$tips_Fri+tips$tips_Sat+tips$tips_Sun+tips$tips_Thur, data=tips)
step <- stepAIC(fit_AIC, direction="both")
step$anova # display results


#Q.17 :
fit_total_bill=lm(tips$tip_rate~tips$total_bill,data=tips)
summary(fit_total_bill)
##
fit_size=lm(tips$tip_rate~tips$sex,data=tips)
summary(fit_size)

#Q.18: optional:
## correlation between total_bill and size
cor.test(tips$size,tips$total_bill,method = c("pearson"))
#====> yes here we have correlation.

fit_correlation=lm(tips$tip_rate~tips$total_bill+tips$size,data=tips)
summary(fit_correlation)
### here we see the problem of correlation between two variables. Due to the correlation 
## between size and total bill, the regression model does not take anymore size as 
## intersting variable
## so solve this problem, we will add a another term to our formula:

fit_correlation_2=lm(tips$tip_rate~tips$total_bill+tips$size+(tips$total_bill)*(tips$size),data=tips)
summary(fit_correlation_2)
#### Conclusion:
##




## the link for more information about how to interpret results for lm:
###  https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
