#===================== Association Rules=====================================
######################Riahi LOURIZ, F3B Data Science ###########################
### this code is inspired from  links below
### http://www.salemmarafi.com/code/market-basket-analysis-with-r/
### http://mhahsler.github.io/arulesViz/reference/plot.html 
### https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
###https://fr.slideshare.net/rdatamining/association-rule-mining-with-r
#================Load the libraries========================================
library(arules)
library(arulesViz)
library(datasets)
#=====================================================================================

#============================== Data Presentation ==================================
#==============================Question 1: Load and describe data===================
#load the data set
data(Groceries)
# the class of data:
class(Groceries)  # Groceries is a "transactions" dataset.
# summary of data:
summary(Groceries) 
## Comment: we got 9835 rows( which is the number of transactions) and 169 columns(
##which is the number of items)
## to make it clear: the data look like the table below:
##======================================================================================
## TransID#   items purchased
##====================================================================================
##    1    #     A  B C D G 
##    2    #     B  A C H
##    3    #     C F 
##    4    #     A B 
##    5    #     A D G 
##    6    #     A B C 
##    7    #     C H G 
## ...................................................................................
## SO basically, each transaction has a purchased items. The absence of an item means it was not purchased
## in that transaction.

# Create an item fequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute") # whole milk is the most frequent item


#============================== Generation of frequent itemset========================
#============================Question 2, 3===============================================

### RECALL :==========================================================================
### given the Rule  { X ==> Y }:   support= frequency(X,Y)/N     N : total number of transactions
###                                confidence =Frequency (X,Y)/Frequency(X)
###                                lift= support/(support(X)*support(Y))
###===================================================================================  
rules = apriori(Groceries, parameter = list(minlen=1, sup = 0.02,maxlen=1))
itemsets <- unique(generatingItemsets(rules))
itemsets.df <- as(itemsets, "data.frame")
frequentItemsets <- itemsets.df[with(itemsets.df, order(-support,items)),]
names(frequentItemsets)[1] <- "itemset"
head(frequentItemsets,10)

# Get the rules
rules<-apriori(Groceries,parameter=list(supp=0.001,conf=0.8,maxlen=3))
# show the top 10 rules, but only 2 digits
options(digits=2)
inspect(rules[0:10])
# summary about rules:
summary(rules)   # maxlen=3    =====> 29  rules generated

#=============================== Question 4 ====================================
rules <- apriori(Groceries, parameter = list(supp=0.02,minlen=4,maxlen=4))
itemsets <- unique(generatingItemsets(rules))
itemsets.df <- as(itemsets, "data.frame")
frequentItemsets <- itemsets.df[with(itemsets.df, order(-support,items)),]
names(frequentItemsets)[1] <- "itemset"
str(frequentItemsets)
head(frequentItemsets,10)
### no change in result !!!!!

#==================================Question 5===================================
rules <- apriori(Groceries, parameter = list(supp=0.01))
itemsets <- unique(generatingItemsets(rules))
itemsets.df <- as(itemsets, "data.frame")
frequentItemsets <- itemsets.df[with(itemsets.df, order(-support,items)),]
names(frequentItemsets)[1] <- "itemset"
str(frequentItemsets)
head(frequentItemsets,10)
###

#===================== Rule generation and visualization 
#============================= Question 6========================================

rules<-apriori(Groceries,parameter=list(supp=0.002,conf=0.8))
# show the top 10 rules, but only 2 digits
options(digits=2)
inspect(rules[0:10])
# summary about rules:
summary(rules)   # supp=0.001 and conf=0.6   =====> 2918 rules generated  ( support=0.001)
			# supp=0.001 and conf=0.8 =====> we got 410 rules
			# supp=0.002 and conf=0.8  ====> 11 rules generated
### we can say those paramter are imporant to choose the most valuable rules which depends
### on the economy context and nature of the algorithm being used
### recall: the confidence measure how likely if a customer buy item X, he will buy item Y
### example: if the confidence of  { X--->Y} is 0.9: it means we are sure 90% that
### if the customer buy X, he will buy also Y 
### so defining the threshold is so important to context.

#===============Question 7=============================================================
rules<-apriori(Groceries,parameter=list(supp=0.001,conf=0.6)) # will give 2918 rules
plot(rules) #scatterplot for rules
plot(rules@quality)
# interactive plot
sel <- plot(rules, interactive=TRUE)
## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control = list(main = "Two-key plot",col=rainbow(max(size(rules))-1L)))


#=================================Question8====================================
rules<-apriori(Groceries,parameter=list(supp=0.001,conf=0.6)) # will give 2918 rules

rules<-sort(rules, by="lift", decreasing=TRUE)   # we got 2918 rules
inspect(rules[1:5])
### the rule with highest lift is {Instant food products,soda}=>{hamburger meat}
#rules_conf<-sort(rules[quality(rules)$confidence >= 0.9],by="lift",decreasing=TRUE)
subrules<-subset(rules,confidence>=0.9)  # another way
#rules_conf      # 129
subrules
#============================== Question 9===============================================

plot(subrules, method="matrix", measure=c("lift", "confidence"))


#============================= Question 10===================================

plot(subrules[1:5],method="graph",interactive=TRUE,shading=NA)

inspect(subrules[1:5])










# Redundancies:
inspect(rules_conf[1:5])
#===========================================================
#Sometimes, rules will repeat. Redundancy indicates that
# one item might be a given. As an analyst you can elect
# to drop the item from the dataset. Alternatively, 
#you can remove redundant rules generated.
#We can eliminate these repeated rules using the follow 
#snippet of code:
#=======================================================
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
summary(rules)  # we get 0 rules 


#  Targeting items :
#======================================================================================================================================================================
#Now that we know how to generate rules, limit the output, lets say we wanted to target items to generate rules.
# There are two types of targets we might be interested in that are illustrated with an example of “whole milk”:
######What are customers likely to buy before buying whole milk
######What are customers likely to buy if they purchase whole milk?
#This essentially means we want to set either the Left Hand Side and Right Hand Side. This is not difficult to do with R!
#Answering the first question we adjust our apriori() function as follows:
#=====================================================================================================================================================================
## ths first Question
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
## the seconde question:
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])



# visualization:
#====================================================================================
#The last step is visualization. Lets say you wanted to map out the rules 
#in a graph. We can do that with another library called “arulesViz”.
#=====================================================================library(arulesViz)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 1))
plot(rules[1:10],method="graph",interactive=TRUE,shading=NA)
