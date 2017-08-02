################ TP.2 : Clustering ####################################
################### Riahi LOURIZ FIG-3A###############################



#=================================================================================
# In this Lab we are going to highlight the notion of clustering in Machine Learning.
# Clustering is an unsupervised Learning used to find groups with same similarities (clusters)
# Key points about Clustering: 
##1. Before applying a clustering algorithm to your data, use the Hopkins statistic algorithm to make sure that your data is clusterable( not uniforme distributed) 
##2. Find the optimal K ( number of clusters)
##3. How lo label your clusters
##4.  do not forget to scale your data, to make sure that all features contribute to the distance equally



#===================================================================================
#============== Useful commands in R============================
##1. Load all the data available in R:  library(MASS)
##2. see all data available in R ( after loading of course): data()
##3. load a specific data : ( e.g: data(Nile)  will laod the Nile datasets into memory, to see the contents: Nile ))
#==============================================================================




#================================ Answers=========================================
#=========================Q1: Loading and describing data =============================
## ruspini data set( online) ( if not provided)
#ruspini<-fread('https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/cluster/ruspini.csv')
#ruspini[,V1:=NULL] # to delete the first variable V1( useless)
## ruspini.csv
ruspini=read.csv('ruspini.csv') 
ruspini$X<-NULL  # remove X( useless)
head(ruspini)
## food.csv
food<-read.csv('food.csv',sep=';') # use sep=';' because food is a ; separated data values
head(food)
## describe each dataset:
str( ruspini) # run this line to see a full description about the data( e.g: variables, length, ..)

str(food)
## boxplots: 
#### if you still do not know about boxplot and how to figure out the outliers try to do the steps in boxplot.txt
boxplot(ruspini$x,data=ruspini, main="Distribution of the first coordinate")
boxplot(ruspini$y,data=ruspini, main="Distribution of the seconde coordinate")
## no outliers for x and y
boxplot(food$Iron,data=food, main="Distribution of Iron")
## yes, Iron presents some outliers
boxplot(food$Energy,data=food, main="Distribution of Energy")
## energy : No
boxplot(food$Protein,data=food, main="Distribution of Protein")
##Protein: Yes
boxplot(food$Calcium,data=food, main="Distribution of calcium")
##Calcium: Yes 
boxplot(food$Fat,data=food, main="Distribution of Fat")
## Fat: No









#======================= Q.2: Plot Ruspini data in 2D =====================
plot(ruspini$x,ruspini$y)
## we can expect that there is  4 clusters 




#=======================Q.3: K-means  algorithm======================
##before applying K-means, let's see the hopkins statistic
library(clustertend)
hopkins(ruspini, n = nrow(ruspini)-1)






###================= Hopkins statistics==================================
#If the value of Hopkins statistic is close to zero, then we can reject  # 
#the null hypothesis and conclude that the dataset D is significantly    # 
#a clusterable data.                                                     #
###=======================================================================
### we get a value of: 0.22, so according to the rule above, we can confirm again 
### that our data is clusterable
library(factoextra)
set.seed(2)  # to fix the probel of having different result after each execution
km.res1 <- kmeans(ruspini, 4,iter.max=10)
fviz_cluster(list(data = ruspini, cluster = km.res1$cluster),frame.type = "norm", geom = "point", stand = FALSE)
## as we run many times, each time we get different clusters ( using the same number of clusters 4)






#==========================Q.4:fix issue in Q.3=================================
set.seed(2)  # to fix the probel of having different result after each execution
km.res1 <- kmeans(ruspini, 4)
fviz_cluster(list(data = ruspini, cluster = km.res1$cluster),frame.type = "norm", geom = "point", stand = FALSE)







#===========================Q.5 : optimal number of clusters K==================
library(fpc)
library("cluster")
fviz_silhouette(km.res1)
## below a function that plot the silhouette index against the initial number of clusters
silhouette_function <- function(data,alg){
plot.new()
op <- par(mfrow= c(3,3), oma= c(0,0, 3, 0),mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))
for (k in 2:8)
	plot(silhouette(alg(scale(data),k)$cluster, dist(data)),main=" ")
mtext("Silhouette plot",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()

}
silhouette_function(ruspini,kmeans) # call the function with the argument data=ruspini to plot 
### conclusion: the best number of clusters is 4, because it coorresponds to the best silhouette width
## other method: elbow method:

elbow_function<-function(data,alg){     # alg is the algorithme which will be used (kmeans,..)
k.max <- 15 # Maximal number of clusters

wss <- sapply(1:k.max, 
        function(k){alg(scale(data), k )$tot.withinss})
plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares") 
}

elbow_function(ruspini,kmeans)
# the elbow method confirms that k=4 is an optimal number for clusters.





#========================= Q.6 : PAM ==========================================
silhouette_function(ruspini,pam)
## k=4 is always the best number of clusters.

# choose the appropriate algorithm for our data :

library(clValid)
clmethods <- c("kmeans","pam")
intern <- clValid(scale(ruspini), nClust = 2:8,
              clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
## we get kmeans as the best one !

fit1<-pam(scale(ruspini),4)
attributes(fit1) # to see attributes of fit1



#========================= Q.7:  
head(food)
## we can delete the feature name
food$Name<-NULL
head(food) # make sure that it is dropped
food.scaled<-scale(food) # normalize data
head(food.scaled) # have a look at the new values of attributes

##hopkins statistic:

hopkins(food, n = nrow(food)-1) # gives 0.20 (  if we run many times we get diff results !, take the average )
### so we can say that our data is clusterable

## let's apply kmeans and pam on this data sets
silhouette_function(food.scaled,pam)
silhouette_function(food.scaled,kmeans)
elbow_function(food.scaled,kmeans)

library(clValid)
clmethods <- c("kmeans","pam")
intern <- clValid(scale(food), nClust = 2:8,
              clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
## for the silhouette criterion, we have k=4 for kmeans as the best score.




#=========================== Q.8: hierarchical clustering ==========================
#The result of hierarchical clustering is a tree-based representation of the 
#observations which is called a dendrogram. Observations can be subdivided 
#into groups by cutting the dendogram at a desired similarity level.
# visit for more info: http://www.sthda.com/english/wiki/cluster-analysis-in-r-unsupervised-machine-learning#hierarchical-clustering
#===============================================================================
## install required packages:
install.packages("cluster")
install.packages("dendextend")
## load packages:
library(cluster)
library(dendextend)
library(factoextra)
## let's have a look at a  statistical descriptive of our features:
### this function was taken from the same source as above )
desc_stats <- data.frame(
  Min = apply(food, 2, min), # minimum
  Med = apply(food, 2, median), # median
  Mean = apply(food, 2, mean), # mean
  SD = apply(food, 2, sd), # Standard deviation
  Max = apply(food, 2, max) # Maximum
  )
desc_stats <- round(desc_stats, 1)
head(desc_stats)
###Note that the variables have a large different means and variances. 
###This is explained by the fact that the variables are measured in different units
### some are in grams , other milligrams, ...
### that's why we should normalize our data:
food.scaled<-scale(food)
###
# Dissimilarity matrix

d <- dist(food.scaled, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Plot the obtained dendrogram
plot(res.hc, cex = 0.6, hang = -1)

### comments:================================================================
#### the plot contains 27 leaves (which is exactly the number of observation)
#### more than that we notice that those observation are grouped into clusters with same
#### similarities.
#### from source given above: In the dendrogram displayed above, each leaf corresponds 
####to one observation. As we move up the tree, observations that are similar 
####to each other are combined into branches, which are themselves fused at 
###a higher height.The height of the fusion, provided on the vertical axis,
#### indicates the (dis)similarity between two observations. 
####The higher the height of the fusion, the less similar the observations are.
#==============================================================================
 
#======================Q.9:==================================
###using the manhattan distance

d2 <- dist(food.scaled, method = "manhattan")
# Hierarchical clustering using Ward's method
res.hc2 <- hclust(d2, method = "ward.D2" )
# Plot the obtained dendrogram
plot(res.hc2, cex = 0.6, hang = -1)
### we get different groups
### use the agnes method
library("cluster")
# Compute agnes()
res.agnes.average <- agnes(food.scaled, method = "average")
res.agnes.ward <- agnes(food.scaled, method = "ward")

# Agglomerative coefficient
par(mfrow = c(1,2))
# Plot the tree using pltree()
pltree(res.agnes.average, cex = 0.6, hang = -1,main = "Dendrogram of agnes") 
pltree(res.agnes.ward, cex = 0.6, hang = -1,main = "Dendrogram of agnes") 
## we can see the difference between the two methods in terms of distributing




#========================Q.10: cut dendrogram================================
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Number of members in each cluster
table(grp)

plot(res.hc, cex = 0.6)
rect.hclust(res.hc, k = 4, border = 2:5)


######## Nice plot to compare dendrograms=======================



library(dendextend)
# Compute distance matrix
res.dist <- dist(food.scaled, method = "euclidean")
# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Create a list of dendrograms
dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2)
### quality of alignement
tanglegram(dend1, dend2,
  highlight_distinct_edges = FALSE, # Turn-off dashed lines
  common_subtrees_color_lines = FALSE, # Turn-off line colors
  common_subtrees_color_branches = TRUE, # Color common branches 
  main = paste("entanglement =", round(entanglement(dend_list), 2))
  )
### avalue of 0.16====>  good alignment because near to 0

