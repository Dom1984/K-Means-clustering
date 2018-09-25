

#installing and loading relevant packages:

install.packages('NbClust')
install.packages('scales')
install.packages('ggplot2')

library(dplyr) 
library(tidyr) 
library(ggplot2)
library(magrittr) 
library(corrplot) 
library(purrr) 
library(caret) # 1 stop shop for ML libraries, ML functionalities, and ML controls
library(rpart) # Decision Tree
library(rpart.plot) # Visualize Decision Trees
library(e1071) # SVM and Naive-Bayes
library(class) # K-Nearest-Neighbors
library(randomForest) # Random Forest
library(broom) 
library(stringr)
library(wordcloud)
library(tidytext)
library('cluster')
library('NbClust')
library(dplyr)
library(factoextra)
library(rpart)
library(party)
library(partykit)
library(rattle)

#Read in csv file
beerdata <- read.csv("C:/Users/dcal2/OneDrive/Documents/Data & Web Mining/beer_recipes.csv", header = TRUE)

beer1 <- beerdata
beer1 <- data.frame(beer1) # COnvert to dataframe (if not already)

#View top data head, summary and structure

head(beer1)
summary(beer1)
str(beer1)
names(beer1)
glimpse(beer1)

####General visualisations for our introduction###

#Barchart for Style - Top 10 styles of beer in the data
b2 <- beer1 %>%
  count(Style) %>%
  top_n(10) %>%
  arrange(n, Style) %>%
  mutate(Style = factor(Style, levels = unique(Style)))


beer1 %>%
  filter(Style %in% b2$Style) %>%
  mutate(Style = factor(Style, levels = levels(b2$Style))) %>%
  ggplot(aes(x = Style, fill = factor(Style))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar()


#Barchart for BrewMethod - Top 4 in order

b2 <- beer1 %>%
  count(BrewMethod) %>%
  top_n(4) %>%
  arrange(n, BrewMethod) %>%
  mutate(BrewMethod = factor(BrewMethod, levels = unique(BrewMethod)))


beer1 %>%
  filter(BrewMethod %in% b2$BrewMethod) %>%
  mutate(BrewMethod = factor(BrewMethod, levels = levels(b2$BrewMethod))) %>%
  ggplot(aes(x = BrewMethod, fill = factor(BrewMethod))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar()

nrow(beer.final)



#Change Boilgravity and MashThickness from Factor to a numeric

beer1$MashThickness <- as.numeric(beer1$MashThickness)
beer1$BoilGravity <- as.numeric(beer1$BoilGravity)

#Create smaller data frame with variables to be used in Kmeans analysis

beer2 <- beer1[, c('ABV', 'IBU', 'Color', 'BoilGravity', "MashThickness", 'BoilTime' )]

#Creating a beer3 variable to use after running the algorithm when we are examining clusters, so that we can see styles
beer3 <- beer1[, c('Style', 'StyleID', 'ABV', 'IBU', 'Color', 'BoilGravity', "MashThickness", "Efficiency", 'BoilTime', 'BrewMethod')]

#Check for outliers:
summary(beer2)

#Remove outliers from both dataframes:
beer2 <- subset(beer2, ABV <=20 & IBU <= 1000 & beer2$Color <= 75 & BoilGravity <= 250, BoilTime <= 100)
beer3 <- subset(beer3, ABV <=20 & IBU <= 1000 & beer3$Color <= 75 & BoilGravity <= 250, BoilTime <= 100)


#Scale data
beer.scaled <- scale(beer2)
head(beer.scaled)

#Selecting K - first a Scree Plot

#Scree Plot to see how many clusters we should use

set.seed(123)
k.max <- 15 # Maximal number of clusters


wss <- sapply(1:k.max, 
              function(k){
                kmeans(beer.scaled, k, nstart=15 )$tot.withinss
              })
#Scree plot - 8 clusters is optimal
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 8, lty =2)

#Kmeans Clustering with 8 centers
km.out <- kmeans(beer.scaled, 8, nstart = 50)
#View results
km.out
km.out$tot.withinss
km.out$cluster

# Nice visualisation for k-means clusters

fviz_cluster(km.out, data = beer.scaled, geom = "point",
             stand = FALSE, frame.type = "norm")



#Merge our clusters with our final data, use beer3 variable with 'Style' column

beer.final <- beer3

#Add the clusters number as a column
beer.final <- cbind(beer.final, km.out$cluster)

#Check data
summary(beer.final)
head(beer.final, 50)
str(beer.final)

# Change the cluster name to something sensible - 'Clusters'

colnames(beer.final)<-c('Style', 'StyleID', 'ABV', 'IBU', 'Color', 'BoilGravity', "MashThickness", "Efficiency", 'BoilTime', 'BrewMethod', 'Cluster')

# Extract the cluster groups

cluster1 <- beer.final[which(beer.final$Cluster == 1),]
cluster2 <- beer.final[which(beer.final$Cluster == 2),]
cluster3 <- beer.final[which(beer.final$Cluster == 3),]
cluster4 <- beer.final[which(beer.final$Cluster == 4),]
cluster5 <- beer.final[which(beer.final$Cluster == 5),]
cluster6 <- beer.final[which(beer.final$Cluster == 6),]
cluster7 <- beer.final[which(beer.final$Cluster == 7),]
cluster8 <- beer.final[which(beer.final$Cluster == 8),]


#Create some visualisations to highliught the clusters

# Colour vs Bitterness
ggplot(data = beer.final, mapping = aes(x = Color, y = IBU, colour = 'stress')) + geom_point()
ggplot(data = beer.final, mapping = aes(x = Color, y = IBU, colour = km.out$cluster)) + geom_point()

#Alcohol content vs Bitterness
ggplot(data = beer.final, mapping = aes(x = ABV, y = IBU, colour = 'stress')) + geom_point()
ggplot(data = beer.final, mapping = aes(x = ABV, y = IBU, colour = km.out$cluster)) + geom_point()

#ALcohol vs Colour
ggplot(data = beer.final, mapping = aes(x = Color, y = ABV, colour = 'stress')) + geom_point()
ggplot(data = beer.final, mapping = aes(x = Color, y = ABV, colour = km.out$cluster)) + geom_point()

#Alcohol content and brew Meethod
ggplot(data = beer.final, mapping = aes(x = ABV, y = BrewMethod, colour = km.out$cluster)) + geom_point()

ggplot(data = beer.final, mapping = aes(x = Color, y = BrewMethod, colour = km.out$cluster)) + geom_point()

ggplot(data = beer.final, mapping = aes(x = Color, y = MashThickness, colour = km.out$cluster)) + geom_point()

#We can view our cluster data:

cluster1
cluster2
cluster3
cluster4
cluster5
cluster6
cluster7
cluster8

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)
summary(cluster5)
summary(cluster6)
summary(cluster7)
summary(cluster8)


#Visualisations by Cluster - Plotting ABV against Colour


#Cluster 1
ggplot(data = cluster1, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'blue') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 1", y = "Alcohol by Volume", x = "Colour")

#Cluster 2
ggplot(data = cluster2, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'red') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 2", y = "Alcohol by Volume", x = "Colour")

#Cluster 3
ggplot(data = cluster3, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'green') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 3", y = "Alcohol by Volume", x = "Colour")

#Cluster 4
ggplot(data = cluster4, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'orange')  + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 4", y = "Alcohol by Volume", x = "Colour")

#Cluster 5
ggplot(data = cluster5, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'purple')  + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 5", y = "Alcohol by Volume", x = "Colour")

#Cluster 6
ggplot(data = cluster6, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'maroon') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 6", y = "Alcohol by Volume", x = "Colour")

#Cluster 7
ggplot(data = cluster7, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'yellow') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 7", y = "Alcohol by Volume", x = "Colour")

#Cluster 8
ggplot(data = cluster8, mapping = aes(x = Color, y = ABV)) + geom_point(colour = 'black') + scale_y_continuous(limit = c(0, 20)) + scale_x_continuous(limit = c(0, 50)) + labs(title = "Cluster 8", y = "Alcohol by Volume", x = "Colour")

# Now we will extract random samples from our data


sample_n(cluster2, 25)

sample_n(cluster1, 25)

sample_n(cluster4, 25)

sample_n(cluster6, 25)


###Preparing data for classification

#Create a new version of the data to work on
beerforest <- beer.final

#Ensure Class that we will use, 'Clusters', is categorical
beerforest$Cluster <- as.factor(beerforest$Cluster)

#Rename Clusters into categorical names describing the observations, based on their traits

beerforest$Cluster <- gsub('1', 'Very Strong Bitter', beerforest$Cluster)
beerforest$Cluster <- gsub('2', 'Stout', beerforest$Cluster)
beerforest$Cluster <- gsub('3', 'Medium Ale', beerforest$Cluster)
beerforest$Cluster <- gsub('4', 'Special Ale', beerforest$Cluster)
beerforest$Cluster <- gsub('5', 'Strong Bitter', beerforest$Cluster)
beerforest$Cluster <- gsub('6', 'Light Beer', beerforest$Cluster)
beerforest$Cluster <- gsub('7', 'Extra Strong', beerforest$Cluster)
beerforest$Cluster <- gsub('8', 'Light-to-Medium', beerforest$Cluster)

#Check the data

summary(beerforest)
str(beerforest)
head(beerforest)


########## Classification Trees #############

#Decision Tree
# Fit a classification tree (same data)
# Sample 50% of the data as training data
#Sample 25% of the data as validation 
#Let the remaining 25% data be test data

set.seed(1000)

N <- nrow(beerforest)
indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
indtrain <- sort(indtrain)
indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
indvalid <- sort(indvalid)
indtest <- setdiff(1:N,union(indtrain,indvalid))

#Fit a classifier to only the training data
fit.r1 <- rpart(form, data = beerforest, subset=indtrain)

prp(fit.r1)

# Classify for ALL of the observations
pred.r <- predict(fit.r1,type="class", newdata= beerforest)


# Look at table for the validation data only (rows=truth, cols=prediction)
tab.r <- table(beerforest$Cluster[indvalid],pred.r[indvalid])
tab.r


# Work out the accuracy
acc.r <- sum(diag(tab.r))/sum(tab.r)
acc.r

########## Random Forest #############

# Load the randomForest package
install.packages('randomForest')
library(randomForest)

# Implement the random forest algorithm
fit.rf <- randomForest(form,data=beerforest)

# Examine the results
fit.rf

# Let's look at variable importance
importance(fit.rf)

# Or via this nice plot
varImpPlot(fit.rf)

# Note! Remember MeanDecreaseGini represents the mean decrease in node impurity 
# (and not the mean decrease in accuracy).

# Compare with the tree
pred.rf <- predict(fit.rf,newdata=beerforest,type="class")

table(beerforest$Cluster,pred.r)
table(beerforest$Cluster,pred.rf)


