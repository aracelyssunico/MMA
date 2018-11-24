# MMA
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
pacman::p_load('cluster', 'hclust','mclust', 'e1071', 'fpc', 'dplyr', 'readxl', 'sqldf', 'tidyr', 'purrr', 'ggplot2')

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

DOS2data <- read_excel(file.choose())
DOS2data <- read.csv(file.choose())
str(DOS2data)
summary(DOS2data)
sum(is.na(DOS2data))

# Let's change those ugly column names...
names(DOS2data)[7:36] <- c("GrocerySurvey", "LowPrices", "Freshness", "ChoiceVariety", "Healthiness", "OrganicAlternatives", 
                           "ConvenientStoreLayout", "StoreLocation", "ProductQuality", "ServiceQuality", "ReturnPolicy", 
                           "Cleanliness", "Busyness", "ActivePerson", "InActivePerson", "PhysicallyFit", "EatHealthy", 
                           "HealthyPerson", "EatPoorly", "ProportionFoodThatAreFruitsAndVegetables", "HoursOfExerciseLastWeek", 
                           "NeighbourhoodClass", "Gender", "Age", "FamilySize", "MaritalStatus", "Occupation", 
                           "OccupationDescription", "HighestDegree", "HouseholdAnnualIncome")

DOS2data <- subset(DOS2data, select = -c(OccupationDescription))

#Store Attributes 

loblaw_raw_data_base <- sqldf("select LowPrices, Freshness, ChoiceVariety, Healthiness, OrganicAlternatives, ConvenientStoreLayout, StoreLocation, ProductQuality, ServiceQuality, ReturnPolicy, Cleanliness, Busyness from DOS2data")


### The reasoning for the level compression is two fold ###
# 1. Avoid 0 in the level transformation to keep consistent with all the other categorical variables (they all start with 1)
# 2. Only use numbers here rather than factors because we trying to do k-mean clustering

#Data CLeaning 

# lowerEducation = 1, higherEducation = 2
DOS2data$HighestDegree <- ifelse(DOS2data$HighestDegree == 2, 1, 2)

# student = 1, non-student = 2
DOS2data$Occupation <- ifelse(DOS2data$Occupation == 5, 1, 2)

# couple = 1, single = 2
DOS2data$MaritalStatus <- ifelse(DOS2data$MaritalStatus %in% c(1,2), 1, 2)

#First Attempt to CLuster only with store attributes 

### attempted clustering part, does not make much nse to me, probably doing something wrong here ###
wss<-0
wss <- (nrow(loblaw_raw_data_base) - 1) * sum(apply(loblaw_raw_data_base, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(loblaw_raw_data_base, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "within groups sum of squares", main= "Within cluster sum of squares error (WSS) Plot")

set.seed(20190510)
loblaw_raw_data_base_kmeans <- kmeans(loblaw_raw_data_base, centers = 4
                                      )

# inspect it
seg.summ(loblaw_raw_data_base, loblaw_raw_data_base_kmeans$cluster)

clusplot(loblaw_raw_data_base, loblaw_raw_data_base_kmeans$cluster, color = T, shade = T, labels = 9, lines = 0, main = "K-means cluster plot")



# Feature Engineering
DOS2data$StoreValue   <- (DOS2data$LowPrices + DOS2data$Freshness + DOS2data$ChoiceVariety + 
                          DOS2data$StoreLocation + DOS2data$ProductQuality + DOS2data$ReturnPolicy)/6

DOS2data$StoreComfort <- (DOS2data$ServiceQuality + DOS2data$Cleanliness + DOS2data$Busyness + 
                         DOS2data$ConvenientStoreLayout)/4
  
DOS2data$StoreHealth  <- (DOS2data$Healthiness + DOS2data$OrganicAlternatives)/2

summary(DOS2data)


### attempted clustering part 2
wss <- 0

for (i in 1:20) wss[i] <- sum(kmeans(scale(DOS2data[, 35:37]), centers = i)$withinss)
plot(1:20, wss, type = "b", xlab = "Number of Clusters", ylab = "within groups sum of squares", main= "Within cluster sum of squares error (WSS) Plot")

set.seed(20190510)
DOS2data_kmeans <- kmeans(scale(DOS2data[, c(35:37)]), centers = 3, nstart = 25)

#determine the percentage of the population that falls in each cluster 
population <- DOS2data_kmeans$cluster
table(population)

# inspect it
segmentation <- seg.summ(DOS2data[, 8:38], DOS2data_kmeans$cluster)
segmentation


#split groups into separate data frames for analysis 
group_1 <- segmentation %>% filter(Group.1 ==1)
group_2 <- segmentation %>% filter(Group.1 ==2)
group_3 <- segmentation %>% filter(Group.1 ==3)

#evaluate store attributes to determine what customers need in a store (clumns 2-13)
group_1_store <- sort(colMeans((group_1[,2:13])))
group_1_store


group_2_store <- sort(colMeans((group_2[,2:13])))
group_2_store

group_3_store <- sort(colMeans((group_3[,2:13])))
group_3_store


#evaluate customer lifestyle's based on columns (14-21)
group_1_lifestyle <- sort(colMeans((group_1[,14:21])))
group_1_lifestyle

group_2_lifestyle <- sort(colMeans((group_2[,14:21])))
group_2_lifestyle

group_3_lifestyle <- sort(colMeans((group_3[,14:21])))
group_3_lifestyle


#evaluate customer demographic information based on columns (22-29)
group_1_demo <- sort(colMeans((group_1[,22:29])))
group_1_demo

group_2_demo <- sort(colMeans((group_2[,22:29])))
group_2_demo

group_3_demo <- sort(colMeans((group_3[,22:29])))
group_3_demo


#plot clusters
clusplot(scale(DOS2data[, c(35:37)]), DOS2data_kmeans$cluster, 
               color = T, shade = T, labels = 9, lines = 0, main = "K-means cluster plot")

