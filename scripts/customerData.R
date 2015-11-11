# Load packages -----------------------------------------------------------
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("xlsx")

loadP <- c("lubridate","dplyr","tidyr","ggplot2","useful","readxl")
lapply(loadP, library, character.only = TRUE)


# Load Sample Data --------------------------------------------------------

# customer infomation sample data
customerData <- read.csv("SGSC-CTCustomer-Household-Data.csv", header = TRUE)

# offer response data
customerOffers <- read.csv("SGSC-CTOffers-and-Acceptances.csv", header = TRUE)

# meter sample
meterSample <- read.csv("smallelectricitytimeofusedataset.csv", header = TRUE, check.names = TRUE, 
                        strip.white = TRUE,stringsAsFactors = FALSE)

# consumption data
consumptionData <- read.csv("data.csv", header = T)
# Clean Sample Data -------------------------------------------------------

# find variables with Y/N

# create lookup table to change variables for clustering
toDummy_Use <- c("Y" = 1, "N" = 0, " " = 0, is.na = 0) # create look up table for variable conversion
toDummy_HH <- c("NA" = 0) # create look up table for variable conversion

hasYN_Use <- which(colnames(customerData) %in% colnames(select(customerData, starts_with("HAS"), 
                                                           starts_with("IS")))) # find Y/N variables

hasNA_HH <- which(colnames(customerData) %in% colnames(select(customerData, starts_with("NUM")))) # find household variable

customerData[hasYN_Use] <- sapply(customerData[hasYN_Use], function(x){ # create function to transform to 
  toDummy_Use[x]
}) 

head(customerData[hasYN_Use]) # check information in usage variables
head(customerData[hasNA_HH]) # check information in household variables 


apply(customerData[34:37],1, sum, na.rm = T)
apply(customerData[34:37],2, summary, na.rm = T)


# Clustering --------------------------------------------------------------


customerCluster <- customerData %>%
  filter(!is.na(NUM_OCCUPANTS), !is.na(NUM_CHILDREN_0_10), !is.na(NUM_CHILDREN_11_17), !is.na(NUM_OCCUPANTS_70PLUS)) %>% 
  filter(NUM_OCCUPANTS !=0) %>% 
  select(starts_with("NUM"),starts_with("HAS"))

summary(customerCluster)
dim(customerCluster)
table(customerCluster$NUM_OCCUPANTS)

# perform k means clustering
customerK_Means4 <- kmeans(scale(customerCluster[c(1:6,8)], center = FALSE), centers = 4, nstart = 50) # 
customerK_Means5 <- kmeans(scale(customerCluster[c(1:6,8)], center = FALSE), centers = 5, nstart = 50) # 
customerK_Means6 <- kmeans(scale(customerCluster[c(1:6,8)], center = FALSE), centers = 6, nstart = 50) # 
customerK_Means7 <- kmeans(scale(customerCluster[c(1:6,8)], center = FALSE), centers = 7, nstart = 50) # 
customerK_Means8 <- kmeans(scale(customerCluster[c(1:6,8)], center = FALSE), centers = 8, nstart = 50) # 

customerK_Means4$centers #54
customerK_Means5$centers #66.6
customerK_Means6$centers #70
customerK_Means7$centers #73
customerK_Means8$centers #74

# create data frame
clusterDataC <- data.frame(customerK_Means8$centers, row.names = NULL)
clusterDataC$cluster <- c(1:NROW(customerK_Means8$centers))

clusterDF <- gather(data = clusterDataC, key = variable, value = value, -cluster)

ggplot(data = clusterDF) +
  geom_bar(aes(x = factor(cluster), y = value, fill = factor(cluster)), stat = "identity") +
  scale_fill_discrete("Cluster Number") +
  xlab("cluster numbers") +
  # facet_grid(cluster~variable)
  facet_wrap(~variable) +
  coord_flip()

head(clusterDF)
ggplot(data = clusterData) +
  geom_bar(aes(x = factor(cluster), y = NUM_CHILDREN_0_10, fill = factor(cluster)), stat = "identity") +
  scale_fill_discrete("Cluster Number") +
  ggtitle("number of children 0 - 10") +
  xlab("cluster numbers")

ggplot(data = clusterData) +
  geom_bar(aes(x = factor(cluster), y = NUM_ROOMS_HEATED, fill = factor(cluster)), stat = "identity") +
  scale_fill_discrete("Cluster Number") +
  ggtitle("number of rooms heated") +
  xlab("cluster numbers")
  
ggplot(data = clusterData) +
  geom_bar(aes(x = factor(cluster), y = NUM_OCCUPANTS_70PLUS, fill = factor(cluster)), stat = "identity") +
  scale_fill_discrete("Cluster Number") +
  xlab("cluster numbers") +
  ggtitle("number of 70+")


# Graph based on k-means
library(cluster)
clusplot(customerCluster,  # data frame
         customerK_Means4$cluster,  # cluster data
         color = TRUE,  # color
         #          shade = TRUE,  # Lines in clusters
         lines = 3,  # Lines connecting centroids
         labels = 2)  # Labels clusters and cases

# first plots 
plot.kmeans(customerK_Means4, data = customerCluster)

library(useful)

plot.kmeans(wineK3, data = wineTrain)
pg <- plot.kmeans(wineK3, data = wine, class = "Cultivar", shape = cluster)
pg + xlab("Stuff")
pg  + ggtitle("Age and Height of Schoolchildren")
?plot.kmeans


write.csv(clusterDataC,file = "meansClusters.csv", row.names = F)
