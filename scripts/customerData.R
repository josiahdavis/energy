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
table(customerData[2])
colnames(customerData)

# remove retail customers
# customerData <- customerData[customerData$TRIAL_CUSTOMER_TYPE!= "Retail",]

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


# check for zero and null values
apply(customerData[hasNA_HH], 2, summary, na.rm = TRUE) # apply functions across
apply(customerData[hasNA_HH], 2, mean, na.rm = TRUE) # apply functions across
sapply(customerData[hasNA_HH], sum, na.rm = TRUE) # apply functions across
apply(customerData[hasYN_Use], 2, summary, na.rm = TRUE) # apply functions across

# assign household size to missing variables 
customerData$NUM_OCCUPANTS[is.na(customerData$NUM_OCCUPANTS)] <- 
  as.integer(runif(NROW(customerData$NUM_OCCUPANTS[is.na(customerData$NUM_OCCUPANTS)]),mean(customerData$NUM_OCCUPANTS, na.rm = TRUE), max(customerData$NUM_OCCUPANTS, na.rm = TRUE)))
customerData$NUM_OCCUPANTS[customerData$NUM_OCCUPANTS==0] <- 
  as.integer(runif(NROW(customerData$NUM_OCCUPANTS[customerData$NUM_OCCUPANTS==0]),mean(customerData$NUM_OCCUPANTS, na.rm = TRUE), max(customerData$NUM_OCCUPANTS, na.rm = TRUE)))
customerData$NUM_OCCUPANTS[customerData$NUM_OCCUPANTS==0] <- 
  as.integer(runif(NROW(customerData$NUM_OCCUPANTS[customerData$NUM_OCCUPANTS==0]),mean(customerData$NUM_OCCUPANTS, na.rm = TRUE), max(customerData$NUM_OCCUPANTS, na.rm = TRUE)))


# assign # refrigerators size to missing variables 
# customerData$NUM_REFRIGERATORS[customerData$NUM_REFRIGERATORS==0] <- 1
  # as.integer(runif(NROW(customerData$NUM_REFRIGERATORS[customerData$NUM_REFRIGERATORS==0]),median(customerData$NUM_REFRIGERATORS, na.rm = TRUE), max(customerData$NUM_REFRIGERATORS, na.rm = TRUE)))
customerData$NUM_REFRIGERATORS[is.na(customerData$NUM_REFRIGERATORS)] <- 1
  # as.integer(runif(NROW(customerData$NUM_REFRIGERATORS[is.na(customerData$NUM_REFRIGERATORS)],median(customerData$NUM_REFRIGERATORS, na.rm = TRUE), max(customerData$NUM_REFRIGERATORS, na.rm = TRUE))))
summary(customerData$NUM_REFRIGERATORS)

# assign # num_rooms_heated
# remove NA values
customerData[hasNA_HH][is.na(customerData[hasNA_HH])] <- 0
customerData$NUM_ROOMS_HEATED[customerData$NUM_ROOMS_HEATED==0] <- as.integer(runif(NROW(customerData$NUM_ROOMS_HEATED[customerData$NUM_ROOMS_HEATED==0]),median(customerData$NUM_ROOMS_HEATED, na.rm = TRUE),max(customerData$NUM_ROOMS_HEATED, na.rm = TRUE)))

summary(customerData[hasNA_HH])
summary(customerData[hasYN_Use])
summary(customerData)

# create tag for sample data
inSample <- unique(customerData[customerData$CUSTOMER_KEY %in% unique(meterSample$CUSTOMER_KEY),1])
# create factor variable for sample data
customerData$inSample <- as.factor(ifelse(customerData$CUSTOMER_KEY %in% inSample, "SampleData","NoSample"))
# table(customerData$inSample)


# Clustering --------------------------------------------------------------

# create subset of numeric values
customerCluster <- customerData %>%
  filter(!is.na(NUM_OCCUPANTS)) %>% 
  select(starts_with("NUM"))

customerCluster <- na.omit(customerCluster)

summary(customerCluster)
customerCluster$NUM_OCCUPANTS <- sapply(customerCluster$NUM_OCCUPANTS, as.numeric) # change to numeric
customerCluster <- sapply(customerCluster, as.numeric)

# perform k means clustering
customerK_Means4 <- kmeans(scale(customerCluster, center = FALSE), centers = 4, nstart = 50) # 77.7
customerK_Means4
customerK_Means5 <- kmeans(customerCluster, centers = 5, nstart = 50) # 80.3
customerK_Means6 <- kmeans(customerCluster, centers = 6, nstart = 50) # 82.9
customerK_Means7 <- kmeans(customerCluster, centers = 7, nstart = 50) # 85
customerK_Means8 <- kmeans(customerCluster, centers = 8, nstart = 50) # 86.7
?scale
# customerCluster$cluster <- customerK_Means$cluster # assign clusters to customer data
# customerCluster$CUSTOMER_KEY <- customerData$CUSTOMER_KEY[]
# head(customerCluster)


clusterData <- data.frame(customerK_Means4$centers[,1:6], row.names = NULL)
clusterData$cluster <- c(1:4)

ggplot(data = clusterData) +
  geom_bar(aes(x = factor(cluster), y = NUM_OCCUPANTS, fill = factor(cluster)), stat = "identity") +
  scale_fill_discrete("Cluster Number") +
  xlab("cluster numbers") +
  ggtitle("number of occupants")

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
