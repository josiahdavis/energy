# ====================================
# Dimension Reduction of Usage Data
# ====================================

# Clear working space
rm(list = ls()); gc()

library(ggplot2)
library(dplyr)

# ====================================
# READ DATA AND FORMAT
# ====================================

# Read in the data
fileLoc <- "/Users/josiahdavis/Documents/GitHub/energy/"
d <-read.csv(paste(fileLoc, "electricityWide.csv", sep="")) 

# Format the date and respondent fields
d$OUTPUT_DATE <- as.Date(d$OUTPUT_DATE, "%m/%d/%y")
d$respondent <- as.factor(d$respondent)

# ====================================
# CREATE VARIABES OF INTEREST
# ====================================

# Create a total variable
other <- c("respondent", "OUTPUT_DATE", "TYPE", "b")
usageColumns <- names(d)[!(names(d) %in% other)]
d$total <- rowSums(d[,usageColumns])

# Remove unnecessary variable
d <- d[,names(d) != "b"]

# Create time variables
d$year <- as.integer(format(d$OUTPUT_DATE, "%Y"))
d$month <- as.integer(format(d$OUTPUT_DATE, "%m"))
d$season <- ifelse(d$month < 3, "Winter", 
                   ifelse(d$month < 6, "Spring",
                          ifelse(d$month < 9, "Summer",
                                 ifelse(d$month < 12, "Fall", "Winter"))))
d$day <- as.factor(format(d$OUTPUT_DATE, "%a"))
d$weekday <- d$day %in% c("Sat", "Sun")

# Subset for general electricity consumption
ds <- d[(d$TYPE == "general"),]

# Aggregate by respondent
dr <- aggregate(ds[,usageColumns], by=list(OUTPUT_DATE = ds$OUTPUT_DATE), FUN=sum)
dr$day <- as.factor(format(dr$OUTPUT_DATE, "%a"))
dr$weekday <- dr$day %in% c("Sat", "Sun")
dr$total <- rowSums(dr[,usageColumns])

# ====================================
# REDUCE THE DIMENSIONS USING PCA
# ====================================

pca <- prcomp(dr[,usageColumns],
                 center = TRUE,
                 scale = TRUE) 

# Evaluate the variance captured in PCs
plot(pca, type = "l")
summary(pca) 

# Create new datafrome for plotting
td <- as.data.frame(predict(pca, dr[,usageColumns]))
td <- cbind(dr[,c("weekday", "total")], td)
  
# Create plot of usage colored by total usage
ggplot(td, aes(PC1, PC2, color = total)) + 
  geom_point(alpha = 1/4)

# Create plot of usage colored by weekend/weekday
ggplot(td, aes(PC1, PC2, color = weekday)) + 
  geom_point(alpha = 1/4)
