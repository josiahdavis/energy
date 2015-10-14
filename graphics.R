# ===================================
# BASIC EXPLORATORY VISUALIZATIONS 
# ===================================

# Clear working space
rm(list = ls()); gc()

# Import packages
library(dplyr)
library(ggplot2)

# Read in the data 
fileLoc <- "/Users/josiahdavis/Documents/GitHub/energy/"
d <-read.csv(paste(fileLoc, "smallelectricitytimeofusedataset.csv", sep="")) 
d <- d[,1:3]
names(d) <- c("customer", "datetime", "usage")

# Format variables and add new variables
d$datetime <- strptime(d$datetime, "%m/%d/%y %H:%M")
d$customer <- as.factor(d$customer)
d$hour <- as.integer(format(d$datetime, "%H"))
d$day <- as.factor(format(d$datetime, "%a"))

# Inspect
head(d)
str(d)

# ===================================
# How do people typically use 
# electricity throughout the week?
# ===================================

# Create the aggregation for visualization
dw <- d %>%
  select(hour, day, usage) %>%
  group_by(hour, day) %>%
  summarize(
    usage = median(usage)
  )

# Chart the visualization
ggplot(dw, aes(hour, usage, color = day)) + 
  geom_line(size = 1.1) + 
  ggtitle("General Hourly Electricity Usage Pattern") + 
  scale_y_continuous(name="Average Electricity Usage", breaks=seq(0, 0.4, 0.1)) + 
  scale_x_continuous(name="Time of Day", breaks = seq(0, 24, 5))

# ===================================
# How do individual's vary in their 
# usage throughout the day?
# ===================================

# Create the aggregation for visualization
di <- d %>%
  select(hour, customer, usage) %>%
  group_by(hour, customer) %>%
  summarize(
    usage = median(usage)
  )

# Compute the volatility metric
di2 <- di %>%
  select(hour, customer, usage) %>%
  group_by(customer) %>%
  summarize(
    volatility = sd(usage)
  )
di <- left_join(di, di2, by = "customer")

# Sort by the volatility metric
di <- arrange(di, desc(volatility))
di$customer <- factor(unique(di$customer), levels = unique(di$customer))
rm(di2)

# Chart the visualization
ggplot(di, aes(hour, usage)) + 
  geom_area(fill = "steelblue") + 
  facet_grid(customer ~ .) + 
  ggtitle("Individual Variation in Electricity Usage") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        strip.text.y = element_blank(), strip.background = element_blank()) +
  scale_y_continuous(name="Average Electricity Usage", breaks=NULL) + 
  scale_x_continuous(name="Time of Day", breaks = seq(1, 24, 2))