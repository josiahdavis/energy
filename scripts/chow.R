# ===================================
# READ IN DATA AND FORMAT
# ===================================

# Clear working space
rm(list = ls())
gc()

# Import packages
library(dplyr)
library(ggplot2)

# Read in the data
fileLoc <- "/Users/josiahdavis/Documents/GitHub/energy/"
d <-read.csv(paste(fileLoc, "dataDaily.csv", sep="")) 
d <- d[,c("CUSTOMER_KEY", "yearMonthDay", "general_KWH", "hdd", "cdd")]
names(d) <- c("customer", "time", "general", "hdd", "cdd")


# Format variables and subset time interval
d$time <- as.Date(d$time, "%Y-%m-%d")
d <- filter(d, time >= as.Date("2012-05-01"), 
                  time < as.Date("2014-03-01"))

str(d)

# ===================================
# EVALUATE USAGE AND HDD/CDD
# ===================================

# Evaluate  usage against cooling degree days (cdd)
ggplot(d[d$cdd > 0,], aes(cdd, general)) + 
  geom_point(alpha = 1/4) + 
  geom_smooth(method="lm") + 
  ggtitle("Relationship between Electricity Usage and Cooling Degree Days")  + 
  scale_x_continuous(name="Cooling Degree Days") + 
  scale_y_continuous(name="Electricity Usage")
summary(lm(general ~ cdd, d))

# Evaluate usage against heating degree days
ggplot(d[(d$hdd > 0) & (d$hdd < 2.5),], aes(hdd, general)) + 
  geom_point(alpha = 1/4) + 
  geom_smooth(method="lm") + 
  ggtitle("Relationship between Electricity Usage and Heating Degree Days") + 
  scale_x_continuous(name="Heating Degree Days") + 
  scale_y_continuous(name="Electricity Usage")
summary(lm(general ~ hdd, d))

# Plot usage across time
ggplot(d, aes(time, general)) +
  geom_point(alpha = 1/4) +
  geom_smooth(method="lm") +
  scale_size_area() + 
  scale_x_date(breaks = seq.Date(min(d$time), max(d$time), "quarter")) + 
  ggtitle("Electricity Usage Over Time") 
summary(lm(general ~ time, d))

# ===================================
# EVALUATE STRUCTURAL BREAK
# ===================================

# Choose a breaking point for the chow test (arbitrary for now)
breakPoint <- as.Date("2013-09-01")

## Create a linear regression
mc = lm(general ~ time, data = d)
m1 = lm(general ~ time, data = d[d$time <= breakPoint,])
m2 = lm(general ~ time, data = d[d$time > breakPoint,])

# Calculate the chow test statistic
# Reference: https://en.wikipedia.org/wiki/Chow_test
SC <- sum(mc$residuals^2)
S1 <- sum(m1$residuals^2)
S2 <- sum(m2$residuals^2)
k <- mc$rank
N1 <- length(m1$residuals)
N2 <- length(m2$residuals)
chow <- ((SC - (S1 + S2)) / k) / ((S1 + S2) / (N1 + N2 - 2 * k))
chow

# Check for significance
Fcrit <- qf(.99, k, N1 + N2 - 2*k)
Fcrit
chow > Fcrit 
1 - pf(chow, k, N1 + N2 - 2*k)

# Visualize lines of best fit
ggplot(d, aes(time, general)) +
  geom_point(alpha = 1/10) +
  scale_size_area() + 
  stat_smooth(method="lm", se=FALSE, color = "#1f77b4", size = 1.1) + 
  stat_smooth(data = d[d$time <= breakPoint,], method="lm", size = 1.1, se=TRUE, color = "#ff7f0e") + 
  stat_smooth(data = d[d$time > breakPoint,], method="lm", size = 1.1, se=TRUE, color = "#2ca02c") + 
  scale_x_date(name = "Time", breaks = seq.Date(min(d$time), max(d$time), "quarter")) + 
  scale_y_continuous(name="Electricity Usage", limits = c(0, 70))
