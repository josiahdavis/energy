# Clear working space
rm(list = ls())
gc()

# Import packages
library(dplyr)
library(ggplot2)

# Read in the data
fileLoc <- "C:/Users/josiahd/Documents/PGE/energy/"
fileLoc <- "/Users/josiahdavis/Documents/GitHub/energy/"
d <-read.csv(paste(fileLoc, "data.csv", sep="")) 
d <- d[,c("CUSTOMER_KEY", "month_Year", "general_KWH", "hdd", "cdd")]
names(d) <- c("customer", "time", "general", "hdd", "cdd")


# Format variables and subset time interval
d$time <- as.Date(paste0("01-", d$time), "%d-%m-%Y")
d <- filter(d, time >= as.Date("2012-05-01"), 
                  time < as.Date("2014-03-01"))

str(d)

# Plot usage across time
ggplot(d, aes(time, general)) +
  geom_point(alpha = 1/4) +
  geom_smooth(method="lm") +
  scale_size_area() + 
  scale_x_date(breaks = seq.Date(min(d$time), max(d$time), "quarter"))
 
# Evaluate  usage against cooling degree days (cdd)
ggplot(d, aes(cdd, general)) + 
  geom_point(alpha = 1/4) + 
  geom_smooth(method="lm")

# Evaluate usage against heating degree days
ggplot(d, aes(hdd, general)) + 
  geom_point(alpha = 1/4) + 
  geom_smooth(method="lm")


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
  geom_point(alpha = 1/4) +
  scale_size_area() + 
  stat_smooth(method="lm", se=FALSE, color = "#1f77b4") + 
  stat_smooth(data = d[d$time <= breakPoint,], method="lm", se=TRUE, color = "#ff7f0e") + 
  stat_smooth(data = d[d$time > breakPoint,], method="lm", se=TRUE, color = "#2ca02c") + 
  scale_x_date(breaks = seq.Date(min(d$time), max(d$time), "quarter")) + 
  scale_y_continuous(limits = c(-0, 1500))
