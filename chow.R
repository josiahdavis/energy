# Clear working space
rm(list = ls())
gc()

# Import packages
library(dplyr)
library(ggplot2)

# Read in the data
fileLoc <- "C:/Users/josiahd/Documents/PGE/energy/"
d <-read.csv(paste(fileLoc, "data.csv", sep="")) 
d <- d[,c("CUSTOMER_KEY", "month_Year", "general_KWH", "max_Monthly", "min_Monthly")]
names(d) <- c("customer", "time", "general", "max", "min")


# Format variables and subset time interval
d$time <- as.Date(paste0("01-", d$time), "%d-%m-%Y")
d <- filter(d, time >= as.Date("2012-05-01"), 
                  time < as.Date("2014-03-01"))

str(d)

# Plot usage across time
ggplot(d, aes(time, general)) +
  geom_point(alpha = 1/2) +
  geom_smooth() +
  scale_size_area() + 
  scale_x_date(breaks = seq.Date(min(d$time), max(d$time), "quarter"))

# Plot usage against min temperature
ggplot(d, aes(min, general)) + 
  geom_point(alpha = 1/2) + 
  geom_smooth()

# Plot usage against max temperature
ggplot(d, aes(max, general)) + 
  geom_point(alpha = 1/2) + 
  geom_smooth()


# Choose a breaking point for the chow test (arbitrary for now)
breakPoint <- as.Date("2013-09-01")

## Create a linear regression
mc = lm(gen ~ month, data = dm)
m1 = lm(gen ~ month, data = dm[dm$month <= breakPoint,])
m2 = lm(gen ~ month, data = dm[dm$month > breakPoint,])

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
chow > Fcrit 
1 - pf(chow, k, N1 + N2 - 2*k)

# Visualize lines of best fit
ggplot(dm, aes(month, gen)) +
  geom_point(alpha = 1/4) +
  scale_size_area() + 
  stat_smooth(method="lm", se=FALSE, color = "#1f77b4") + 
  stat_smooth(data = dm[dm$month <= breakPoint,], method="lm", se=TRUE, color = "#ff7f0e") + 
  stat_smooth(data = dm[dm$month > breakPoint,], method="lm", se=TRUE, color = "#2ca02c") + 
  scale_x_date(breaks = seq.Date(min(dm$month), max(dm$month), "quarter")) + 
  scale_y_continuous(limits = c(-0, 1500))