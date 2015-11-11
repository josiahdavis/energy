###### Testing the assumptions of linear regression


# Set directory and load packages ----------------------------
# setwd("./scripts/")
# install.packages("car")
# install.packages("lmtest")
library(lmtest)
library(car)
library(ggplot2)

# Generate Random Data ----------------------------------------------------

days <- 10; hours <- 24; people <- 4

d <- data.frame(
  usage_hour = rep(floor(seq(0,23.9, by = 1 / days)), people),
  usage_date = rep(seq(as.Date("2014/1/1"), as.Date("2014/1/10"), "days"), hours * people), 
  sp_id = sort(rep(sample(1000000:2000000, people), hours * days)),
  usage = abs(rnorm(days * hours * people, mean = 1, sd = 1)),
  mean_temp = abs(rnorm(days * hours * people, mean = 65, sd = 10))
)

# split into training and test data

idx <- sample(1:nrow(d), 600, replace = FALSE, prob = NULL)
d_train <- d[idx,]
d_test <- d[-idx,]
# dim(d_train)
# dim(d_test)
rm(idx, d, days, hours, people) # remove variables
# head(d_train)

# Create Linear Regression ------------------------------------------------

m <- lm(usage ~ mean_temp, d_train)
d_test$p <- predict(m, d_test)
d_test$res <- (d_test$usage - d_test$p)


# TESTING ASSUMPTIONS -----------------------------------------------------
# (1) LINEARITY: random pattern suggests that a linear model is appropriate
# (2) INDEPENDENCE OF ERRORS: A pattern that is not random suggests lack of independence.
# (3) HOMOSCEDASTICITY: residuals should vary randomly around 
# zero and the spread of the residuals should be about the same throughout the plot 
# (4) NORMALITY OF ERRORS: This would show up as a funnel or megaphone shape to the residual plo


# OBSERVED VS PREDICTED VALUES
obsVSpred <- ggplot(data = d_test) +
  geom_point(aes(x = p, y = usage)) +
  # stat_smooth(aes(x = p, y = usage), method = "loess") +
  stat_smooth(aes(x = p, y = usage), method = "loess") +
  xlim(c(min(d_test$p),max(d_test$p))) +
  ggtitle("Observed vs. Expected Values") +
  labs(x = "Expected Values", y = "Actual Values")

# vertical and horizontal for average of median actual and expected


# RESIDUALS DISTRIBUTION
resDensity <- ggplot(data = d_test)+
#   geom_density(aes(x = res, y =..density.., fill = "Actual"), alpha = .3) +
#   geom_density(aes(x = rnorm(NROW(d_test), 0, 1 ),
#                    y = ..density.., fill = "expected"), alpha = .3) +
  stat_function(aes(color = "expected"), fun = dnorm, color = "blue") +
  xlim(-3,3) +
  ggtitle("Distribution of Residuals: Actual vs. Expected") +
  labs(x = "Residuals", y = "Density") +
  labs(fill = "Residual Type") +
  theme(legend.position=c(1,1), legend.justification=c(1,1))


# RESIDUALS VS. EXPECTED VALUES
resVSexp <- ggplot(data = d_test) +
  geom_jitter(aes(x = p, y = res)) +
  geom_hline(yintercept = 0, size = 1.5, color = "red", linetype = "dashed") +
  stat_smooth(aes(x = p, y = res), method = "lm", se = FALSE) +
  ggtitle("Residuals vs. Expected Values") +
  labs(x = "Expected Value", y = "Residual")


# RESIDUALS VS. DATE
# A pattern that is not random suggests lack of independence.
resVSdate <- ggplot(data = d_test) +
  geom_boxplot(aes(factor(usage_date) , y = res)) +
  geom_hline(yintercept = 0, size = 1.5, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs. Usage Date") +
  labs(x = "Usage Date", y = "Residual")


resVdateJitter <- ggplot(d_test, aes(usage_date, res)) + 
  geom_jitter() +
  scale_x_date(breaks = seq.Date(min(d_test$usage_date), max(d_test$usage_date),"day")) +
  geom_hline(yintercept = 0, size = 1.5, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs. Usage Date") +
  labs(x = "Usage Date", y = "Residual") +
  geom_smooth(method = "lm", aes(usage_date, res))
  


# RESIDUALS VS. TIME OF DAY
# A pattern that is not random suggests lack of independence.
resVShour <- ggplot(d_test, aes(usage_hour, res)) + 
  geom_boxplot(aes(factor(usage_hour) , y = res)) +
  geom_hline(yintercept = 0, size = 1.5, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs. Usage Hour") +
  labs(x = "Usage Hour", y = "Residual")


resVShourJitter <- ggplot(d_test, aes(usage_hour, res)) + 
  geom_jitter() +
  geom_hline(yintercept = 0, size = 1.5, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs. Usage Hour") +
  labs(x = "Usage Hour", y = "Residual") +
  geom_smooth(method = "lm", aes(usage_hour, res))


# RESIDUALS AUTOCORRELATION
# Durbin Watson Autocorrelation Test
# null hypothesis that the errors are serially uncorrelated against 
# the alternative that they follow a first order autoregressive process

durbTest <- durbinWatsonTest(m)

# The Breusch–Godfrey serial correlation LM test 
# The null hypothesis is that there is no serial correlation of any order up to p.[4]
# https://en.wikipedia.org/wiki/Breusch–Godfrey_test
bgResults <- bgtest(m)

durbTest$p; bgResults$p.value
# LM test = 1.9849, df = 1, p-value = 0.1589
