
# Load packages -----------------------------------------------------------
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("xlsx")

loadP <- c("lubridate","dplyr","tidyr","ggplot2","xlsx")
lapply(loadP, library, character.only = TRUE)

# Load Sample Data --------------------------------------------------------
# meter sample data
meterSample <- read.csv("smallelectricitytimeofusedataset.csv", header = TRUE, check.names = TRUE, 
                        strip.white = TRUE,stringsAsFactors = FALSE)
# head(meterSample)
# sapply(meterSample, class)
# head(meterSample)
# meterTest <- meterSample[1:20,]

# customer infomation sample data
customerData <- read.csv("../../Dropbox/DropData/PowerData/SGSC-CTCustomer-Household-Data.csv", header = TRUE)
# str(customerData)
# head(customerData)
# summary(customerData$POSTCODE)
# length(table(as.factor(customerData$POSTCODE)))
# length(table(customerData$SUBURB_NAME))

# offer response data
customerOffers <- read.csv("../../Dropbox/DropData/PowerData/SGSC-CTOffers-and-Acceptances.csv", header = TRUE)

# daily meter reading
dailyReads <- read.csv("../../Dropbox/DropData/PowerData/energyWide.csv", header = TRUE)
summary(dailyReads)

# load historic weather data Sydney (Observatory Hill)
# 66062 Lat: 33.86° SLon: 151.21° EElevation: 39 m

tempHistoricMax <- read.csv("../../Dropbox/DropData/PowerData/IDCJAC0010_066062_1800/IDCJAC0010_066062_1800_Data.csv")
tempHistoricMin <- read.csv("../../Dropbox/DropData/PowerData/IDCJAC0011_066062_1800/IDCJAC0011_066062_1800_Data.csv")


# Clean Sample Data -------------------------------------------------------

# meter sample data
str(meterSample)
summary(meterSample)
meterSample$FullTime <- strptime(meterSample$End.Datetime, format = "%d/%m/%Y %H:%M")
as.Date(meterSample$End.Datetime, format = "%d/%m/%Y")
meterSample$Year <- year(meterSample$FullTime)
meterSample$Year <- ifelse(nchar(meterSample$Year) == 2, paste("20",year(meterSample$FullTime), sep=""),year(meterSample$FullTime))
meterSample$Month <- month(meterSample$FullTime)
meterSample$Day <- day(meterSample$FullTime)

# change CUSTOMER_ID to factor
meterSample$CUSTOMER_KEY <- factor(meterSample$CUSTOMER_KEY)

meterGroup <- meterSample %>% 
  select(-FullTime) %>% 
  group_by(CUSTOMER_KEY, Year, Month) %>% 
  select(CUSTOMER_KEY, Month, General.Supply.KWH:Net.Generation.KWH) %>% 
  summarise(n = n())

list(unique(meterSample$CUSTOMER_KEY)) # find unique customer keys
summary(as.factor(meterSample$Year))

# customer infomation sample data
head(customerData)
# list(unique(customerData$CUSTOMER_KEY))
customerDataSub <- customerData[customerData$CUSTOMER_KEY %in% meterSample$CUSTOMER_KEY,] #subset on customers in meter samples

customerDataSubRank  <-  customerDataSub %>% 
  group_by(TRIAL_REGION_NAME) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# list(customerDataSubRank$TRIAL_REGION_NAME)

# customerDataSubRank  <-  customerDataSub %>% 
#   group_by(POSTCODE) %>% 
#   summarize(postCount = n()) %>% 
#   arrange(desc(postCount))
# head(customerDataSub)

# summary(customerDataSub)

# formula that should be an anonymous function
tempConvert <- function(x){
  x <- round(x*(9/5) + 32,digits = 0)
  x
}




# Combine Data Sets -------------------------------------------------------


# sydney observatory data (min and max temperature)
tempHistoricSub <- tempHistoricMax %>% 
  left_join(tempHistoricMin,by = c("Year","Month","Day")) %>% # join max and min on ymd
  filter(Year >= 2012) %>% # years greater than 2012
  rename(MaxTemp_C = Maximum.temperature..Degree.C.,MinTemp_C = Minimum.temperature..Degree.C.) %>% # shorten name
  select(stationNumber = Bureau.of.Meteorology.station.number.x, # select grouping columns
         Month, Year,MaxTemp_C, MinTemp_C) %>% 
  mutate(MaxTempF = tempConvert(MaxTemp_C),
         MinTempF = tempConvert(MinTemp_C)) %>% 
  mutate(month_Year = paste(Month, Year, sep = "-")) %>% # create new variables
  group_by(month_Year) %>% # group by year and month year dummy
  summarise(max_Monthly = max(MaxTempF),
            min_Monthly = min(MinTempF),
            median_High = median(MaxTempF),
            median_Low = round(median(MinTempF),0),
            measured_Days = n(),
            days_80 = sum(MaxTempF > 80), # days max temp was above 80
            days_65 = sum(MinTempF < 65)) %>% # days max temp was below 65
  arrange(month_Year)


  # cooling degree days
  # if max temp for month is less than base then cooling degrees == 0
  # if max temp > base and minimum temp <= base
  
tempHistoricSub # view data frame

# list(unique(meterSample$CUSTOMER_KEY)) # 30 unique households
# dim(meterSample[meterSample$CUSTOMER_KEY == "8170837",] ) # 15977 rows
# dim(meterSample) #618189 rows

# summarize sample meter and combine with sydney observatory data 
meterSampleSub <- meterSample %>% 
  select(CUSTOMER_KEY, Month, Year, General.Supply.KWH:Net.Generation.KWH) %>% 
  mutate(month_Year = paste(Month, Year, sep = "-")) %>% # create new variables
  group_by(CUSTOMER_KEY, Year, month_Year) %>% 
  summarize(general_KWH = sum(General.Supply.KWH),
            offPeak_KWH = sum(Off.Peak.KWH),
            grossGen_KWH = sum(Gross.Generation.KWH),
            netGen_KWH = sum(Net.Generation.KWH),
            total_KWH = general_KWH+offPeak_KWH+grossGen_KWH+netGen_KWH) %>% 
  left_join(tempHistoricSub, by = "month_Year")# join with temperature data



# Export to CSV -----------------------------------------------------------

write.csv(x = meterSampleSub, file = "../../Dropbox/Utility/meterSample_temp.csv",row.names = FALSE)
list.files()
getwd()
