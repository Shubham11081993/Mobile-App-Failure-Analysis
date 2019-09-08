# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table) 
library(vtreat) 
library(scales)
library(magrittr)

setwd("C:/Users/anukr/Desktop/capstone")


#### DATA IMPORT, WRANGLING & LOOKUP ####

# Import Apple Device lookup data from URL
AppleDevices <- read.csv("original_AppleDevices.csv")

# Wrangle and structure imported Apple Device lookup data 
names(AppleDevices)[1] <- "DEVICE_MODEL"
names(AppleDevices)[2] <- "PRODUCT_NAME"

AppleDevices <- AppleDevices %>% 
  separate_('PRODUCT_NAME', into = c('PRODUCT_NAME','PRODUCT_MODEL_INFO'), sep = '\\(')
AppleDevices$PRODUCT_MODEL_INFO <- gsub('\\)','', AppleDevices$PRODUCT_MODEL_INFO)


# Import raw login data file 
Login <- read.csv("original_login_data.csv")


# Correct column names for accuracy and consistency, format timestamp and created Date, Hour, Weekday and Week Number
names(Login)[1] <- "Timestamp"
names(Login)[4] <- "DEVICE"
names(Login)[8] <- "RESULT"
names(Login)[9] <- "Volume"
Login$RESULT <- as.character(Login$RESULT)

Login$Timestamp <- format(Login$Timestamp, tz="America/New_York",usetz=TRUE)
Login$Date <- as.Date(Login$Timestamp,  tz="America/New_York")
Login$Hour <- strftime(Login$Timestamp, tz="America/New_York", "%H")
Login$Weekday <- weekdays(as.Date(Login$Timestamp))
Login$Weekday <- format(Login$Weekday, tz="America/New_York",usetz=FALSE)
Login$WeekNumber <- strftime(Login$Timestamp, format = "%U")
Login$WeekNumber <- format(Login$WeekNumber, tz="America/New_York",usetz=FALSE)

# merging device info then append device group names
Login <- full_join(Login, AppleDevices, by = "DEVICE_MODEL")
Login$DEVICE_GROUP <- Login$PRODUCT_NAME
Login <- Login %>% mutate(DEVICE_GROUP = sub('^iPhone.*', 'iPhone', DEVICE_GROUP)) %>%
  mutate(DEVICE_GROUP = sub('^iPod.*', 'iPod Touch', DEVICE_GROUP)) %>%
  mutate(DEVICE_GROUP = sub('^iPad.*', 'iPad', DEVICE_GROUP)) %>%
  filter(!is.na(DEVICE_GROUP)) %>%
  filter(!is.na(Timestamp))


# Measures to validate Lookup functionality
lookup <- Login %>%
  group_by(DEVICE_GROUP, PRODUCT_NAME, DEVICE_MODEL, DEVICE) %>%
  summarise(Total = sum(Volume))

# Checking the structure and statistical summary 
str(Login)
summary(Login)

# Review total volume by Device (Device group)
total_VOLUME_by_device_group <- Login %>% 
  filter(!is.na(RESULT)) %>%
  group_by(DEVICE_GROUP, RESULT) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total)
total_VOLUME_by_device_group$SUCCESS [is.na(total_VOLUME_by_device_group$SUCCESS)] <- 0
total_VOLUME_by_device_group$POLICY [is.na(total_VOLUME_by_device_group$POLICY)] <- 0
total_VOLUME_by_device_group$DEFECT [is.na(total_VOLUME_by_device_group$DEFECT)] <- 0
total_VOLUME_by_device_group <- mutate(total_VOLUME_by_device_group, TOTAL = (SUCCESS + POLICY + DEFECT)) 
ggplot(total_VOLUME_by_device_group, aes(x = DEVICE_GROUP, y = TOTAL))+
  geom_col()
print(total_VOLUME_by_device_group)

# Review total volume by Device (Sub group)
total_VOLUME_by_device_subgroup <- Login %>% 
  group_by(DEVICE_GROUP, PRODUCT_NAME, RESULT) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total)
total_VOLUME_by_device_subgroup$SUCCESS [is.na(total_VOLUME_by_device_subgroup$SUCCESS)] <- 0
total_VOLUME_by_device_subgroup$POLICY [is.na(total_VOLUME_by_device_subgroup$POLICY)] <- 0
total_VOLUME_by_device_subgroup$DEFECT [is.na(total_VOLUME_by_device_subgroup$DEFECT)] <- 0
total_VOLUME_by_device_subgroup <- mutate(total_VOLUME_by_device_subgroup, TOTAL = (SUCCESS + POLICY + DEFECT)) 
total_VOLUME_by_device_subgroup <- filter(total_VOLUME_by_device_subgroup, TOTAL > 0)
total_VOLUME_by_device_subgroup$TOTAL <- as.numeric(total_VOLUME_by_device_subgroup$TOTAL)
ggplot(total_VOLUME_by_device_subgroup, aes(x = PRODUCT_NAME, y = TOTAL))+
  geom_col()+ 
  scale_y_continuous(labels = comma) +
  facet_grid(DEVICE_GROUP ~ ., scales = "free") +
  coord_flip() +
  theme_classic()


# Hourly Policy/Fail rate by device group, chart over time and box plot
rate_hourly_RESULT_group_device <- Login %>% 
  group_by(Timestamp, DEVICE_GROUP, RESULT) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total) 
rate_hourly_RESULT_group_device$SUCCESS [is.na(rate_hourly_RESULT_group_device$SUCCESS)] <- 0
rate_hourly_RESULT_group_device$POLICY [is.na(rate_hourly_RESULT_group_device$POLICY)] <- 0
rate_hourly_RESULT_group_device$DEFECT [is.na(rate_hourly_RESULT_group_device$DEFECT)] <- 0
rate_hourly_RESULT_group_device <- rate_hourly_RESULT_group_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_GROUP))
rate_hourly_RESULT_group_device$'<NA>' <- rate_hourly_RESULT_group_device$'<NA>' <- NULL

# Created one data frame for each device  group
rate_hourly_RESULT_group_deviceiPhone <- filter(rate_hourly_RESULT_group_device, DEVICE_GROUP == "iPhone")
rate_hourly_RESULT_group_deviceiPad <- filter(rate_hourly_RESULT_group_device, DEVICE_GROUP == "iPad")
rate_hourly_RESULT_group_deviceiPod <- filter(rate_hourly_RESULT_group_device, DEVICE_GROUP == "iPod Touch")

# Chart POLICY rate over time by device group
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(col = DEVICE_GROUP, group = DEVICE_GROUP))+
  geom_line(data = rate_hourly_RESULT_group_deviceiPad, aes(color = DEVICE_GROUP, group = DEVICE_GROUP))+
  geom_line(data = rate_hourly_RESULT_group_deviceiPod, aes(color = DEVICE_GROUP, group = DEVICE_GROUP))

# Box plot hourly POLICY rate counts 
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = DEVICE_GROUP, y = POLICY_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_group_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_group_deviceiPod, aes())

# Chart FAILURE rate over time by device  group
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(color = DEVICE_GROUP, group = DEVICE_GROUP))+
  geom_line(data = rate_hourly_RESULT_group_deviceiPad, aes(color = DEVICE_GROUP, group = DEVICE_GROUP))+
  geom_line(data = rate_hourly_RESULT_group_deviceiPod, aes(color = DEVICE_GROUP, group = DEVICE_GROUP))

# Box plot hourly FAILURE rate counts 
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = DEVICE_GROUP, y = FAIL_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_group_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_group_deviceiPod, aes())

# Create Policy/Fail rate data frame by device group and sub group   
rate_hourly_RESULT_subgroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_GROUP,PRODUCT_NAME, RESULT) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total) 
rate_hourly_RESULT_subgroup_device$SUCCESS [is.na(rate_hourly_RESULT_subgroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_subgroup_device$POLICY [is.na(rate_hourly_RESULT_subgroup_device$POLICY)] <- 0
rate_hourly_RESULT_subgroup_device$DEFECT [is.na(rate_hourly_RESULT_subgroup_device$DEFECT)] <- 0
rate_hourly_RESULT_subgroup_device <- rate_hourly_RESULT_subgroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_GROUP))

# Filter and split into data frames for each group
rate_hourly_RESULT_group_deviceiPhone <-  filter(rate_hourly_RESULT_subgroup_device, DEVICE_GROUP == "iPhone")
rate_hourly_RESULT_group_deviceiPad <-  filter(rate_hourly_RESULT_subgroup_device, DEVICE_GROUP == "iPad")
rate_hourly_RESULT_group_deviceiPod <- filter(rate_hourly_RESULT_subgroup_device, DEVICE_GROUP == "iPod Touch")

# POLICY RATE - Plot policy rate over time for each group, stacked by subgroup
#for iPhone
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = PRODUCT_NAME, group = PRODUCT_NAME))

#for iPad
ggplot(rate_hourly_RESULT_group_deviceiPad, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = PRODUCT_NAME, group = PRODUCT_NAME))

ggplot(rate_hourly_RESULT_group_deviceiPod, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = PRODUCT_NAME, group = PRODUCT_NAME))

# POLICY RATE - Box plot policy rate for each group, split by subgroup
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_group_deviceiPad, aes(x = PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_group_deviceiPod, aes(x = PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

# FAIL RATE - Plot fail rate over time for each group, stacked by subgroup

ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = PRODUCT_NAME, group = PRODUCT_NAME))

ggplot(rate_hourly_RESULT_group_deviceiPad, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = PRODUCT_NAME, group = PRODUCT_NAME))

ggplot(rate_hourly_RESULT_group_deviceiPod, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = PRODUCT_NAME, group = PRODUCT_NAME))

# FAIL RATE - Box plot fail rate over time for each group, stacked by subgroup
ggplot(rate_hourly_RESULT_group_deviceiPhone, aes(x = PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_group_deviceiPad, aes(x = PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_group_deviceiPod, aes(x = PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())


# Investigate - Elevated iPad Fail Rates

# Put iPad data into a separate data frame, group by RESULT RATE and DEVICE
x <- filter(Login, DEVICE_GROUP =="iPad") %>%
  group_by(Timestamp, DEVICE, RESULT) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad Fail Rate Box Plot by DEVICE
ggplot(x, aes(x = DEVICE, y = FAIL_RATE))+
  geom_boxplot(aes())

# Put iPad data into a separate data frame, (iPad) group by RESULT RATE and AuthMethod
x <- filter(Login, DEVICE_GROUP =="iPad") %>%                                              
  filter(DEVICE=="iPad") %>%
  group_by(Timestamp, DEVICE_GROUP, AUTH_METHOD, RESULT) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad (iPad App) Fail RATE Box Plot by AUTH_METHOD
ggplot(x, aes(x = AUTH_METHOD, y = FAIL_RATE))+
  geom_boxplot(aes()) + 
  facet_grid(. ~ DEVICE_GROUP, scales = "free") +
  theme_classic()

# iPad (iPad) Fail RATE Box Plot by AUTH_METHOD for each sub group
x <- filter(Login, DEVICE_GROUP =="iPad") %>% 
    filter(DEVICE=="iPad") %>%
  group_by(Timestamp, AUTH_METHOD, PRODUCT_NAME, RESULT) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)
x_pwd <- filter(x, AUTH_METHOD=="Password")
x_pat <- filter(x, AUTH_METHOD=="Pattern")
x_fin <- filter(x, AUTH_METHOD=="FingerPrint")

ggplot(x_pwd, aes(x = PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes()) +
  facet_grid(. ~ AUTH_METHOD, scales = "free") +
  theme_classic()

ggplot(x_pat, aes(x = PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes()) +
  facet_grid(. ~ AUTH_METHOD, scales = "free") +
  theme_classic()



#### LOGISTIC REGRESSION, PREDICTION ####

# Load required libraries for Logistic Regression 
library(caTools)
library(ROCR)
library(effects)

# Convert dependent variable to binomial: 
# Prep dependent variables for logistic regression and prediction of DEFECT (Login Failure)

Login$RESULT_FAIL <- Login$RESULT
Login <- Login %>% 
  mutate(RESULT_FAIL = sub('SUCCESS', 0, RESULT_FAIL)) %>%
  mutate(RESULT_FAIL = sub('POLICY', 0, RESULT_FAIL)) %>%
  mutate(RESULT_FAIL = sub('DEFECT', 1, RESULT_FAIL)) 
Login$RESULT_FAIL <- as.factor(Login$RESULT_FAIL)

# Prep dependent variables for logistic regression and prediction of POLICY (Login Policy Failure)
# ROC results were significantly under 0.5.
Login$RESULT_POLICY <- Login$RESULT
Login <- Login %>% 
  mutate(RESULT_POLICY = sub('SUCCESS', 0, RESULT_POLICY)) %>%
  mutate(RESULT_POLICY = sub('POLICY', 1, RESULT_POLICY)) %>%
  mutate(RESULT_POLICY = sub('DEFECT', 0, RESULT_POLICY)) 
Login$RESULT_POLICY <- as.factor(Login$RESULT_POLICY)



# Convert statistically significant OS versions to binomial:
Login$VER_FAIL <- factor(Login$DEVICE_OPERATING_SYSTEM_VERSION, levels=c("10.0.2", "10.0.3", "10.2.1", "10.3", "10.3.1", "10.3.2", "9.0.2", "9.3", "9.3.1", "9.3.4", "9.3.5"))
Login <- Login %>%
  mutate(VER_FAIL = sub("^10.*", 1, VER_FAIL)) %>%
  mutate(VER_FAIL = sub("^9.*", 1, VER_FAIL))
Login$VER_FAIL[Login$VER_FAIL != 1] <- 0
Login$VER_FAIL [is.na(Login$VER_FAIL)] <- 0

# Convert statistically significant Device Models to binomial: 
Login$DEV_MOD_FAIL <- factor(Login$DEVICE_MODEL, levels=c("iPad2,4", "iPad2,7","iPad3,2","iPad4,1", "iPad4,2", "iPad4,4","iPad4,5","iPad4,7","iPad4,8","iPad5,1","iPad5,2","iPad5,3","iPad5,4","iPad6,11","iPad6,12","iPad6,3","iPad6,4","iPad6,7","iPad6,8","iPhone4,1","iPhone5,1","iPhone5,2","iPhone5,3","iPhone5,4","iPhone6,1","iPhone6,2","iPhone7,1","iPhone7,2","iPhone8,1","iPhone8,2","iPhone8,4","iPhone9,1","iPhone9,2","iPhone9,3","iPhone9,4","iPod5,1","iPod7,1"))
Login <- Login %>%
  mutate(DEV_MOD_FAIL = sub("^iP.*", 1, DEV_MOD_FAIL)) 
Login$DEV_MOD_FAIL[Login$DEV_MOD_FAIL != 1] <- 0
Login$DEV_MOD_FAIL [is.na(Login$DEV_MOD_FAIL)] <- 0

# Summarize and table totals by binomial value 
# Required approach because each row represents aggregated and grouped counts


totals_fail <- Login %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(totals_fail)

totals_policy <- Login %>% 
  group_by(RESULT_POLICY) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_POLICY, Total) 
print(totals_policy)

# Convert to factors for various iterations of model 

Login$DEVICE_MODEL <- as.factor(Login$DEVICE_MODEL)
Login$PRODUCT_NAME <- as.factor(Login$PRODUCT_NAME)
Login$DEVICE_GROUP <- as.factor(Login$DEVICE_GROUP)
Login$PRODUCT_MODEL_INFO <- as.factor(Login$PRODUCT_MODEL_INFO)
Login$Date <- as.factor(Login$Date)
Login$VER_FAIL <- as.factor(Login$VER_FAIL)
Login$DEV_MOD_FAIL <- as.factor(Login$DEV_MOD_FAIL)
Login$RESULT_FAIL <- as.factor(Login$RESULT_FAIL)
Login$Weekday <- as.factor(Login$Weekday)
Login$Hour <- as.numeric(Login$Hour)

# Created logistic regression model on entire dataset through multiple iterations and to 
# identify the best independent variables to use. 

LogMod <- glm(RESULT_FAIL ~ 
                AUTH_METHOD
              + DEVICE_MODEL
              + DEVICE_OPERATING_SYSTEM_VERSION,
              weights=Volume,
              data=Login,
              family="binomial")
summary(LogMod)

# Split data for training, testing and prediction
set.seed(1000)
split = sample.split(Login$RESULT_FAIL, SplitRatio = 0.65)
train = subset(Login, split == TRUE)
test = subset(Login, split == FALSE)
train <- train[,-7]
test <- test[,-7]

# Collecting true colums for each subset 
trainTotals <- train %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(trainTotals)

testTotals <- test %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(testTotals)

# Created logistic regression model on training subset, using final set of independent variables.
LogMod1 <- glm(RESULT_FAIL ~ AUTH_METHOD 
               + DEVICE_MODEL,
               weights=Volume,
               data=train,
               family="binomial")
summary(LogMod1)

# Measure effectivness: ROCR for out of sample AUC
predictTrain = predict(LogMod1, type = "response", newdata = train)
table(train$RESULT_FAIL, predictTrain > 0.01)

ROCRpred = prediction(predictTrain, train$RESULT_FAIL)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.010))

# Run model on TEST data, Plot and review ROC
predictTest = predict(LogMod1, type = "response", newdata = test)
table(test$RESULT_FAIL, predictTest > 0.01)

ROCRpred = prediction(predictTest, test$RESULT_FAIL)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.010), text.adj=c(-0.5, 1.0))



