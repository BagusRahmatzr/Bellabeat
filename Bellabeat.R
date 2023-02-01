library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(anytime)
library(skimr)
library(janitor)
library(here)
library(ggrepel)

dailyActivity <- read.csv('D:\\Data Analis\\Bellabeat\\dailyActivity.csv')
dailyCalories <- read.csv('D:\\Data Analis\\Bellabeat\\dailyCalories.csv')
dailyIntensities <- read.csv('D:\\Data Analis\\Bellabeat\\dailyIntensities.csv')
dailySteps <- read.csv('D:\\Data Analis\\Bellabeat\\dailySteps.csv')
sleepDay <- read.csv('D:\\Data Analis\\Bellabeat\\sleepDay.csv')
weight <- read.csv('D:\\Data Analis\\Bellabeat\\weight.csv')

colnames(dailyActivity)
colnames(dailyCalories)
colnames(dailyIntensities)
colnames(dailySteps)
colnames(sleepDay)
colnames(weight)


head(dailyActivity)
head(dailyCalories)
head(dailyIntensities)
head(dailySteps)
head(sleepDay)
head(weight)

glimpse(dailyActivity)
glimpse(dailyCalories)
glimpse(dailyIntensities)
glimpse(dailySteps)
glimpse(sleepDay)
glimpse(weight)

# Pembersihan data

# melihat jumlah pengguna
n_unique(dailyActivity$Id)
n_unique(dailyCalories$Id)
n_unique(dailyIntensities$Id)
n_unique(dailySteps$Id)
n_unique(sleepDay$Id)
n_unique(weight$Id)

# melihat banyak duplikat
sum(duplicated(dailyActivity)) # 0 duplikat
sum(duplicated(dailyCalories)) # 0 duplikat
sum(duplicated(dailyIntensities)) # 0 duplikat
sum(duplicated(dailySteps)) # 0 duplikat
sum(duplicated(sleepDay))# 3 duplikat
sum(duplicated(weight)) # 0 duplikat

#hapus duplikat pada tabel sleep
sleepDay <- sleepDay[!duplicated(sleepDay), ]

# cek kembali data duplikat
sum(duplicated(dailyActivity))
sum(duplicated(dailyCalories)) 
sum(duplicated(dailyIntensities))
sum(duplicated(dailySteps)) 
sum(duplicated(sleepDay))
sum(duplicated(weight))

#menghapus duplikat dan Null(N/A)
dailyActivity <- dailyActivity %>%
  distinct() %>%
  drop_na()

dailyCalories <- dailyCalories %>%
  distinct() %>%
  drop_na()

dailyIntensities <-dailyIntensities %>%
  distinct() %>%
  drop_na()

dailySteps <- dailySteps %>%
  distinct() %>%
  drop_na()

sleepDay <- sleepDay %>%
  distinct() %>%
  drop_na()

weight <- weight %>%
  distinct() %>%
  drop_na()

#mengecek outlier
boxplot((dailyActivity[,15]), main="calories")
plot(dailyActivity$Calories)
boxplot((dailyActivity[,5]), main="TrackerDistance")
plot(dailyActivity$TrackerDistance)
boxplot((dailyActivity[,4]), main="TotalDistance")
plot(dailyActivity$TotalDistance)
boxplot((dailyActivity[,3]), main="TotalSteps")
plot(dailyActivity$TotalSteps)


# Memperbaiki format penanggalan
dailyActivity <- dailyActivity %>%
  rename(date = ActivityDate) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

dailyCalories <- dailyCalories %>%
  rename(date = ActivityDay) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

dailyIntensities <-dailyIntensities %>%
  rename(date = ActivityDay) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

dailySteps <-dailySteps %>%
  rename(date = ActivityDay) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

sleepDay <-sleepDay %>%
  rename(date = SleepDay) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

weight <-weight %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
  
head(dailyActivity)
head(dailyCalories)
head(dailyIntensities)
head(dailySteps)
head(sleepDay)
head(weight)

summary(dailyActivity)
summary(sleepDay)
summary(dailySteps)
summary(dailyCalories)
summary(weight)

dailyActvity_and_Sleep <- merge(dailyActivity, sleepDay, by=c("Id","date"))
glimpse(dailyActvity_and_Sleep)

ggplot(data = dailyActivity, aes(x=TotalSteps, y=Calories, color=date)) + 
  geom_point() + geom_smooth(method = 'loess', formula= 'y ~ x') + 
  labs(title = "Total Steps vs. Calories", x="Total Steps")

ggplot(data = dailyActivity, aes(x=TotalDistance, y=Calories, color=date)) + 
  geom_point() + geom_smooth(method = 'loess', formula= 'y ~ x')+
  labs(title = "Total Distance vs. Calories", x="Total Distance")

ggplot(data = dailyActivity, aes(x=SedentaryMinutes, y=Calories, color=date)) + 
  geom_point() + geom_smooth(method = 'loess', formula= 'y ~ x')+
  labs(title = "Sedentary Minutes vs. Calories", x="Sedentary Minutes")

ggplot(data = sleepDay, aes(x=TotalTimeInBed, y=TotalMinutesAsleep, color=date)) + 
  geom_point() + geom_smooth(method = 'loess', formula= 'y ~ x')+
  labs(title = "Total Minutes A sleep vs. Total Time In Bed", x="Total Time In Bed", y="Total Minutes A sleep")


install.packages("viridis")
library(viridis)

ggplot(data = dailyActvity_and_Sleep, aes(x=TotalMinutesAsleep, y=TotalSteps, color=Calories)) + 
  geom_point() +
  labs(title = "Total Minutes A sleep vs. TotalSteps", x="Total Minutes A sleep", y="Total Steps") +
  geom_smooth(formula = y ~ x, method=lm, mapping = aes(y = TotalSteps, x = TotalMinutesAsleep), color = "blue") +
  geom_vline(xintercept=420,linetype="dashed",size=.5, color = "red") +
  geom_vline(xintercept=540,linetype="dashed",size=.5, color = "red")

ggplot(data = dailyActvity_and_Sleep, aes(x=TotalMinutesAsleep, y=Calories)) + 
  geom_point() +
  labs(title = "Total Minutes A sleep vs. Calories", x="Total Minutes Asleep") +
  geom_smooth(formula = y ~ x, method=lm, mapping = aes(y = Calories, x = TotalMinutesAsleep), color = "blue") +
  geom_vline(xintercept=420,linetype="dashed",size=.5, color = "red") +
  geom_vline(xintercept=540,linetype="dashed",size=.5, color = "red")

ggplot(data = dailyActvity_and_Sleep, aes(x=TotalMinutesAsleep, y=SedentaryMinutes, color=Calories)) + 
  geom_point() +
  labs(title = "Total Minutes A sleep vs. Sedentary Minutes", y="Sedentary Minutes", x="Total Minutes Asleep") +
  geom_smooth(formula = y ~ x, method=lm, mapping = aes(y = SedentaryMinutes, x = TotalMinutesAsleep), color = "blue") +
  geom_vline(xintercept=420,linetype="dashed",size=.5, color = "red") +
  geom_vline(xintercept=540,linetype="dashed",size=.5, color = "red")

ggplot(data=dailyActivity, aes(x=TotalSteps, y=Calories)) + 
  geom_point(color="dark green") + geom_smooth(method = 'loess', formula= 'y ~ x') + 
  labs(title="Total Steps vs. Calories", x= "Total Steps", y = "Calories")

#ekspore data  
write.csv(dailyActivity, "D:\\Data Analis\\Bellabeat\\dailyActivity_new.csv", row.names = FALSE)
write.csv(dailyCalories, "D:\\Data Analis\\Bellabeat\\dailyCalories_new.csv", row.names = FALSE)
write.csv(dailyIntensities, "D:\\Data Analis\\Bellabeat\\dailyIntensities_new.csv", row.names = FALSE)
write.csv(dailySteps, "D:\\Data Analis\\Bellabeat\\dailySteps_new.csv", row.names = FALSE)
write.csv(sleepDay, "D:\\Data Analis\\Bellabeat\\sleepDay_new.csv", row.names = FALSE)
write.csv(weight, "D:\\Data Analis\\Bellabeat\\weight_new.csv", row.names = FALSE)