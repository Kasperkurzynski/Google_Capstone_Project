library(tidyverse)
library(ggpubr)
library(lubridate)
library(gridExtra)
library(forcats)
library(janitor)
library(RColorBrewer)
library(scales)

# Data load
daily_activity <- read.csv("./project_datasets_2022/dailyActivity_merged.csv")
daily_sleep <- read.csv("./project_datasets_2022/sleepDay_merged.csv")
weight_info <- read.csv("./project_datasets_2022/weightLogInfo_merged.csv")

# Data previev
head(daily_activity)
glimpse(daily_activity)

head(daily_sleep)
glimpse(daily_sleep)

head(weight_info)
glimpse(weight_info)

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(weight_info$Id)

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(weight_info))

daily_sleep <- daily_sleep %>%
  distinct()
sum(duplicated(daily_sleep))

sum(is.na(daily_activity))
sum(is.na(daily_sleep))
sum(is.na(weight_info))

weight_info$Fat <- NULL
sum(is.na(weight_info))

daily_activity <- daily_activity %>%
  rename(date = ActivityDate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y"))

weight_info <- weight_info %>%
  rename(date = Date) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y"))

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
weight_info <- clean_names(weight_info)

daily_activity$day <- wday(daily_activity$date, label=TRUE)
daily_sleep$day <- wday(daily_sleep$date, label=TRUE)

daily_activity$day <- as.factor(daily_activity$day)
daily_sleep$day <- as.factor(daily_sleep$day)

daily_activity$day <- substr(daily_activity$day,1,1)
daily_activity$day <- recode(daily_activity$day, n = "Sunday",
                                p = "Monday",
                                w = "Tuesday",
                                ś = "Wednesday",
                                c = "Thursday",
                                p = "Friday",
                                s = "Saturday")

daily_sleep$day <- substr(daily_sleep$day,1,1)
daily_sleep$day <- recode(daily_sleep$day, n = "Sunday",
                              p = "Monday",
                              w = "Tuesday",
                              ś = "Wednesday",
                              c = "Thursday",
                              p = "Friday",
                              s = "Saturday")

merge_data <- merge(merge(daily_activity, daily_sleep, by = c('id', 'date'), all = TRUE), 
                    weight_info, by = c('id', 'date'), all = TRUE)

summary(merge_data)

final_data <- merge_data %>%
  select(-c("log_id", "is_manual_report", "weight_pounds", 
            "total_sleep_records", "tracker_distance", "logged_activities_distance")) %>%
  rename(day_activity = day.x,
         day_sleep = day.y) %>%
  mutate(day_activity = factor(day_activity),
         day_sleep = factor(day_sleep))

final_data$id <- as.factor(final_data$id)

head(final_data)
glimpse(final_data)
summary(final_data)

# Analysis
scoreShapiroSteps <- shapiro.test(final_data$total_steps)
print(scoreShapiroSteps)

scoreShapiroCalories <- shapiro.test(final_data$calories)
print(scoreShapiroCalories)

scoreShapiroBed <- shapiro.test(final_data$total_time_in_bed)
print(scoreShapiroBed)

scoreShapiroAsleep <- shapiro.test(final_data$total_minutes_asleep)
print(scoreShapiroAsleep)

ggscatter(final_data, x = "total_steps", y = "calories",
          conf.int = TRUE,
          add = "reg.line",
          color = "deepskyblue2") + stat_cor(method = "spearman") 

ggscatter(final_data, x = "total_time_in_bed", y = "total_minutes_asleep",
          conf.int = TRUE,
          add = "reg.line",
          color = "blue") + stat_cor(method = "spearman") 


sumMin <- final_data %>% 
  select(very_active_minutes, 
         fairly_active_minutes, 
         lightly_active_minutes, 
         sedentary_minutes) %>% 
  summarise(across(everything(), list(sum))) %>% 
  gather(active_level, minutes) %>% 
  mutate(active_level = factor(active_level, 
                               labels = c('Moderate Activity','Light Activity',
                                          'Sedentary','Heavy Activity'))) %>%
  mutate(percentage = percent(round(minutes / sum(minutes),3)))


ggbarplot(sumMin, x = "active_level", y = "minutes",
          color = "active_level", fill = "active_level", palette = "Set3",
          label = c("1.7%", "1.1%", "15.8%", "81.3%"), lab.col = "black", x.text.angle = 45)



sumDist <- final_data %>% 
  select(very_active_distance, 
         moderately_active_distance, 
         light_active_distance) %>% 
  summarise(across(everything(), list(sum))) %>% 
  gather(dist_level, distance) %>% 
  mutate(dist_level = factor(dist_level, 
                               labels = c('Light Activity', 'Moderate Activity', 'Heavy Activity'))) %>%
  mutate(percentage = percent(round(distance / sum(distance),3)))

ggbarplot(sumDist, x = "dist_level", y = "distance",
          color = "dist_level", fill = "dist_level", palette = "PiYg",
          label = c("28%", "10%", "62%"), lab.col = "black", x.text.angle = 45)

idAvg <- final_data %>%
  group_by(id) %>%
  summarise(AvgSteps = mean(total_steps),
            AvgCalor = mean(calories),
            AvgWeight = mean(weight_kg, na.rm=T))


ggscatter(idAvg, x = "AvgWeight", y = "AvgSteps",
          size = "AvgWeight", color = "id") + 
  stat_cor(method = "pearson") + rremove("legend")

ggscatter(idAvg, x = "AvgWeight", y = "AvgCalor",
          size = "AvgWeight", color = "id") + 
  stat_cor(method = "pearson") + rremove("legend")


ggboxplot(final_data, x = "day_activity", y = "total_steps",
          color = "day_activity", palette = "BrBg",
          add = "jitter") + stat_compare_means() + rremove("legend")

ggboxplot(final_data, x = "day_activity", y = "calories",
          color = "day_activity", palette = "BrBg",
          add = "jitter") + stat_compare_means() + rremove("legend")


histbed <- ggplot(final_data, aes(x = total_time_in_bed)) + geom_histogram(bins = 15, fill = "lightskyblue3", color = "black") +
  theme_minimal()
histasleep <- ggplot(final_data, aes(x =  total_minutes_asleep)) + geom_histogram(bins = 15, fill = "coral3", color = "black") +
  theme_minimal()
grid.arrange(histbed, histasleep)

