#Importing libraries
library(tidyverse) 
library(reshape2)
library(scales)
library(ggplot2)

library(ggpubr)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(ggrepel)

#Importing the data
dailyActivity_merged <- read.csv("/cloud/project/Data/dailyActivity_merged.csv")
dailyCalories_merged <- read.csv("/cloud/project/Data/dailyCalories_merged.csv")
dailyIntensities_merged <- read.csv("/cloud/project/Data/dailyIntensities_merged.csv")
dailySteps_merged <- read.csv("/cloud/project/Data/dailySteps_merged.csv")
sleepDay_merged <- read.csv("/cloud/project/Data/sleepDay_merged.csv")
weightLogInfo_merged <- read.csv("/cloud/project/Data/weightLogInfo_merged.csv")
hourly_steps <- read_csv("/cloud/project/Data/hourlySteps_merged.csv")

str(dailyActivity_merged)
str(sleepDay_merged)
str(weightLogInfo_merged)

#Remove duplicate and NA
daily_activity <- dailyActivity_merged %>%
  distinct() %>%
  drop_na()

daily_sleep <- sleepDay_merged %>%
  distinct() %>%
  drop_na()

weightLogInfo <- weightLogInfo_merged %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

#clean and rename columns
clean_names(daily_activity)
daily_activity<- rename_with(daily_activity, tolower)
clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(weightLogInfo)
weightLogInfo <- rename_with(weightLogInfo, tolower)
clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

#rename columns
daily_activity_clean <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep_clean <- daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

#merge daily activity and sleep data to create a flat structure

daily_activity_sleep_clean <- merge(daily_activity_clean, daily_sleep_clean, by=c ("id", "date"))
glimpse(daily_activity_sleep_clean)

#left merge daily activity and sleep data to create a flat structure
daily_activity_sleep_merged <- merge(daily_activity_clean, daily_sleep_clean, by=c ("id", "date"), all.x = TRUE)

#Find averages by users
#for active minutes, addup all types of active minutes and then average
#for sleep minutes, ignore the records have NA records.
daily_average_users <- daily_activity_sleep_merged %>%
  group_by(id) %>%
  summarize(meandailysteps = mean(totalsteps), meandailydistance = mean(totaldistance), 
            meandailycalories = mean(calories), meandailysleep = mean(totalminutesasleep, na.rm = TRUE),
            meandailyactiveminutes = mean(veryactiveminutes+fairlyactiveminutes+lightlyactiveminutes+sedentaryminutes))

#classify user types based on their steps
#users < 5000 = sedentary; between 5000 and 7499=lightly active; 
#between 7500 and 9999 fairly active ; greater than 10000 = very active
user_type <- daily_average_users %>%
  mutate(user_type = case_when(
    meandailysteps < 5000 ~ "sedentary",
    meandailysteps >= 5000 & meandailysteps < 7499 ~ "lightly active", 
    meandailysteps >= 7500 & meandailysteps < 9999 ~ "fairly active", 
    meandailysteps >= 10000 ~ "very active"
  ))

#now let's find percentage of user types in our dataset
user_type_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarize(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type , 
                                      levels = c("very active", "fairly active", "lightly active", "sedentary"))

#Below we can see that users are fairly distributed by their activity considering the daily amount of steps. 
#We can determine that based on users activity all kind of users wear smart-devices.
user_type_percent %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User type distribution")

#Steps and minutes asleep per weekday
weekday_steps_sleep <- daily_activity_sleep_merged %>%
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <-ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                            "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <-weekday_steps_sleep%>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep, na.rm = TRUE))

#let's check the chart data
ggarrange(
  ggplot(weekday_steps_sleep) +
    geom_col(aes(weekday, daily_steps), fill = "#006699") +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily steps per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(weekday_steps_sleep, aes(weekday, daily_sleep)) +
    geom_col(fill = "#85e0e0") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes asleep per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)

#In the above graphs above we can determine the following:
#Users walk daily the recommended amount of steps of 7500 besides Sunday's.
#Users don't sleep the recommended amount of minutes/ hours - 8 hours.

#Hourly steps throughout the day
hourly_steps<- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 


hourly_steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(steptotal)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))

#We will now determine if there is any correlation between different variables:
#Daily steps and daily sleep
#Daily steps and calories

ggarrange(
  ggplot(daily_activity_sleep_merged, aes(x=totalsteps, y=totalminutesasleep))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14)), 
  ggplot(daily_activity_sleep_merged, aes(x=totalsteps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
)

#Days used smart device
#high use - users who use their device between 21 and 31 days.
#moderate use - users who use their device between 10 and 20 days.
#low use - users who use their device between 1 and 10 days.


daily_use <- daily_activity_sleep_merged %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

#We will now create a percentage data frame to better visualize the results in the graph. 
#We are also ordering our usage levels.


daily_use_percent <- daily_use %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage, levels = c("high use", "moderate use", "low use"))

#Now that we have our new table we can create our plot:
daily_use_percent %>%
  ggplot(aes(x="",y=total_percent, fill=usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily use of smart device")


#Analyzing our results we can see that
#88% of the users of our sample use their device frequently - between 21 to 31 days.
#9% use their device 11 to 20 days.
#3% of our sample use really rarely their device.

#Being more precise we want to see how many minutes do users wear their device per day. 
#For that we will merge the created daily_use data frame and daily_activity to be able to 
#filter results by daily use of device as well.



daily_use_merged <- merge(daily_activity, daily_use, by=c ("id"))
head(daily_use_merged)

#We need to create a new data frame calculating the total amount of minutes users wore the device every day and creating three different categories:
#All day - device was worn all day.
#More than half day - device was worn more than half of the day.
#Less than half day - device was worn less than half of the day.

minutes_worn <- daily_use_merged %>% 
  mutate(total_minutes_worn = veryactiveminutes+fairlyactiveminutes+lightlyactiveminutes+sedentaryminutes)%>%
  mutate (percent_minutes_worn = (total_minutes_worn/1440)*100) %>%
  mutate (worn = case_when(
    percent_minutes_worn == 100 ~ "All day",
    percent_minutes_worn < 100 & percent_minutes_worn >= 50~ "More than half day", 
    percent_minutes_worn < 50 & percent_minutes_worn > 0 ~ "Less than half day"
  ))


#As we have done before, to better visualize our results we will create new data frames. 
#In this case we will create four different data frames to arrange them later on on a same visualization.
#First data frame will show the total of users and will calculate percentage of minutes worn the device 
#taking into consideration the three categories created.
#The three other data frames are filtered by category of daily users so that we can see also the difference of daily use and time use.

minutes_worn_percent<- minutes_worn%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))


minutes_worn_highuse <- minutes_worn%>%
  filter (usage == "high use")%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_moduse <- minutes_worn%>%
  filter(usage == "moderate use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_lowuse <- minutes_worn%>%
  filter (usage == "low use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_highuse$worn <- factor(minutes_worn_highuse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_percent$worn <- factor(minutes_worn_percent$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_moduse$worn <- factor(minutes_worn_moduse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_lowuse$worn <- factor(minutes_worn_lowuse$worn, levels = c("All day", "More than half day", "Less than half day"))

#Now that we have created the four data frames and also ordered worn level categories, we can visualize our results in the following plots. 
#All the plots have been arranged together for a better visualization.

ggarrange(
  ggplot(minutes_worn_percent, aes(x="",y=total_percent, fill=worn)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5), size = 3.5)+
    labs(title="Time worn per day", subtitle = "Total Users"),
  ggarrange(
    ggplot(minutes_worn_highuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "none")+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text_repel(aes(label = labels),
                      position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "High use - Users"), 
    ggplot(minutes_worn_moduse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Moderate use - Users"), 
    ggplot(minutes_worn_lowuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Low use - Users"), 
    ncol = 3), 
  nrow = 2)


# Per our plots we can see that 36% of the total of users wear the device all day long, 60% more than half day long and just 4% less than half day.
# 
# If we filter the total users considering the days they have used the device and also check each day how long they have worn the device, we have the following results:
#   
#   Just a reminder:
#   
#   high use - users who use their device between 21 and 31 days.
# moderate use - users who use their device between 10 and 20 days.
# low use - users who use their device between 1 and 10 days.
# High users - Just 6.8% of the users that have used their device between 21 and 31 days wear it all day. 88.9% wear the device more than half day but not all day.
# 
# Moderate users are the ones who wear the device less on a daily basis.
# 
# Being low users who wear more time their device the day they use it.


# Bellabeat's mission is to empower women by providing them with the data to discover themselves.
# 
# In order for us to respond to our business task and help Bellabeat on their mission, based on our results, I would advice to use own tracking data for further analysis. Datasets used have a small sample and can be biased since we didn't have any demographic details of users. Knowing that our main target are young and adult women I would encourage to continue finding trends to be able to create a marketing stragety focused on them.
# 
# That being said, after our analysis we have found different trends that may help our online campaign and improve Bellabeat app:
#   
#   Recommendation	Description
# 1. Daily notification on steps and posts on app	We classified users into 4 categories and saw that the average of users walk more than 7,500 steps daily besides Sundays. We can encourage customers to reach at least daily recommended steps by CDC - 8.000 sending them alarms if they haven't reached the steps and creating also posts on our app explaining the benefits of reaching that goal. As CDC explains the more steps you walk the lower is the mortality rate. We also saw a positive correlation between steps and calories.
# 3. Notification and sleep techniques	Based on our results we can see that users sleep less than 8 hours a day. They could set up a desired time to go to sleep and receive a notification minutes before to prepare to sleep. Also offer helpfull resources to help customers sleep - ex. breathing advises, podcasts with relaxing music, sleep techniques.
# 2. Reward system	We are aware that some people don't get motivated by notifications so we could create a kind of game on our app for a limited period of time. Game would consist in reaching different levels based on amount of steps walked every day. You need to maintain activity level for a period of time (maybe a month) to pass to the next level. For each level you would win certain amount of stars that would be redeemable for merchandise or discount on other Bellabeat products.
# On our analysis we didn't just check trends on daily users habits we also realized that just 50% of the users use their device on a daily basis and that just 36% of the users wear the device all time the day they used it. We can continue promote Bellabeat's products features:
#   
#   Water-resistant
# Long-lasting batteries
# Fashion/ elegant products
# You can wear the products everyday to any occasion without worrying about the battery.
# 
