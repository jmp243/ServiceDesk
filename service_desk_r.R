# Jung Mee Park
# July 29, 2021
# Service Desk Chat Analysis

library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(tibble)
library(tidyverse)
# install.packages("ggstatsplot")
library(ggstatsplot)
library(lubridate)
# library(plotly)

# get the data, change the path according to where you locate
# the data

# set working directory 
# load in all data files
setwd("~/Documents/Trellis/ServiceDesk/ServiceDesk_data")

appointments <- read.csv("appointments_did.csv", header = TRUE, na.strings=c("","NA"))

availabilities <- read.csv("availabilities.csv", header = TRUE, na.strings=c("","NA"))

cases <- read.csv("cases_did.csv", header = TRUE, na.strings=c("","NA"))

chat <- read.csv("ChatTranscriptV2.csv", header = TRUE, na.strings=c("","NA"))

eprs <- read.csv("eprs_did.csv", header = TRUE, na.strings=c("","NA"))

enrollment <- read.csv("studentenrollments_did.csv", header = TRUE, na.strings=c("","NA"))

KPI <- read.csv("keyindicators_did.csv", header = TRUE, na.strings=c("","NA"))

LiveAgent <- read.csv("LiveAgentSession.csv", header = TRUE, na.strings=c("","NA"))

# check the number of NA values
sum(is.na(chat))

# # change chat duration NA to 0
# chat$ChatDuration[is.na(chat$ChatDuration)] <- 0


# examine the chat duration
summary(chat$ChatDuration)

# recode data for LiveChatDeployment.DeveloperName
chat$LiveChatDeployment.DeveloperName <- recode_factor(chat$LiveChatDeployment.DeveloperName, 
                                                       "OSFA_Chat" = "OSFA Chat", 
                                                       "OSFA_Chat_After_Hours" = "OSFA Chat After Hours",
                                                       "Registrar_Chat" = "Registrar Chat",
                                                       "SOS_Automated_Invite" = "SOS Automated",
                                                       "SOS_Chat" = "SOS Chat",
                                                       "University_Services_Chat" = "University Services Chat",
                                                       "X24_7_Chat" = "24/7 Chat")
# recode LiveChatButton.DeveloperName
chat$LiveChatButton.DeveloperName <- recode_factor(chat$LiveChatButton.DeveloperName, 
                                                       "OSFA_Chat" = "OSFA Chat", 
                                                       "OSFA_Chat_After_Hours" = "OSFA Chat After Hours",
                                                       "Registrar_Chat" = "Registrar Chat",
                                                       "SOS_Automated_Invite" = "SOS Automated",
                                                       "SOS_Chat" = "SOS Chat",
                                                       "University_Services_Chat" = "University Services Chat",
                                                       "X24_7_Chat" = "24/7 Chat")

# change minutes into hours 
durationMinutes = chat$ChatDuration/60


# view summary of ranges
  
# # This data shows which unit had the most chats
# DeployGraph <- chat %>%
#   # drop_na(LiveChatDeployment.DeveloperName) %>% 
#   ggplot(aes(x = LiveChatButton.DeveloperName, fill = LiveChatButton.DeveloperName, response = Status)) +
#   geom_bar() +
#   coord_flip() +
#   labs(x = "Units", y = "Number of Chats")
# 
# DeployGraph <- DeployGraph + labs(title = "Number of Chats across Units", 
#                                   subtitle = "")
# 
# print(DeployGraph)




# Part 1: Overall
# box plot 
par(mfrow=c(2,2))

# for total chat duration in minutes
durationMinutes = chat$ChatDuration/60
boxplot(durationMinutes, main = "Chat Duration", ylab = "Minutes")

# for respondent wait time
waitMinutes = chat$WaitTime/60
boxplot(waitMinutes, main = "Respondent Wait Time", ylab = "Minutes")

# for abandoned chats
boxplot(chat$Abandoned, main = "Abandoned Chats", ylab = "Numbers of chat")

# This function is used to get the int value of time

# filter1 <- function (string) {
#   hour <- substr(string, 12, 13)
#   int_hour <- strtoi(hour)
#   minute <- substr(string,15, 16)
#   int_minute <- strtoi(minute)
#   retVal = int_hour + (int_minute/60)
#   return (retVal)
# }

# create day of the week

# 2020-07-20T16:59:19.000Z #Complete ISO-8601 date

# change to day of the week

# date_value = "2020-10-08T18:06:08.000Z" #is 5
# day_of_the_week = date(date_value) %>%
#   wday()
# day_of_the_week

date_value = chat$CreatedDate
day_of_the_week = date(date_value) %>%
  wday()
day_of_the_week
# day of the week
# chats = dplyr::mutate(chat, day_of_the_week)
# chats$day_of_the_week <- recode_factor(chats$day_of_the_week, 
#                                                    "1" = "Sunday", 
#                                                    "2" = "Monday",
#                                                    "3" = "Tuesday",
#                                                    "4" = "Wednesday",
#                                                    "5" = "Thursday",
#                                                    "6" = "Friday",
#                                                    "7" = "Saturday")
chat = dplyr::mutate(chat, day_of_the_week)
chat$day_of_the_week <- recode_factor(chat$day_of_the_week, 
                                       "1" = "Sunday", 
                                       "2" = "Monday",
                                       "3" = "Tuesday",
                                       "4" = "Wednesday",
                                       "5" = "Thursday",
                                       "6" = "Friday",
                                       "7" = "Saturday")

# try converting time with lubridate
# date_value = "2020-10-08T18:06:08.000Z" #is 5
# day_of_the_week = date(date_value) %>%
#   wday()
# day_of_the_week
# ymd_hms("2020-10-08T18:06:08.000Z")

Date_Time = ymd_hms(chat$CreatedDate)
head(Date_Time) #2020-07-02 21:56:42 UTC
chat <- dplyr::mutate(chat, Date_Time)

# convert time zones
# df <- data.frame(actualtime = c(
#   '2015-04-15 13:10:00',
#   '2015-04-15 14:22:00',
#   '2015-04-15 10:14:00'),
#   timezone = c(
#     'Australia/Sydney',
#     'Australia/Perth',
#     'Australia/Perth'))
# ts_df <- do.call(rbind, lapply(1:nrow(), function(i) {
#   tz <- chat$timezone[i]
#   raw <- as.POSIXct(strptime(
#     chat$Date_Time[i],
#     format = "%Y-%m-%d %H:%M:%S",
#     tz ="UTC"),
#     tz = "UTC")
#   ts <- format(raw, tz = tz, usetz = TRUE)
#   data.frame(raw=raw,tz=tz,converted = as.POSIXct(ts))
# }))

# add the variable Date_Time column to the dataset

# AZ_time = chatTime - 7
# chat <- dplyr::mutate(chat, AZ_time = (chatTime - 7))
# add AZ time to variables

t1 <- as.POSIXct(Date_Time, tz = "GMT")
attributes(t1)$tzone
AZ_time <- lubridate::with_tz(t1, "MST")

# head(AZ_time)
chat <- dplyr::mutate(chat, AZ_time)


# convert time to decimals 
tm1.dechr <- hour(chat$AZ_time) + minute(chat$AZ_time)/60 + second(chat$AZ_time)/3600
tm1.dechr

chat <- dplyr::mutate(chat, tm1.dechr)
## [1] 23.92

# chatTime = filter1(chat$CreatedDate)


# for chat time of day
boxplot(filter1(chat$AZ_time), main = "Chat Time of Day", ylab = "Hour")

# boxplot(filter2(chat$CreatedDate))

# Part 2: By unit

# for total chat duration
chats = dplyr::mutate(chat, durationHour = chat$ChatDuration/60)
ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = durationHour)) +
  geom_boxplot() +
  ggtitle("Unit Chat Duration") + 
  ylim(0, 100) +
  ylab("Chat Time in Minutes") +
  xlab("Unit Name")


# for respondent wait time
ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = WaitTime)) +
  geom_boxplot() +
  ggtitle("Unit respondent wait time") + 
  ylim(0, 5000) +
  ylab("Chat Time in Seconds") +
  xlab("Unit Name")

# for abandoned chats
Ab_Chat <- ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = Abandoned,
                             fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ggtitle("Unit abandoned chats") + 
  ylab("number of abandonded chats") +
  xlab("Unit Name")

Ab_Chat <- Ab_Chat + labs(title = "Abandoned Chats", 
                          subtitle = "full data",  fill = "Live Chat Developer")

print(Ab_Chat)
# for chat time of day
# chats = dplyr::mutate(chat, chatTime = filter1(chat$CreatedDate))
Unit_ToD <- ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, 
                              y = chatTime, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  ylim(0, 24) +
  ylab("Chat Time of Day") +
  xlab("Unit Name")

Unit_ToD <- Unit_ToD + labs(title = "Unit Chat Time of Day", 
                            subtitle = "full data",  fill = "Live Chat Developer")
print(Unit_ToD)
# group data using key variables and grouped by LCDDN
chats %>%
  select(ChatDuration, Abandoned, CreatedDate, WaitTime, LiveChatDeployment.DeveloperName, chatTime) %>%
  group_by(LiveChatDeployment.DeveloperName)

# # change column name for LCDDN, omitted. Just change name in legend
# colnames(chats)[colnames(chats) == "LiveChatDeployment.DeveloperName"] <- "Live Chat Developer"

# Create a bar graph based on Live chat developer
# new bar graph that is cleaner
DeployGraph2 <- chat %>%
  # drop_na(LiveChatDeployment.DeveloperName) %>% 
  ggplot(aes(x = LiveChatDeployment.DeveloperName, fill = LiveChatDeployment.DeveloperName, 
             response = Status)) +
  geom_bar() +
  labs(x = "Units", y = "Number of Chats") +
  theme(legend.position="none") +
  coord_flip() 

DeployGraph2 <- DeployGraph2 + labs(title = "Number of Chats across Units", 
                                    subtitle = "",  fill = "Live Chat Developer")

print(DeployGraph2)


## try to add more variables, try to add a percentage
Graph3 <- chat %>% 
  group_by(LiveChatDeployment.DeveloperName) %>% 
  summarize(count = n())  %>%  # count records by species
  mutate(pct = count/sum(count))  # find percent of total

DeployGraph3 <- 
  # drop_na(LiveChatDeployment.DeveloperName) %>% 
  ggplot(Graph3, aes(LiveChatDeployment.DeveloperName, 
                     pct, fill = LiveChatDeployment.DeveloperName)) +
  geom_bar(stat='identity') +
  labs(x = "Units", y = "Number of Chats") +
  theme(legend.position="none") +
  coord_flip() +
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))
  # scale_y_continuous(labels = scales::percent)

DeployGraph3 <- DeployGraph3 + labs(title = "Number of Chats across Units", 
                                    subtitle = "",  fill = "Live Chat Developer")

print(DeployGraph3)

### Graph with counts and percentages with adjusted labels
Graph4 <- chat %>% 
  group_by(LiveChatDeployment.DeveloperName) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

# d %>% 
#   ggplot(aes(x = counts, y = group)) +
#   geom_col() +
#   geom_text(aes(label = scales::percent(pct), x = if_else(counts > 0.1*max(counts), counts/2, counts+10))) OR
# geom_text(aes(label = scales::percent(pct), x = if_else(counts > 0.1*max(counts), counts/2, counts+ 0.05*max(counts))))

DeployGraph4 <- 
  # drop_na(LiveChatDeployment.DeveloperName) %>% 
  ggplot(Graph4, aes(LiveChatDeployment.DeveloperName, 
                     count, fill = LiveChatDeployment.DeveloperName)) +
  geom_bar(stat='identity') +
  labs(x = "Units", y = "Number of Chats") +
  coord_flip() +
  # geom_text(aes(label=LiveChatDeployment.DeveloperName), hjust=-0.3) +
  theme(legend.position="none") +
  # geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))
  # geom_text(aes(label=scales::percent(pct)), x = count + 15)
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

  # geom_text(aes(label=scales::percent(pct)), vjust = 1.0)
  # geom_text(aes(label = scales::percent(pct)), vjust = 0.5, nudge_x = -.5) 
# scale_y_continuous(labels = scales::percent)

DeployGraph4 <- DeployGraph4 + labs(title = "Number of Chats across Units", 
                                    subtitle = "full data",  fill = "Live Chat Developer")

print(DeployGraph4)


# # how to find outliers in r
# Q <- quantile(chat$ChatDuration, probs=c(.25, .75), na.rm = TRUE) # can't do FALSE
# 
# # how to find outliers in r - calculate Interquartile Range
# iqr <- IQR(chat$ChatDuration)

# remove outliers
# https://stackoverflow.com/questions/68371040/adapting-a-code-for-removing-outliers-function-not-running-in-loop

# Outliesplease <- function(x){
#   Q1 <- quantile(x, probs=.25)
#   Q3 <- quantile(x, probs=.75)
#   iqr = Q3-Q1
#   upper_limit = Q3 + (iqr*1.5)
#   lower_limit = Q1 - (iqr*1.5)
#   x[x> upper_limit | x < lower_limit] = NA
#   return(x)
# } # this works as a function 

chat2 <- 
   select(chat, Abandoned, WaitTime) 
# %>% 
#   dplyr::summarize(wait_sec = mean(WaitTime, na.rm=TRUE)) 

chat2

# group data based on units
# filter by large campus groupings
# filter(chat, ChatDuration > 0) argument works on its own
# average chat duration based on Live Chat Deployment Developer

# average chat duration by division
chat %>%
  group_by(LiveChatDeployment.DeveloperName) %>%
  # dplyr::summarize(Mean = (mean(ChatDuration, na.rm=TRUE)/60)) #for hours
  dplyr::summarize(Mean = mean(ChatDuration, na.rm=TRUE))

# average wait times by division
chat %>%
  group_by(LiveChatDeployment.DeveloperName) %>%
  dplyr::summarize(Ave_wait = mean(WaitTime, na.rm=TRUE))

# average time for abandoned chats
chat %>%
  group_by(LiveChatDeployment.DeveloperName) %>%
  dplyr::summarise(Ave_abandon = mean(Abandoned, na.rm = TRUE))

# chat time of day 
# based on chats = dplyr::mutate(chat, chatTime = filter(chat$CreatedDate))
chats %>%
  group_by(LiveChatDeployment.DeveloperName) %>%
  dplyr::summarise(Ave_hour = mean(chatTime, na.rm = TRUE))


# Pie Chart of Live Deployment
slices <- c(8695, 73, 1125, 30, 8446)
lbls <- c("OSFA", "Registrar",  "SOS", "Univ Serv","24/7")
pct <- round(slices/sum(slices)*100, 1)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=terrain.colors(length(lbls))) 
pie(slices,labels = lbls, col=terrain.colors(length(lbls)), 
    main="Chat Deployment based on Developers")
