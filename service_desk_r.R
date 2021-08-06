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
filter <- function (string) {
  hour <- substr(string, 12, 13)
  int_hour <- strtoi(hour)
  minute <- substr(string,15, 16)
  int_minute <- strtoi(minute)
  retVal = int_hour + (int_minute/60)
  return (retVal)
}

# for chat time of day
boxplot(filter(chat$CreatedDate), main = "Chat Time of Day", ylab = "Hour")

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
ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = Abandoned)) +
  geom_boxplot() +
  ggtitle("Unit abandoned chats") + 
  ylab("number of abandonded chats") +
  xlab("Unit Name")

# for chat time of day
chats = dplyr::mutate(chat, chatTime = filter(chat$CreatedDate))
ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = chatTime)) +
  geom_boxplot() +
  ggtitle("Unit Chat Time of Day") +
  ylab("Chat Time of Day") +
  xlab("Unit Name")

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
  ggplot(aes(x = LiveChatDeployment.DeveloperName, fill = LiveChatDeployment.DeveloperName, response = Status)) +
  geom_bar() +
  coord_flip() +
  labs(x = "Units", y = "Number of Chats")

DeployGraph2 <- DeployGraph2 + labs(title = "Number of Chats across Units", 
                                    subtitle = "",  fill = "Live Chat Developer")

print(DeployGraph2)



# # how to find outliers in r
# Q <- quantile(chat$ChatDuration, probs=c(.25, .75), na.rm = TRUE) # can't do FALSE
# 
# # how to find outliers in r - calculate Interquartile Range
# iqr <- IQR(chat$ChatDuration)

# remove outliers
# https://stackoverflow.com/questions/68371040/adapting-a-code-for-removing-outliers-function-not-running-in-loop

Outliesplease <- function(x){
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x[x> upper_limit | x < lower_limit] = NA
  return(x)
}

chat2 <- chat %>% 
   group_by(LiveChatDeployment.DeveloperName) %>% 
   dplyr::mutate(is.numeric, Outliesplease)

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
pie(slices,labels = lbls, col=terrain.colors(length(lbls)), main="Chat Deployment based on Developers") 
