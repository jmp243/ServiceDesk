library(dplyr)
library(ggplot2)

# This function is used to get the int value of time
filter <- function (string) {
  hour <- substr(string, 12, 13)
  int_hour <- strtoi(hour)
  minute <- substr(string,15, 16)
  int_minute <- strtoi(minute)
  retVal = int_hour + (int_minute/60)
  return (retVal)
}

# get the data, change the path according to where you locate
# the data

chat <- read.csv("ChatTranscriptV2.csv")

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


