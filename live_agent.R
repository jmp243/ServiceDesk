# Jung Mee Park
# August 9, 2021

# looking at Live Agent data
head(LiveAgent)
library(knitr)

# Convert time in CreatedDate 
# This function is used to get the int value of time
# filter1 <- function (string) {
#   hour <- substr(string, 12, 13)
#   int_hour <- strtoi(hour)
#   minute <- substr(string,15, 16)
#   int_minute <- strtoi(minute)
#   retVal = int_hour + (int_minute/60)
#   return (retVal)
# }

# manipulate the dataset date time
library(lubridate)
date_value2 = LiveAgent$CreatedDate
day_of_the_week2 = date(date_value2) %>%
  wday()
day_of_the_week2

LiveAgent2 = dplyr::mutate(LiveAgent, day_of_the_week2)

LiveAgent2$day_of_the_week2 <- recode_factor(LiveAgent2$day_of_the_week2, 
                                      "1" = "Sunday", 
                                      "2" = "Monday",
                                      "3" = "Tuesday",
                                      "4" = "Wednesday",
                                      "5" = "Thursday",
                                      "6" = "Friday",
                                      "7" = "Saturday")

Date_Time2 = ymd_hms(LiveAgent2$CreatedDate)

t2 <- as.POSIXct(Date_Time2, tz = "GMT")
attributes(t2)$tzone
AZ_time2 <- lubridate::with_tz(t2, "MST")

# head(AZ_time)
LiveAgent2 <- dplyr::mutate(LiveAgent2, AZ_time2)

# convert to decimal time instead of hour_created
tm2.dechr <- hour(LiveAgent2$AZ_time2) + minute(LiveAgent2$AZ_time2)/60 + second(LiveAgent2$AZ_time2)/3600
# tm2.dechr
#### updated dataset is LiveAgent2
summary(LiveAgent2$ChatReqTimedOut) # max is about 1010 for time maxed out

tibble(LiveAgent2$TimeAtCapacity, LiveAgent2$TimeIdle, LiveAgent2$TimeInAwayStatus)

# someMinutes = data$variable/60

# explore the data
glimpse(LiveAgent2)
# finding means of Time variables
# mean(LiveAgent2$TimeAtCapacity)
# 
# LiveAgent2 %>% 
#   dplyr::summarize(TimeAtCap = mean(TimeAtCapacity, na.rm=TRUE)/60) 
# 
# LiveAgent2 %>% 
#   dplyr::summarize(idle = mean(TimeIdle, na.rm=TRUE)/60)  # seems to have a huge outlier
# 
# LiveAgent2 %>% 
#   dplyr::summarize(InAway = mean(TimeInAwayStatus, na.rm=TRUE)/60)
# 
# LiveAgent2 %>% 
#   dplyr::summarize(in_chats = mean(TimeInChats, na.rm=TRUE)/60)
# 
# LiveAgent2 %>% 
#   dplyr::summarize(online = mean(TimeInOnlineStatus, na.rm=TRUE)/60)

#https://www.statology.org/calculate-mean-multiple-columns-in-r/
LiveA_table <- LiveAgent2 %>% 
  select(starts_with("Time"))

colMeans(LiveA_table, na.rm = TRUE)


# also examine the ChatReq
ChatReq <- LiveAgent2 %>% 
  select((starts_with("Chat")))

Time <- LiveAgent2 %>% 
  select((starts_with("Time")))

ChatTime <- LiveAgent2 %>% 
  select((starts_with("Chat"))|(starts_with("Time"))) # this or the version below

ChatTime2 <- LiveAgent2 %>% 
  select(starts_with(c("Chat","Time","hour","login"))) # make sure I don't add login twice

summary(ChatTime2)

#######
# ChatTime2 data set
# chats = dplyr::mutate(chat, chatTime = filter1(chat$CreatedDate))
# for chat req assigned
Agent_ToD <- ggplot(LiveAgent2, aes(x = ChatReqAssigned, 
                              y = tm2.dechr, fill = ChatReqAssigned)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_ToD <- Agent_ToD + labs(title = "Chat Request Assigned") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_ToD)
## For Chat Req Declined
Agent_ToD1 <- ggplot(LiveAgent2, aes(x = ChatReqDeclined, 
                                    y = tm2.dechr)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_ToD1 <- Agent_ToD1 + labs(title = "Chat Request Declined") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_ToD1)

## For Chat Req Engaged
Agent_ToD2 <- ggplot(LiveAgent2, aes(x = ChatReqEngaged, 
                                     y = tm2.dechr, fill = ChatReqEngaged)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_ToD2 <- Agent_ToD2 + labs(title = "Chat Request Engaged") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_ToD2)

## For Chat Req TimedOUt
Agent_ToD3 <- ggplot(LiveAgent2, aes(x = ChatReqTimedOut, 
                                     y = tm2.dechr, fill = ChatReqTimedOut)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_ToD3 <- Agent_ToD3 + labs(title = "Chat Request Timed Out") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_ToD3)

# print all 4 ChatReq plots
LiveAgent2_all <- (Agent_ToD + Agent_ToD1) / (Agent_ToD2 + Agent_ToD3) +    # Create grid of plots with title
  plot_annotation(title = "Overview of Live Agent Requests", subtitle = "full data") 

LiveAgent2_all 

### box plot of login 
# Date_Time3 = ymd_hms(LiveAgent2$LoginTime)
# 
# t3 <- as.POSIXct(Date_Time3, tz = "GMT")
# attributes(t2)$tzone
# AZ_time3 <- lubridate::with_tz(t3, "MST")
# 
# LiveAgent2 <- dplyr::mutate(LiveAgent2, AZ_time3)

# convert to decimal time
login <- hour(LiveAgent2$AZ_time2) + minute(LiveAgent2$AZ_time2)/60 + second(LiveAgent2$AZ_time2)/3600

Agent_login <- ggplot(LiveAgent2, aes(x = login, 
                                     y = tm2.dechr, fill = ChatReqAssigned)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_login <- Agent_login + labs(title = "Live Agent Time Created") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_login)

# box plot of hour created
Agent_hour <- ggplot(LiveAgent2, aes(x = tm2.dechr, 
                                      y = login, fill = ChatReqAssigned)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Overall") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Agent_hour <- Agent_hour + labs(title = "Live Agent Login Hour") +
  scale_y_continuous(breaks=seq(0,24,2))

print(Agent_hour)

### plot hour created and login onto the same graph

# LiveAgent2_time <- melt(LiveAgent2,id.vars=c("Id"),value.name="value",
#                      variable.name="Hour")
# 
# head(LiveAgent2_time)

# plot <- ggplot(LiveAgent2_time, aes(x=Hour, y=value, group = Id)) +
#   geom_line() + labs(y= "Deaths", x = "Day")
# plot + ggtitle("")
#   +geom_point()
# 
# print(plot)

##### More useful graphs that show both hour created and login time
# library(quantmod)
# 
# LiveAgent2_Time_plot <- ggplot(LiveAgent2, aes(x=ChatReqEngaged, y=tm2.dechr)) + 
#   geom_point(aes(y = tm2.dechr, colour = "tm2.dechr")) +
#   # geom_smooth() +
#   geom_point(aes(y = login, colour = "login")) +
#   # geom_smooth()+
#   ylab("Hour") +
#   xlab("Number of Chat Requests Engaged") +
#   scale_color_discrete(name="Time", labels=c("Chat Created", "Agent Login Time"))
# 
# LiveAgent2_Time_plot <- LiveAgent2_Time_plot + labs(title = "Time of Chat Requests Engaged based on Time of Day", 
#                                                     subtitle="full data", fill = "Time") +
#   scale_y_continuous(breaks=seq(0,24,2)) +
#   scale_x_continuous()
# 
# LiveAgent2_Time_plot

# ## chat req timed out
# LiveAgent2_Time_out <- ggplot(LiveAgent2, aes(x=ChatReqTimedOut, y=tm2.dechr)) + 
#   geom_point(aes(y = tm2.dechr, colour = "tm2.dechr")) +
#   # geom_smooth() +
#   geom_point(aes(y = login, colour = "login")) +
#   # geom_smooth()+
#   ylab("Hour") +
#   xlab("Chat Requests Timed Out") +
#   scale_color_discrete(name="Time", labels=c("Chat Created", "Agent Login Time"))
# 
# LiveAgent2_Time_out <- LiveAgent2_Time_out + labs(title = "Time of Chat Requests Timed Out based on Time of Day", 
#                                                     subtitle="full data", fill = "Time") +
#   scale_y_continuous(breaks=seq(0,24,2)) +
#   scale_x_continuous()
# 
# LiveAgent2_Time_out
### new attempt at a combined graph
# plot(LiveAgent2$ChatReqEngaged, LiveAgent2$hour_created, col="blue", pch="o", lty=1)
# points(LiveAgent2$ChatReqEngaged,LiveAgent2$login, col="red", pch="*")
# lines(LiveAgent2$ChatReqEngaged,LiveAgent2$login, col="red",lty=2)
# 
# # live agent time at capacity
# LiveAgent2_Cap <- ggplot(LiveAgent2, aes(x=TimeAtCapacity, y=hour_created)) + 
#   geom_point(aes(y = hour_created, colour = "hour_created")) +
#   geom_point(aes(y = login, colour = "login")) +
#   ylab("Hour") +
#   xlab("Time at Capacity") + 
#   scale_color_discrete(name="Time", labels=c("Chat Created", "Agent Login Time"))
# 
# LiveAgent2_Cap <- LiveAgent2_Cap + labs(title = "Time at Capacity based on Time of Day", 
#                                         subtitle = "full data") +
#   scale_y_continuous(breaks=seq(0,24,2)) 
#   # scale_color_discrete(name = "Legend")
# 
# LiveAgent2_Cap

# live agent time idle
LiveAgent2_Idle <- ggplot(LiveAgent2, aes(x=TimeIdle/60, y=tm2.dechr)) + 
  geom_point(aes(y = tm2.dechr, colour = "tm2.dechr")) +
  geom_point(aes(y = login, colour = "login")) +
  ylab("Hour") +
  xlab("Idle Time (minutes)") + 
  scale_color_discrete(name="Time", labels=c("Chat Created", "Agent Login Time"))

LiveAgent2_Idle <- LiveAgent2_Idle + labs(title = "Idle Time based on Time of Day", 
                                          subtitle = "full data") +
  scale_y_continuous(breaks=seq(0,24,2)) 
print(LiveAgent2_Idle)
# scale_color_discrete(name = "Legend")

# LiveAgent2_Idle
# ### select data columns first
# LiveAgent2_time <- LiveAgent2 %>% 
#   dplyr:: select(c("hour_created","login", "ChatReqEngaged"))
# 
# ggp <- ggplot(LiveAgent2_time, aes(x=ChatReqEngaged, y=login, col = hour_created)) +             # Create ggplot2 plot
#   geom_line()
# ggp # I don't know how to interpret this 
# 
# ### plot for time idle and chat req timed out
# mod8plot <- ggplot(LiveAgent2, aes(x= TimeIdle/60, y = ChatReqTimedOut)) +
#   geom_line() +
#   # geom_smooth() +
#   stat_smooth(method = "loess", se = FALSE) +
# labs(x="Idle Time (minutes)", y="Chat Requests Timed Out") 
# 
# mod8plot <- mod8plot + labs(title = "Chat Requests Timed Out by Idle Time", 
#                             subtitle = "full data") 
# # scale_color_discrete(name = "Live Chat Developer")
# 
# mod8plot
# 
