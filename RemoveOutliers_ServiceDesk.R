# Jung Mee Park
# August 6, 2021
# remove extreme outliers in R
# https://stackoverflow.com/questions/53201016/how-do-you-remove-outliers-in-r
# 
# # return boxplot object
# durationMinutes = chat$ChatDuration/60
# b <- boxplot(durationMinutes, main = "Chat Duration", ylab = "Minutes")
# 
# # find extremes from the boxplot's stats output
# lowerwhisker <- b$stats[1]
# upperwhisker <- b$stats[5]
# 
# # remove the extremes
# chat <- chat[chat>lowerwhisker & chat<upperwhisker] 
# 
# # replot Chat data
# b<-boxplot(durationMinutes, main = "Chat Duration", ylab = "Minutes")
# also read https://www.r-bloggers.com/2012/06/whisker-of-boxplot/
# install.packages("patchwork")
library(patchwork)
library(ggpubr)
library(janitor)

# Part I filter out outliers
# outliers for Chat Duration
#filter outliers
outliers <- boxplot(chat$ChatDuration)$out

#drop the rows containing outliers
chatD_out <- chat[-c(which(chat$ChatDuration %in% outliers)),] # this did work

# chatD_out1 = dplyr::mutate(chatD_out, chatTime = filter1(chatD_out$CreatedDate))
# box plot of chat duration 
durationMinutes1 <- chatD_out$ChatDuration/60
boxplot(durationMinutes1, main = "Chat Duration", ylab = "Minutes")

# filter outliers for the Wait time
# outliers1 <- boxplot(chat$WaitTime)$out
#drop the rows containing outliers
outliers1 <- boxplot(chat$WaitTime)$out
chat_wait_out <- chat[-c(which(chat$WaitTime %in% outliers1)),] # this did work

# for respondent wait time
waitMinutes1 = chat_wait_out$WaitTime/60
boxplot(waitMinutes1, main = "Respondent Wait Time", ylab = "Minutes")

# for abandoned chats
# filter outliers for the Abandoned
outliers2 <- boxplot(chat$Abandoned)$out

#drop the rows containing abandoned chat outliers
chatAb_out <- chat[-c(which(chat$Abandoned %in% outliers2)),] # this did work
boxplot(chatAb_out$Abandoned, main = "Abandoned Chats", ylab = "Seconds")


# for chat time of day, no outliers were dropped
# boxplot(filter(chat$CreatedDate), main = "Chat Time of Day", ylab = "Hour")

outliers3 <- boxplot(chat$tm1.dechr)$out

CreatedDate_out <- chat[-c(which(chat$tm1.dechr %in% outliers3)),]
# drop the rows containing outliers
boxplot(tm1.dechr, main = "Chat Time of Day", ylab = "Hour")


# Use subsetted data for further analysis
# for total chat duration
# chats1 = dplyr::mutate(chatD_out, durationHour = chatD_out$ChatDuration/60)
Chat_Dur_graph <- ggplot(chat, aes(x = LiveChatDeployment.DeveloperName, y = ChatDuration/60, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 35) +
  ylab("Chat Time in Minutes") +
  xlab("Unit Name")

Chat_Dur_graph <- Chat_Dur_graph + labs(title = "Unit Chat Duration", 
                                    subtitle = "outliers removed",  fill = "Live Chat Developer")
print(Chat_Dur_graph)

# for total wait time 
# chatD_out1 and waitMinutes1
waitMinutes1 = chat_wait_out$WaitTime/60
Wait_time_graph <- ggplot(chat_wait_out, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes1, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 4) +
  ylab("Wait Time in Minutes") +
  xlab("Unit Name")

Wait_time_graph <- Wait_time_graph + labs(title = "Unit Wait Time", 
                                        subtitle = "outliers removed",  fill = "Live Chat Developer")
print(Wait_time_graph)

# # filter out Registrar and university services
# # yes, the data is filtered but does not fix the outlier problem
# chatD_out1a <- chatD_out1 %>% 
#   dplyr::filter(LiveChatDeployment.DeveloperName != "Registrar Chat") %>% 
#   dplyr::filter(LiveChatDeployment.DeveloperName != "University Services Chat") %>% 
#   dplyr::select(LiveChatDeployment.DeveloperName, WaitTime)
# 
# # filter out more outliers
# outliers4 <- boxplot(chatD_out1a$WaitTime)$out
# chat_wait_out1 <- chatD_out1a[-c(which(chatD_out1a$WaitTime %in% outliers4)),] # this did not work
# 
# # for respondent wait time
# waitMinutes1a = chatD_out1a$WaitTime/60
# Wait_time_graph2 <- ggplot(chatD_out1a, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes1a, fill = LiveChatDeployment.DeveloperName)) +
#   geom_boxplot() +
#   ylim(0, 180) +
#   ylab("Wait Time in Minutes") +
#   xlab("Unit Name")
# 
# Wait_time_graph2 <- Wait_time_graph2 + labs(title = "Unit Wait Time", 
#                                           subtitle = "outliers removed",  fill = "Live Chat Developer")
# print(Wait_time_graph2)
# 

# Part II
# set theme
# theme_set(theme_gray())

# combined graph of boxplots
# par is for base R
# par(mfrow=c(2,2)) 

# boxplot without outliers
# boxplot of chat without outliers

# boxplot for chat duration
# durationMinutes1 = chatD_out$ChatDuration/60
# boxplot(durationMinutes1, main = "Chat Duration", ylab = "Minutes")

# overall chat duration without outliers
chatD_graph0 <- ggplot(chatD_out, aes(y = durationMinutes1)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x = "Overall", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

chatD_graph0 <- chatD_graph0 + labs(title = "Chat Duration")
print(chatD_graph0)


# grouped by unit without outliers
chatD_graph <- ggplot(chatD_out, aes(x = LiveChatDeployment.DeveloperName, y = durationMinutes1, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_graph <- chatD_graph + labs(title = "Chat Duration", subtitle = "outliers removed")

print(chatD_graph)

# full data for chat duration
chats = dplyr::mutate(chat, durationHour = chat$ChatDuration/60)
chatD_full <- ggplot(chats, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_full <- chatD_full + labs(title = "Chat Duration", subtitle = "full data")
print(chatD_full)

# plot multiple graphs

chatD_both <- (chatD_full + chatD_graph) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(chatD_both)
### more with chat duration
# chatD_graph2 <- ggplot(chatD_out1, aes(x = LiveChatDeployment.DeveloperName, 
#                                        y = ChatDuration, fill = LiveChatDeployment.DeveloperName)) +
#   geom_bar(stat = 'identity') +
#   labs(x="Unit", y="seconds") +
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   theme(legend.position="none") 
# 
# chatD_graph2 <- chatD_graph2 + labs(title = "Chat Duration", subtitle = "outliers removed")
# print(chatD_graph2)

# boxplot for wait time outliers removed
# waitMinutes1 = chat_wait_out$WaitTime/60
# boxplot(waitMinutes1, main = "Respondent Wait Time", ylab = "Minutes")
Wait_time_graph2a <- ggplot(chat_wait_out, aes(y = waitMinutes1)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Overall", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Wait_time_graph2a <- Wait_time_graph2a + labs(title = "Wait Time")
print(Wait_time_graph2a)

# grouped by unit
Wait_time_graph2 <- ggplot(chat_wait_out, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes1, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

Wait_time_graph2 <- Wait_time_graph2 + labs(title = "Wait Time", subtitle = "outliers removed")
print(Wait_time_graph2)

# full data wait time
Wait_time_graph1 <- ggplot(chat, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 5) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Wait_time_graph1 <- Wait_time_graph1 + labs(title = "Wait Time", subtitle = "full data")
print(Wait_time_graph1)

# plot multiple wait time graphs
Wait_time_both <- (Wait_time_graph1 + Wait_time_graph2) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(Wait_time_both)


# boxplot for abandoned chats
# boxplot(chatAb_out$Abandoned, main = "Abandoned Chats", ylab = "Numbers of chat")
Chat_ab_graph0 <- ggplot(chatAb_out, aes(y = Abandoned/60)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Overall", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_graph0 <- Chat_ab_graph0 + labs(title = "Abandoned Chats")
print(Chat_ab_graph0)

# grouped by unit
Chat_ab_graph <- ggplot(chatAb_out, aes(x = LiveChatDeployment.DeveloperName, y = Abandoned/60, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_graph <- Chat_ab_graph + labs(title = "Abandoned Chats", subtitle = "outliers removed")
print(Chat_ab_graph)

# full data for abandoned chats grouped by unit
Chat_ab_full <- ggplot(chat, aes(x = LiveChatDeployment.DeveloperName, y = Abandoned/60, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_full <- Chat_ab_full + labs(title = "Abandoned Chats", subtitle = "full data")
print(Chat_ab_full)

# plot multiple abandon graphs

Chat_ab_both <- (Chat_ab_full + Chat_ab_graph) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(Chat_ab_both)

# boxplot for chat time of day outliers removed 
# boxplot(filter1(chat$CreatedDate), main = "Chat Time of Day", ylab = "Hour")
# created_hour <- filter1(CreatedDate_out$AZ_time)

CreatedDate_graph0 <- ggplot(CreatedDate_out, aes(y = tm1.dechr)) +
  geom_boxplot() +
  # ylim(0, 24) +
  labs(x="Overall", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

CreatedDate_graph0 <- CreatedDate_graph0 + labs(title = "Chat Time of Day") +
  scale_y_continuous(breaks=seq(0,24,4))

print(CreatedDate_graph0)

# grouped by unit
# created_hour <- filter1(CreatedDate_out$CreatedDate)
CreatedDate_graph <- ggplot(CreatedDate_out, aes(x = LiveChatDeployment.DeveloperName, y = tm1.dechr, 
                                                 fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 24) +
  labs(x="Unit", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

CreatedDate_graph <- CreatedDate_graph + labs(title = "Chat Time of Day", subtitle = "outliers removed") +  
  scale_y_continuous(breaks=seq(0,24,4), limits = c(0,24))

print(CreatedDate_graph)

# with full data for chat time of day
# chats = dplyr::mutate(chat, chatTime = filter1(chat$CreatedDate))

#
Unit_ToD <- ggplot(chat, aes(x = LiveChatDeployment.DeveloperName, 
                              y = tm1.dechr, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ggtitle("Unit Chat Time of Day") +
  # ylim(0, 24) +
  ylab("Hour") +
  xlab("Unit") +   
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

Unit_ToD <- Unit_ToD + labs(title = "Chat Time of Day", 
                            subtitle = "full data") +
  scale_y_continuous(breaks=seq(0,24,2))
  
print(Unit_ToD)

# multiple plot of ToD with and without the outliers
# plot both Time of Day graphs


ToD_both <- (Unit_ToD + CreatedDate_graph) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(ToD_both)


# Part III combined graphs
# plot multiple graphs with the overview of service chat
ggp_all <- (chatD_graph0 + Wait_time_graph2a) / (Chat_ab_graph0 + CreatedDate_graph0) +    # Create grid of plots with title
  plot_annotation(title = "Overview of Service Desk Requests via Chat", subtitle = "outliers removed") 

ggp_all  

# grouped by units
# redo all graphs 

# redo chat duration
chatD_graph1 <- ggplot(chatD_out, aes(x = LiveChatDeployment.DeveloperName, y = durationMinutes1, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_graph1 <- chatD_graph1 + labs(title = "Chat Duration")

print(chatD_graph1)

# redo abandoned data
Wait_time_graph2b <- ggplot(chat_wait_out, aes(x = LiveChatDeployment.DeveloperName, y = waitMinutes1, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

Wait_time_graph2b <- Wait_time_graph2b + labs(title = "Wait Time")
print(Wait_time_graph2b)

# redo abandoned chat
Chat_ab_graph1 <- ggplot(chatAb_out, aes(x = LiveChatDeployment.DeveloperName, y = Abandoned/60, fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_graph1 <- Chat_ab_graph1 + labs(title = "Abandoned Chats")
print(Chat_ab_graph1)

# redo the Time of Day time scale
CreatedDate_graph1 <- ggplot(CreatedDate_out, aes(x = LiveChatDeployment.DeveloperName, y = tm1.dechr, 
                                                 fill = LiveChatDeployment.DeveloperName)) +
  geom_boxplot() +
  ylim(0, 24) +
  labs(x="Unit", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

CreatedDate_graph1 <- CreatedDate_graph1 + labs(title = "Chat Time of Day") +
  scale_y_continuous(breaks=seq(0,24,4))

print(CreatedDate_graph1)

# combine it into a graph
ggp_unit <- (chatD_graph1 + Wait_time_graph2b) / (Chat_ab_graph1 + CreatedDate_graph1) +    # Create grid of plots with title
  plot_annotation(title = "Overview of Service Desk Requests via Chat", subtitle = "outliers removed") 

ggp_unit  


##Table for Time of Day 
CreatedDate_out %>%
  group_by(LiveChatDeployment.DeveloperName) %>%
  dplyr::summarise(Ave_hour1 = mean(tm1.dechr, na.rm = TRUE))

# new graph for time of day and day of the week
DoW_graph1 <- ggplot(chat, aes(x = day_of_the_week, y = tm1.dechr, 
                                         fill = day_of_the_week)) +
  geom_boxplot() +
  ylim(0, 24) +
  labs(x="Day of the Week", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

DoW_graph1 <- DoW_graph1 + labs(title = "Chat Time of Day", subtitle = "full data") +
  scale_y_continuous(breaks=seq(0,24,4))

print(DoW_graph1)

# DoW graph with outliers removed
DoW_graph <- ggplot(CreatedDate_out, aes(x = day_of_the_week, y = tm1.dechr, 
                                                  fill = day_of_the_week)) +
  geom_boxplot() +
  ylim(0, 24) +
  labs(x="Day of the Week", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

DoW_graph <- DoW_graph + labs(title = "Chat Time of Day", subtitle = "outliers removed") +
  scale_y_continuous(breaks=seq(0,24,4), limits = c(0,24))

print(DoW_graph)

# print both on the same chart
DoW_both <- (DoW_graph1 + DoW_graph) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(DoW_both)
# #### try another look
# DoW_graph2 <- ggplot(chat, aes(x=day_of_the_week, y=tm1.dechr)) + 
#   geom_bar(aes(y = tm1.dechr, colour = "day_of_the_weekr")) +
#   # geom_smooth() +
#   # geom_point(aes(y = LiveChatDeployment.DeveloperName, colour = "LiveChatDeployment.DeveloperName")) +
#   # geom_smooth()+
#   ylab("Hour") +
#   xlab("") +
#   scale_color_discrete(name="", labels=c("", ""))
# 
# DoW_graph2 <- DoW_graph2 + labs(title = "Time of Chat Requests Timed Out based on Time of Day", 
#                                                   subtitle="full data", fill = "Time") +
#   scale_y_continuous(breaks=seq(0,24,2)) 
# 
# DoW_graph2

### Graph with counts and percentages with adjusted labels
DoW_3 <- chat %>% 
  group_by(day_of_the_week) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = round(count/sum(count), 3))

DoW_graph3 <- 
  ggplot(DoW_3, aes(day_of_the_week, 
                     count, fill = day_of_the_week)) +
  geom_bar(stat='identity') +
  labs(x = "Day of the Week", y = "Number of Chats") +
  theme(legend.position="none") +
  geom_text(aes(label = (scales::percent(pct)), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

DoW_graph3 <- DoW_graph3+ labs(title = "Number of Chats per Days", 
                                    subtitle = "full data",  fill = "Live Chat Developer")

print(DoW_graph3)

###
# DoW_4 <- chat %>% 
#   group_by(day_of_the_week) %>% 
#   summarize(count = n())   # count records by species
#   # mutate(pct = count/sum(count))
# DoW_graph4 <- 
#   ggplot(chat, aes(x = day_of_the_week, 
#                     y = count, fill = LiveChatDeployment.DeveloperName)) +
#   geom_bar(stat='identity') +
#   labs(x = "Day of the Week", y = "Number of Chats") +
#   theme(legend.position="none") +
#   geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))
# 
# DoW_graph4 <- DoW_graph4 + labs(title = "Number of Chats per Days", 
#                                subtitle = "full data",  fill = "Live Chat Developer")
# 
# print(DoW_graph4)
# 
DoW_5 <- chat %>% 
  group_by(LiveChatDeployment.DeveloperName) %>% 
  summarize(count = n())

DoW_graph5 <- chat %>%
  pivot_longer(day_of_the_week, names_to = "question", values_to = "response", 
             values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = LiveChatDeployment.DeveloperName)) +
  geom_bar() +
  # geom_text(aes(label = stat(), 
  #                group = day_of_the_week), stat = 'summary', fun = sum, vjust = -1) +
  # geom_text(stat = 'count', aes(label = ..count..) +
  labs(x = "", y = "")

DoW_graph5 <- DoW_graph5 + labs(title = "Chat Deployment by Day of the Week",
                              subtitle = "full data", fill = "Live Chat Developer")
print(DoW_graph5)

# Add summary labels to graph Total Sandbox
DoW_7 <- chat %>% 
  select("day_of_the_week", "LiveChatDeployment.DeveloperName") 

DoW_graph7 <- chat %>% 
  # pivot_longer(day_of_the_week, names_to = "question", values_to = "response",
  #               values_drop_na = TRUE) %>%
  ggplot(aes(x = day_of_the_week, y= LiveChatDeployment.DeveloperName, fill = LiveChatDeployment.DeveloperName)) +
  # ggplot(aes(x = day_of_the_week, y=count, fill = LiveChatDeployment.DeveloperName)) + 
  # geom_col(aes(fill = LiveChatDeployment.DeveloperName), position = "dodge")
  # geom_bar(stat = "identity") +
  geom_col() +
  # geom_text(stat = 'count', aes(label=c(706,4148,3319,3329,3400,2799,668))) 
  # geom_text(aes(label=c(706,4148,3319,3329,3400,2799,668))) 
  geom_text(aes(label=LiveChatDeployment.DeveloperName)) 
  # #             
DoW_graph7
# adjust for each division
# DoW graph with outliers removed
DoW_graph6 <- chat %>% 
  dplyr::filter(LiveChatDeployment.DeveloperName == "OSFA Chat") %>% 
  pivot_longer(day_of_the_week, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = LiveChatDeployment.DeveloperName)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..)) +
  geom_boxplot() +
  labs(x = "", y = "") + 
  theme(legend.position="none") 

DoW_graph6 <- DoW_graph6 + labs(title = "Chat Deployment by Day of the Week",
                                subtitle = "OFSA only", fill = "Live Chat Developer")

print(DoW_graph6)

##
DoW_graph7 <- chat %>% 
  dplyr::filter(LiveChatDeployment.DeveloperName == "24/7 Chat") %>% 
  pivot_longer(day_of_the_week, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = LiveChatDeployment.DeveloperName)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..)) +
  geom_boxplot() +
  labs(x = "", y = "") + 
  theme(legend.position="none") 

DoW_graph7 <- DoW_graph7 + labs(title = "Chat Deployment by Day of the Week",
                                subtitle = "24/7 only", fill = "Live Chat Developer")

print(DoW_graph7)

# ##
# DoW_graph8 <- chat %>% 
#   ggplot(aes(x = Location, y = tm1.dechr, fill = tm1.dechr)) +
#   geom_point() +
#   # geom_text(stat='count', aes(label=..count..)) +
#   geom_boxplot() +
#   labs(x = "", y = "") + 
#   theme(legend.position="none") 
# 
# DoW_graph8 <- DoW_graph8 + labs(title = "Chat Deployment by Day of the Week",
#                                 subtitle = "", fill = "Live Chat Developer")
# 


