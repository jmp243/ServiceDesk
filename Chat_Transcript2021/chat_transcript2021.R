#SET UP Environment
library('tidyverse')
library('dplyr')
library('readxl')
library('salesforcer')
# library('RForcecom')
library('lubridate')
library('httpuv')
library(forcats)

# set working directory in Amazon WorkSpaces
# setwd("U:/WorkSpaces/jmpark_data")
setwd("D:/Users/jmpark/WorkSpaces/jmpark_data")

options(digits=3)

sf_auth() 
# chat transcript
soqlchat_tran <- sprintf("SELECT Abandoned,AccountId,AverageResponseTimeOperator,
                              AverageResponseTimeVisitor,Body,Browser,BrowserLanguage,
                              CaseId,ChatDuration,ChatKey, ContactId,CreatedById,
                              CreatedDate,EndedBy,EndTime,Id,IpAddress,IsChatbotSession,
                              LastModifiedById, LastModifiedDate, LiveChatButton.DeveloperName,
                              LiveChatDeployment.DeveloperName, LiveChatVisitorId,
                              Location,MaxResponseTimeOperator,MaxResponseTimeVisitor,
                              Name,OperatorMessageCount,OwnerId,Platform,ReferrerUri,
                              RequestTime,ScreenResolution,StartTime,Status,UserAgent,
                              VisitorMessageCount,VisitorNetwork,WaitTime
                              FROM LiveChatTranscript")

chat_transcript <- sf_query(soqlchat_tran, object_name="LiveChatTranscript", api_type="Bulk 1.0")
write.csv(chat_transcript, "chat_transcript.csv", row.names = FALSE)

# # Chat session #check for ContactId
soqlchat_act <- sprintf("SELECT ChatReqAssigned,ChatReqDeclined,ChatReqEngaged,ChatReqTimedOut,CreatedById,
                          CreatedDate,Id,LastModifiedById,LastModifiedDate,LoginTime,LogoutTime,Name,
                          TimeAtCapacity,TimeIdle,TimeInAwayStatus,TimeInChats,TimeInOnlineStatus
                          FROM LiveAgentSession")
# # # Chat session longer
# soqlchat_act <- sprintf("SELECT ChatReqAssigned,ChatReqDeclined,ChatReqEngaged,
#                           ChatReqTimedOut,CreatedById,CreatedDate,Id,
#                           LastModifiedById,LastModifiedDate,LoginTime,LogoutTime,
#                           Name,OwnerId,Owner_Role__c,TimeAtCapacity,TimeIdle,
#                           TimeInAwayStatus,TimeInChats,TimeInOnlineStatus 
#                           FROM LiveAgentSession")
chat_act <- sf_query(soqlchat_act, object_name="LiveAgentSession", api_type="Bulk 1.0")

write.csv(chat_act, "chat_act.csv", row.names = FALSE)
# # Chat visitor
soql_chatvisit <- sprintf("SELECT Id FROM LiveChatVisitor")
#
chat_visit <- sf_query(soql_chatvisit, object_name="LiveChatVisitor", api_type="Bulk 1.0")

write.csv(chat_visit, "chat_visit.csv", row.names = FALSE)

## try to load to student enrollment data #check for ContactId
# soql_enrollments <- sprintf("SELECT Academic_Honors__c,Academic_Plan_Requirements_Term__c,
#                            Academic_Standing__c,Admit_Term__c,Admit_Type__c,B_Deficit__c,
#                            Campus_Code__c,Campus__c,Career__c,Checkout_Status__c,Class_Standing__c,
#                            Contact__c,CreatedById,CreatedDate,Cumulative_GPA__c,Degree__c,
#                            Expected_Grad_Term__c,First_Generation__c,Honors_Student__c,Id,
#                            In_Progress_Units__c,IsDeleted,LastModifiedById,LastModifiedDate,
#                            LastReferencedDate,LastViewedDate,Location_Code__c,Location__c,Name,
#                            OwnerId,Plan__c,Primary_College_Code__c,Primary_College__c,Program_Status__c,
#                            Program__c,Residency__c,Student_Athlete__c,Student_Enrollment_Key__c,
#                            Subplan__c,SystemModstamp,Total_UA_only_Units_All__c,
#                            Transfer_Units_All__c FROM Student_Enrollment__c")
# 
# enrollments <- sf_query(soql_enrollments, object_name="Student_Enrollment__c", api_type="Bulk 1.0")
# 
# write.csv(enrollments, "enrollments2021.csv", row.names = FALSE)

soql_short_enroll <- sprintf("SELECT Academic_Plan_Requirements_Term__c,
                           Academic_Standing__c,Admit_Term__c,Admit_Type__c,
                           Campus_Code__c,Campus__c,Career__c,Class_Standing__c,
                           CreatedById,Contact__c, CreatedDate,Cumulative_GPA__c,Degree__c,
                           Expected_Grad_Term__c,First_Generation__c,Honors_Student__c,Id,
                           LastModifiedDate,Location_Code__c,
                           OwnerId,Plan__c,Primary_College_Code__c,Primary_College__c,
                           Program__c,Residency__c,Student_Athlete__c,Student_Enrollment_Key__c,
                           Subplan__c,Total_UA_only_Units_All__c,
                           Transfer_Units_All__c FROM Student_Enrollment__c")
short_enroll <- sf_query(soql_short_enroll, object_name="Student_Enrollment__c", api_type="Bulk 1.0")

write.csv(short_enroll, "short_enroll.csv", row.names = FALSE)

### LOCAL COMPUTER
## read in csv files for the 2021 data in local computer
# load in all data files
setwd("~/Documents/Trellis/ServiceDesk/Chat_Transcript2021/data")

chat2021 <- read.csv("chat_transcript.csv", header = TRUE, na.strings=c("","NA"))

LiveAgent <- read.csv("chat_act.csv", header = TRUE, na.strings=c("","NA"))

chatvisit <- read.csv("chat_visit.csv", header = TRUE, na.strings=c("","NA"))

short_enroll <- read.csv("active_enroll_did.csv", header = TRUE, na.strings=c("","NA"))

# short_enroll <- read.csv("short_enroll.csv", header = TRUE, na.strings=c("","NA"))


## add days of the week for chat 2021
date_value = chat2021$CreatedDate
day_of_the_week = date(date_value) %>%
  wday()
# day_of_the_week

chat2021 = dplyr::mutate(chat2021, day_of_the_week)
chat2021$day_of_the_week <- recode_factor(chat2021$day_of_the_week, 
                                      "1" = "Sunday", 
                                      "2" = "Monday",
                                      "3" = "Tuesday",
                                      "4" = "Wednesday",
                                      "5" = "Thursday",
                                      "6" = "Friday",
                                      "7" = "Saturday")

# check that the date time is in the right time zone
# ymd_hms("2020-10-08T18:06:08.000Z")
# table(chat2021$CreatedDate)
Date_Time = ymd_hms(chat2021$CreatedDate)
# head(Date_Time) #2020-07-02 21:56:42 UTC
chat2021 <- dplyr::mutate(chat2021, Date_Time)

# change to AZ time 
t1 <- as.POSIXct(Date_Time, tz = "GMT")
attributes(t1)$tzone
AZ_time <- lubridate::with_tz(t1, "MST")

chat2021 <- dplyr::mutate(chat2021, AZ_time)

# convert time to decimals 
tm1.dechr <- hour(chat2021$AZ_time) + minute(chat2021$AZ_time)/60 + second(chat2021$AZ_time)/3600
# tm1.dechr

chat2021 <- dplyr::mutate(chat2021, tm1.dechr) 

# evaluate peak times
library(plotly)
# create AZ date variable 
chat2021 <- chat2021 %>% 
  mutate(AZ_date = as.Date(AZ_time))

# histogram of live chats
p <- ggplot(chat2021, aes(x=AZ_date, fill = LiveChatDeployment.DeveloperName)) +
  geom_histogram(binwidth = 5) 

# https://www.r-bloggers.com/2018/06/customizing-time-and-date-scales-in-ggplot2/
p <- p + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
          date_labels = "%b-%y") + labs(title = "Histogram of Chats over Time", 
                                        subtitle = "full data")
p


# filter for each department
# 
# chat24seven <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="X24_7_Chat")
# 
# univserv <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="University_Services_Chat")
# 
# thinktank<- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="Think_Tank_Chat")
# 
# studyabroad <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="Study_Abroad_Chat")
# 
# sos <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="SOS_Chat")
# 
# secd <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="SECD_Chat")
# 
# registrar <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="Registrar_Chat")
# 
# psych <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="Psych_Dept_Chat")
# 
# OSFA <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="OSFA_Chat")
# 
# lifelab <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="LifeLab_Chat")
# 
# engineer <- dplyr::filter(chat2021, LiveChatDeployment.DeveloperName=="College_of_Engineering_Chat") 


### create one recode the lowest 7 into one variable called other, up to 45 variables
chat2021$DeveloperName = recode_factor(chat2021$LiveChatDeployment.DeveloperName, 
                                       "X24_7_Chat" = "24/7",
                                       "University_Services_Chat" = "Other",
                                       "Think_Tank_Chat" = "Other",
                                       "Study_Abroad_Chat" = "Other",
                                       "SOS_Chat" = "SOS",
                                       "SECD_Chat" = "Other",
                                       "Registrar_Chat" = "Registrar",
                                       "Psych_Dept_Chat" = "Other",
                                       "OSFA_Chat" = "OSFA", 
                                       "LifeLab_Chat" = "Other",
                                       "College_of_Engineering_Chat" = "Other")

# peak times with new Developer Categories
# hist(chat$AZ_date, breaks = 20)
p2 <- ggplot(chat2021, aes(x=AZ_date, fill = DeveloperName)) +
  geom_histogram(binwidth = 5) + 
  labs(x = "Date", y="Count")

p2 <- p2 + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                      date_labels = "%b-%y") + labs(title = "Histogram of Chats over Time", 
                                                    subtitle = "full data", fill = "Developer Name")
p2

### new dates, subset with new days
# are these breaks reasonable? 
summary(chat2021$AZ_time) # from June 1, 2020 to August 24, 2021

summer2020 <-chat2021[chat2021$AZ_time >= "2020-06-01" & chat2021$AZ_time <= "2020-08-06",] #summer 2020
peakfall2020 <- chat2021[chat2021$AZ_time >= "2020-08-07" & chat2021$AZ_time <= "2020-09-07",] # peak fall 2020
nonpeakfall2020 <- chat2021[chat2021$AZ_time >= "2020-09-08" & chat2021$AZ_time <= "2020-12-31",] # non peak fall 2020
peakspr2021 <-chat2021[chat2021$AZ_time >= "2021-01-01" & chat2021$AZ_time <= "2021-02-01",] # peak spring 2021
nonpeakspr2021 <-chat2021[chat2021$AZ_time >= "2021-03-01" & chat2021$AZ_time <= "2021-05-31",] # peak spring 2021
summer2021 <-chat2021[chat2021$AZ_time >= "2021-06-01" & chat2021$AZ_time <= "2021-08-06",] #summer 2021
peakfall2021 <- chat2021[chat2021$AZ_time >= "2021-08-07" & chat2021$AZ_time <= "2021-09-08",] # peak fall 2-21

# non_peak_chats <- chat2021[chat2021$AZ_time < "2020-08-17" | chat2021$AZ_time > "2020-09-04",] # none fall peak semesters

# non_peak_chats <- chat %>% 
#   filter(AZ_time < "2020-08-17" | AZ_time > "2020-09-04")

### overall generalizations 
# this works best after
overview_pct <- chat2021 %>% 
  group_by(DeveloperName) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

pct_graph <- 
  # drop_na(LiveChatDeployment.DeveloperName) %>% 
  ggplot(overview_pct, aes(DeveloperName, 
                           count, fill = DeveloperName)) +
  geom_bar(stat='identity') +
  labs(x = "Units", y = "Number of Chats") +
  coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph <- pct_graph + labs(title = "Number of Chats across Units", 
                              subtitle = "full data from 6/1/2020 to 9/8/2021",  fill = "Live Chat Developer")

print(pct_graph)

#filter out outliers
outliers <- boxplot(chat2021$ChatDuration)$out

#drop the rows containing outliers
chatD_out <- chat2021[-c(which(chat2021$ChatDuration %in% outliers)),] # this did work
# chatD_out <- chat_gpa[-c(which(chat_gpa$ChatDuration %in% outliers)),] # this did work

# box plot of chat duration 
# durationMinutes1 <- chatD_out$ChatDuration/60
# boxplot(durationMinutes1, main = "Chat Duration", ylab = "Minutes")

# filter outliers for the Wait time

outliers1 <- boxplot(chat2021$WaitTime)$out
chat_wait_out <- chat2021[-c(which(chat2021$WaitTime %in% outliers1)),] # this did work

# # for respondent wait time
# waitMinutes1 = chat_wait_out$WaitTime/60
# boxplot(waitMinutes1, main = "Respondent Wait Time", ylab = "Minutes")

# for abandoned chats
# filter outliers for the Abandoned
outliers2 <- boxplot(chat2021$Abandoned)$out

#drop the rows containing abandoned chat outliers
chatAb_out <- chat2021[-c(which(chat2021$Abandoned %in% outliers2)),] # this did work
# boxplot(chatAb_out$Abandoned, main = "Abandoned Chats", ylab = "Seconds")

# for chat time of day, no outliers were dropped
# boxplot(filter(chat$CreatedDate), main = "Chat Time of Day", ylab = "Hour")

outliers3 <- boxplot(chat2021$tm1.dechr)$out

CreatedDate_out <- chat2021[-c(which(chat2021$tm1.dechr %in% outliers3)),]
# drop the rows containing outliers
# boxplot(tm1.dechr, main = "Chat Time of Day", ylab = "Hour")

## graph of total chats across days of the week
DoW_graph <- chat2021 %>% 
  # dplyr::filter(LiveChatDeployment.DeveloperName == "24/7 Chat") %>% 
  pivot_longer(day_of_the_week, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = DeveloperName)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..)) +
  geom_boxplot() +
  labs(x = "", y = "") 

DoW_graph <- DoW_graph + labs(title = "Chat Deployment by Day of the Week",
                                subtitle = "ull data from 6/1/2020 to 9/8/2021", fill = "Live Chat Developer")

print(DoW_graph)
##
# new graph for time of day and day of the week
DoW_graph1 <- ggplot(chat2021, aes(x = day_of_the_week, y = tm1.dechr, 
                               fill = day_of_the_week)) +
  geom_boxplot() +
  ylim(0, 24) +
  labs(x="Day of the Week", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

DoW_graph1 <- DoW_graph1 + labs(title = "Chat Time of Day", subtitle = "full data from 6/1/2020 to 9/8/2021") +
  scale_y_continuous(breaks=seq(0,24,4))

print(DoW_graph1)

#plot with pct and day of the week
DoW_3 <- chat2021 %>% 
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
                               subtitle = "full data from 6/1/2020 to 9/8/2021",  fill = "Live Chat Developer")

print(DoW_graph3)
# box plots
# for total chat duration
# chats1 = dplyr::mutate(chatD_out, durationHour = chatD_out$ChatDuration/60)
Chat_Dur_graph <- ggplot(chatD_out, aes(x = DeveloperName, y = ChatDuration/60, fill = DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 35) +
  ylab("Chat Time in Minutes") +
  xlab("Unit Name")

Chat_Dur_graph <- Chat_Dur_graph + labs(title = "Unit Chat Duration", 
                                        subtitle = "outliers removed",  fill = "Live Chat Developer")
print(Chat_Dur_graph)

# for total wait time 
# chatD_out1 and waitMinutes1
waitMinutes1 = chat_wait_out$WaitTime/60
Wait_time_graph <- ggplot(chat_wait_out, aes(x = DeveloperName, y = waitMinutes1, fill = DeveloperName)) +
  geom_boxplot() +
  ylim(0, 4) +
  ylab("Wait Time in Minutes") +
  xlab("Unit Name")

Wait_time_graph <- Wait_time_graph + labs(title = "Unit Wait Time", 
                                          subtitle = "outliers removed",  fill = "Live Chat Developer")
print(Wait_time_graph)

# Part II
# overall chat duration without outliers
chatD_graph0 <- ggplot(chatD_out, aes(y = ChatDuration/60)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x = "Overall", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

chatD_graph0 <- chatD_graph0 + labs(title = "Chat Duration")
print(chatD_graph0)


# grouped by unit without outliers
chatD_graph <- ggplot(chatD_out, aes(x = DeveloperName, y = ChatDuration/60, fill = DeveloperName)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_graph <- chatD_graph + labs(title = "Chat Duration", subtitle = "outliers removed")

print(chatD_graph)
summary(chatD_out$ChatDuration)/60

# full data for chat duration
# chats = dplyr::mutate(chat2021, durationHour = chat$ChatDuration/60)
chatD_full <- ggplot(chat2021, aes(x = DeveloperName, y = WaitTime/60, fill = DeveloperName)) +
  geom_boxplot() +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_full <- chatD_full + labs(title = "Chat Duration", subtitle = "full data")
print(chatD_full)
summary(chat2021$ChatDuration)/60
# plot multiple graphs
library(patchwork)
library(ggpubr)
library(janitor)

chatD_both <- (chatD_full + chatD_graph) +    # Create grid of plots with title
  plot_annotation(title = "Comparing Full and Cleaned Data")

print(chatD_both)

### Wait Time
Wait_time_graph2a <- ggplot(chat_wait_out, aes(y = WaitTime/60)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Overall", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Wait_time_graph2a <- Wait_time_graph2a + labs(title = "Wait Time")
print(Wait_time_graph2a)

# grouped by unit
Wait_time_graph2 <- ggplot(chat_wait_out, aes(x = DeveloperName, y = WaitTime/60, fill = DeveloperName)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

Wait_time_graph2 <- Wait_time_graph2 + labs(title = "Wait Time", subtitle = "outliers removed")
print(Wait_time_graph2)

# full data wait time
Wait_time_graph1 <- ggplot(chat2021, aes(x = DeveloperName, y = WaitTime/60, fill = DeveloperName)) +
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
Chat_ab_graph <- ggplot(chatAb_out, aes(x = DeveloperName, y = Abandoned/60, fill = DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_graph <- Chat_ab_graph + labs(title = "Abandoned Chats", subtitle = "outliers removed")
print(Chat_ab_graph)

# full data for abandoned chats grouped by unit
Chat_ab_full <- ggplot(chat2021, aes(x = DeveloperName, y = Abandoned/60, fill = DeveloperName)) +
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
# for time of day
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
CreatedDate_graph <- ggplot(CreatedDate_out, aes(x = DeveloperName, y = tm1.dechr, 
                                                 fill = DeveloperName)) +
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
Unit_ToD <- ggplot(chat2021, aes(x = DeveloperName, 
                             y = tm1.dechr, fill = DeveloperName)) +
  geom_boxplot() +
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

# combine it into a graph
chatD_graph1 <- ggplot(chatD_out, aes(x = DeveloperName, y = ChatDuration/60, fill = DeveloperName)) +
  geom_boxplot() +
  ylim(0, 35) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

chatD_graph1 <- chatD_graph1 + labs(title = "Chat Duration")

print(chatD_graph1)
## wait time
Wait_time_graph2b <- ggplot(chat_wait_out, aes(x = DeveloperName, y = waitMinutes1, fill = DeveloperName)) +
  geom_boxplot() +
  ylim(0, 5) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position="none") 

Wait_time_graph2b <- Wait_time_graph2b + labs(title = "Wait Time")
print(Wait_time_graph2b)

# redo abandoned chat
Chat_ab_graph1 <- ggplot(chatAb_out, aes(x = DeveloperName, y = Abandoned/60, fill = DeveloperName)) +
  geom_boxplot() +
  # ylim(0, 800) +
  labs(x="Unit", y="Minutes") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

Chat_ab_graph1 <- Chat_ab_graph1 + labs(title = "Abandoned Chats")
print(Chat_ab_graph1)

# redo the Time of Day time scale
CreatedDate_graph1 <- ggplot(CreatedDate_out, aes(x = DeveloperName, y = tm1.dechr, 
                                                  fill = DeveloperName)) +
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

## 2d density map
mod5plot <- ggplot(chatD_out, aes(x=ChatDuration/60, y = WaitTime/60, fill = DeveloperName)) +
stat_density2d(aes(color=DeveloperName), size=.5) +
  # # geom_jitter()+
  # stat_smooth(method = "loess", se = FALSE)
  labs(x="Chat Duration (minutes)", y="Wait Time (seconds)") 

mod5plot <- mod5plot + labs(title = "2D Density Graphy of Chat Duration by Wait Time", 
                            subtitle = "outliers removed", 
                            fill = "Live Chat Developer") +
  # scale_y_continuous(breaks=seq(0,24,4)) +
  scale_color_discrete(name = "Live Chat Developer")
# scale_x_discrete(guide = guide_axis(n.dodge=2))

mod5plot

### tables by unit 
# wait time
chat2021 %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(WaitTime, na.rm=TRUE)) # full data

chat_wait_out %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(WaitTime, na.rm=TRUE)) # outlier removed

# chat Duration 
chat2021 %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(ChatDuration, na.rm=TRUE)/60) # full data

chat_wait_out %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(ChatDuration, na.rm=TRUE)/60) # outlier removed

# abandoned chats 
chat2021 %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(Abandoned, na.rm=TRUE)) # full data

chatAb_out %>%
  group_by(DeveloperName) %>%
  dplyr::summarize(Mean = mean(Abandoned, na.rm=TRUE)) # outlier removed

# chat time of day
CreatedDate_out %>%
  group_by(DeveloperName) %>%
  dplyr::summarise(Ave_hour = mean(tm1.dechr, na.rm = TRUE)) # outlier removed

