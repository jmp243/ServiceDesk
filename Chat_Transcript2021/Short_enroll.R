# jung mee park
# examine enrollment data 
# August 25, 2021

library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lubridate)
library(forcats)

# load in the data
setwd("~/Documents/Trellis/ServiceDesk/Chat_Transcript2021/data")

# short_enroll <- read.csv("short_enroll.csv", header = TRUE, na.strings=c("","NA"))

short_enroll <- read.csv("active_enroll_did.csv", header = TRUE, na.strings=c("","NA")) #data file with new_ID

### create a new variable for Grad and Undergrad called students
short_enroll$Students = recode_factor(short_enroll$Career__c, 
                                       "Correspondence" = "Undergraduate",
                                       "Graduate" = "Graduate",
                                       "Law" = "Graduate",
                                       "Medical School" = "Graduate",
                                       "Pharmacy" = "Graduate",
                                       "Professional" = "Graduate",
                                       "Veterinary Medicine" = "Graduate",
                                       "Undergraduate" = "Undergraduate")

# class standing
short_enroll$class_standing = recode_factor(short_enroll$Class_Standing__c, 
                                      "Freshman" = "Freshman",
                                      "Sophomore" = "Sophomore",
                                      "Junior" = "Junior",
                                      "Senior" = "Senior",
                                      "Graduate" = "Graduate",
                                      "Doctoral" = "Graduate",
                                      "Masters" = "Graduate",
                                      "Prof 1" = "Graduate",
                                      "Prof 2" = "Graduate",
                                      "Prof 3" = "Graduate",
                                      "Prof 4" = "Graduate"
                                      )

# rename contact__c to ContactId
# enrollcontact <- short_enroll %>% 
#   rename(ContactId = Contact__c)  

# # attempt to merge files
# https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti

# merged_data <- merge(x=short_enroll, y=chat2021, by.x="Contact__c", by.y="ContactId", x.all=FALSE, y.all=FALSE) 
# 
# inner_enroll <- inner_join(chat2021, enrollcontact, by = "ContactId") #created a few duplicate rows
chattran <- read.csv("chattran_did.csv", header = TRUE, na.strings=c("","NA"))

# recode developer name
chattran$DeveloperName = recode_factor(chattran$LiveChatDeployment.DeveloperName, 
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

## add days of the week for chat 2021
date_value3 = chattran$CreatedDate
day_of_the_week3 = date(date_value3) %>%
  wday()
# day_of_the_week

chattran = dplyr::mutate(chattran, day_of_the_week3)
chattran$day_of_the_week <- recode_factor(chattran$day_of_the_week, 
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
Date_Time3 = ymd_hms(chattran$CreatedDate)
# head(Date_Time) #2020-07-02 21:56:42 UTC
chattran <- dplyr::mutate(chattran, Date_Time3)

# change to AZ time 
t3 <- as.POSIXct(Date_Time3, tz = "GMT")
attributes(t3)$tzone
AZ_time3 <- lubridate::with_tz(t3, "MST")

chattran <- dplyr::mutate(chattran, AZ_time3)

# convert time to decimals 
tm3.dechr <- hour(chattran$AZ_time) + minute(chattran$AZ_time)/60 + second(chattran$AZ_time)/3600
# tm1.dechr

chattran <- dplyr::mutate(chattran, tm3.dechr) 
# left join to preserve chat data
left_enroll <- left_join(chattran, short_enroll, by = "new_ID") # created a ton of duplicates
# 
# semi_enroll <- semi_join(chat2021, enrollcontact, by = "ContactId") # created a ton of duplicates
# 
# write.csv(inner_enroll, "inner_enroll.csv", row.names = FALSE)
# 
write.csv(left_enroll, "left_enroll.csv", row.names = FALSE)
## table for X.y
# summary(left_enroll$X.y) # eliminate all NA possibly
# 
# summary(left_enroll$Cumulative_GPA__c) # same number of NA as X.y


# GPA data
chat_gpa <- left_enroll %>% 
  drop_na(Cumulative_GPA__c)

write.csv(chat_gpa, "chat_gpa.csv", row.names = FALSE)

### look for more areas to cut out
# table(chat_gpa$Degree__c, chat_gpa$DeveloperName)

# degree_chat <- chat_gpa %>% 
#   drop_na(Degree__c) #too many cuts
#


# graphs distinguishing students
left_graph <- left_enroll %>%
  # mutate(Q7 = fct_infreq(Q7)) %>%
  drop_na(Students) %>% 
  ggplot(aes(x = DeveloperName, fill = Students, response = Students)) +
  geom_bar() +
  coord_flip() +
  labs(x = "", y = "")

left_graph <- left_graph + labs(title = "Chat Usage by Students",
                                subtitle = "undergraduate and graduate students")

print(left_graph)

## count up the NA's
sum(is.na(chat_gpa$Academic_Standing__c)) # 4570

# make tables
library(dplyr)
library(knitr)
library(DT)
library(xtable)
library(tibble)

# exploring the data
# colMeans(chat_gpa[sapply(chat_gpa, is.numeric)])
# sapply(chat_gpa, class)

left_enroll_table <- left_enroll %>% 
  select(Abandoned, ChatDuration, MaxResponseTimeOperator, MaxResponseTimeVisitor, 
         OperatorMessageCount,WaitTime, tm3.dechr, Cumulative_GPA__c)

colMeans(left_enroll_table, na.rm=TRUE)
# for summary tables

left_enroll_table <- left_enroll %>% 
  select(Abandoned, ChatDuration, MaxResponseTimeOperator, MaxResponseTimeVisitor, 
         OperatorMessageCount,WaitTime, tm3.dechr, Cumulative_GPA__c)

d.summary.extended <-  left_enroll_table <- left_enroll %>% 
  select(Abandoned, ChatDuration, MaxResponseTimeOperator, MaxResponseTimeVisitor, 
         OperatorMessageCount,WaitTime, tm3.dechr, Cumulative_GPA__c) %>% 
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble() %>%
  rownames_to_column() %>%
  print()

dim(left_enroll_table)

# install.packages("stargazer")
library(stargazer)
# cols <- c('Abandoned', 'ChatDuration', 'MaxResponseTimeOperator', 'MaxResponseTimeVisitor', 
#           'OperatorMessageCount', 'WaitTime', 'tm3.dechr', 'Cumulative_GPA__c')
# stargazer(
#   left_enroll[, cols], type = "text", 
#   summary.stat = c("min", "p25", "median", "p75", "max", "median", "sd")
# )

# try new graphs with the data
# scatter plot
ggplot(chat_gpa, aes(x=WaitTime/60, y=ChatDuration/60, color=class_standing)) + 
  geom_point(size=6) 

# pie charts with plotly
# annotations
# a <- list(
#   text = "SUBPLOT TITLE A",
#   xref = "paper",
#   yref = "paper",
#   yanchor = "bottom",
#   xanchor = "center",
#   align = "center",
#   x = 0.5,
#   y = 1,
#   showarrow = FALSE
# )
# 
# b <- list(
#   text = "SUBPLOT TITLE B",
#   xref = "paper",
#   yref = "paper",
#   yanchor = "bottom",
#   xanchor = "center",
#   align = "center",
#   x = 0.5,
#   y = 1,
#   showarrow = FALSE
# )

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(short_enroll, class_standing), labels = ~class_standing, 
                       values = ~n, name = "enrollment", domain = list(x = c(0, 0.4), 
                                                                       y = c(0.4, 1)))
fig <- fig %>% add_pie(data = count(chat_gpa, class_standing), labels = ~class_standing, 
                       values = ~n, name = "chat", domain = list(x = c(0.6, 1), y = c(0.4, 1)))

fig <- fig %>% layout(title = "Class Standing by Enrollment and Chat Usage", showlegend = T,
                      grid=list(rows=0, columns=1),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(y = 0.5),
                      annotations = list(x = c(.08, .62, .08, .62),
                                         # y = c(.78, .78, .22, .22),
                                         text = c("Enrollment","Chat Usage"),
                                         # xref = "papper",
                                         yref = "papper",
                                         showarrow = F))

fig

# ##### NLP
# library(gutenbergr)
# library(tidytext)
# library(stringr) 
# library(tidyr)   
# # install.packages("wordcloud")
# library(wordcloud)
# library(hunspell)
# library(SnowballC)
# library(xtable)
# library(knitr)
# library(kableExtra)
# 
# # preprocessing data
# cleaned_text <- chat2021 %>%
#   filter(str_detect(text_review, "^[^>]+[A-Za-z\\d]") | text_review !="") 
