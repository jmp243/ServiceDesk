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
# 
left_enroll <- left_join(chat2021, short_enroll, by = "new_ID") # created a ton of duplicates
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



# graphs distinguishing students
left_graph <- chat_gpa %>%
  # mutate(Q7 = fct_infreq(Q7)) %>%
  drop_na(Students) %>% 
  ggplot(aes(x = DeveloperName, fill = Students, response = Students)) +
  geom_bar() +
  # coord_flip() +
  labs(x = "", y = "")

left_graph <- left_graph + labs(title = "",
                                subtitle = "undergraduate and graduate students")

print(left_graph)

## count up the NA's
sum(is.na(chat_gpa$Academic_Standing__c)) # 4224

# try new graphs with the data
# scatter plot
ggplot(chat_gpa, aes(x=WaitTime/60, y=ChatDuration/60, color=class_standing)) + 
  geom_point(size=6) 