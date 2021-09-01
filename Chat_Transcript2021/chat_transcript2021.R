#SET UP Environment
library('tidyverse')
library('dplyr')
library('readxl')
library('salesforcer')
# library('RForcecom')
library('lubridate')
library('httpuv')

#setwd("~/Trellis Users KPIs")
setwd("U:/WorkSpaces/jmpark_data")

options(digits=3)

sf_auth()

soqlchat_tran <- sprintf("SELECT Abandoned,AccountId,AverageResponseTimeOperator,AverageResponseTimeVisitor,Body,Browser,BrowserLanguage,
                               CaseId,ChatDuration,ChatKey,
                               ContactId,CreatedById,CreatedDate,EndedBy,EndTime,Id,IpAddress,IsChatbotSession,LastModifiedById,LastModifiedDate,
                                 LiveChatButton.DeveloperName,LiveChatDeployment.DeveloperName,
                               LiveChatVisitorId,Location,MaxResponseTimeOperator,MaxResponseTimeVisitor,Name,
                               OperatorMessageCount,OwnerId,Platform,ReferrerUri,RequestTime,
                               ScreenResolution,StartTime,Status,UserAgent,VisitorMessageCount,VisitorNetwork,WaitTime
 FROM LiveChatTranscript")

chat_transcript <- sf_query(soqlchat_tran, object_name="LiveChatTranscript", api_type="Bulk 1.0")
write.csv(chat_transcript, "chat_transcript.csv", row.names = FALSE)

# # Chat session
soqlchat_act <- sprintf("SELECT ChatReqAssigned,ChatReqDeclined,ChatReqEngaged,ChatReqTimedOut,CreatedById,CreatedDate,Id,LastModifiedById,LastModifiedDate,LoginTime,LogoutTime,Name,
                          TimeAtCapacity,TimeIdle,TimeInAwayStatus,TimeInChats,TimeInOnlineStatus
                          FROM LiveAgentSession")

chat_act <- sf_query(soqlchat_act, object_name="LiveAgentSession", api_type="Bulk 1.0")

write.csv(chat_act, "chat_act.csv", row.names = FALSE)
# # Chat visitor
soql_chatvisit <- sprintf("SELECT Id FROM LiveChatVisitor")
#
chat_visit <- sf_query(soql_chatvisit, object_name="LiveChatVisitor", api_type="Bulk 1.0")

write.csv(chat_visit, "chat_visit.csv", row.names = FALSE)
