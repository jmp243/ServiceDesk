# from Frances on August 26, 2021
###SF Data deidentification for Research Assistant use in Fall 2020
  ###will include data from 1 Jan 2020 through 15 August 2020
    ###objects included are 
# contacts
# cases
# availability
# appointments
# enrollments
# key indicators
# Chat Transcript
# Chat Session
# Chat Visitor
# 

# Functions to assist with master data management (MDM) or data integrity of records by finding duplicates
# (sf_find_duplicates(), sf_find_duplicates_by_id()), merging records (sf_merge()), and converting leads (sf_convert_lead())r

# install.packages("devtools")
# install.packages("httpuv")
# 
# install.packages("salesforcer")
# install.packages("tidyselect")
# install.packages("tidyverse")
# install.packages("Rtools")
# install.packages("dplyr")
# suppressWarnings(suppressMessages(library(dplyr)))
# install.packages("RForcecom")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("writexl")
# 
# 
# library(RForcecom)
# 
# rm(list=ls(all=TRUE)) 
# options(digits=3)
# 
# today <- Sys.Date()
# format(today, format="%B %d %Y")
# 
library(dplyr, warn.conflicts = FALSE)
# library(salesforcer)
# 
# # Using OAuth 2.0 authentication
# sf_auth()
# 
# # and confirm a connection to the APIs
# user_info <- sf_user_info()
# print(user_info)
# 
# ############### - WHAT FIELDS ARE on the objects of interest IN SALESFORCE?
# #########
# #####
# ##bring list of all objects and fields
# sf_fields_prod<-sf_describe_object_fields("Contact")
# names(sf_fields_prod)
# 
# my_soql_avail <- sprintf("SELECT	Id, Appointment_Types__c,	CreatedBy.Name,	CreatedById, CreatedDate, Date__c, Duration__c, Number_of_Appointments__c, Meeting_Type__c, 
#                           RecordType.Name, Start_Time__c, End_Time__c 
#                     FROM Availability__c
#                          WHERE CreatedDate>2019-07-02T14:00:00.000Z")
# ###############
# #########
# #####
# ###bring in Contact Records
# my_soql_contact <- sprintf("SELECT	Id,	hed__Ethnicity__c,	hed__Gender__c, Emplid__c, Birthdate, hed_Financial_Aid_Applicant__c, hed_Race__c, MDM_Primary_Type__c
#                     FROM Contact")
# 
# contact_records <- sf_query(my_soql_contact, object_name="Contact", api_type="Bulk 1.0")
# 
# 
# my_soql_appointments <- sprintf("SELECT	Id, Advisor_Email__c,	Advisor_Name__c,	Appointment_Date__c,	Appointment_Details__c,	Appointment_Sequence_Number__c,
#                                 Appointment_End_Time__c, Appointment_Start_Time__c, Duration__c, Appointment_Status__c,	Appointment_Type__c, RecordType.Name,
#                               Contact__r.Id,	Meeting_Type__c, Contact__r.Emplid__c, Availability__c, CreatedBy.name, CreatedById,CreatedDate, Contact__r.NetID__c, Appointment_CPP_Info__c
#                        FROM Appointment__c
#                                  WHERE CreatedDate>2019-07-02T14:00:00.000Z")
# 
# appt_records_all <- sf_query(my_soql_appointments, object_name="Appointment__c", api_type="Bulk 1.0")
# 
# ###bring in Cases - working on getting Appointment__c to work.
# 
# my_soql_cases <- sprintf("SELECT Id, Appointment__c, OwnerId, Reason, ContactEmail, ContactId, CreatedbyId, CreatedDate, Description,
#                           Interaction_Date__C, LastModifiedById, Note_Types__C, Status, Subject, Type, Case_Category__c,Career__c, RecordType.Name,
#                           hed__Category__c, College__c, Created_Day_of_Week__c, Department__c, Department_Approval_Date__c, Subcategory__c,
 #                           CaseNumber, Origin, Approval_Status_c, Action__c, hed__Category__c, Reason,College__c,
#Instructor_Approval_DateTime__c, Type, Term__c
#                          FROM Case
#                          WHERE Subject (NOT Subject  like '%Archive:%' )")
# case_records <- sf_query(my_soql_cases, object_name="Case", api_type="Bulk 1.0")
# 
# case_records %>% group_by(College__c) %>% tally
# 
# my_soql_epr <- sprintf("SELECT	Id, Acknowledged__c,	Additional_Comments__c,	Content_Block_1_Rich__c,	Content_Block_2_Rich__c,	Course_Id__c,	Course_Name__c,	Course_Section__c,	CreatedDate, 
#                         CreatedById,	Name,	Feedback_Type_1__c,Feedback_Type_2__c,	Instructor_Name__c,	Instructor_NetId__c,	
#                         Last_Acknowledged_Date__c,	LastModifiedById,	OwnerId, Specific_Feedback_Type_1__c,
#                         Specific_Feedback_Type_2__c,	Student__c,	Student_Email__c,	Student_First_Name__c,	Student_Last_Name__c,	Student_Name__c
#                     FROM Early_Progress_Reports_Student__c")
# 
# queried_epr_records <- sf_query(my_soql_epr, object_name="Early_Progress_Reports_Student__c", api_type="Bulk 1.0")
# 
# # enrollments
# soql_stu_enr <- sprintf("SELECT	Academic_Plan_Requirements_Term__c,	Academic_Standing__c,	Admit_Term__c,	Admit_Type__c,	B_Deficit__c,	Campus__c,	Campus_Code__c,	Career__c,
#                          Checkout_Status__c,	Class_Standing__c,	Contact__c,	CreatedById, Cumulative_GPA__c,	Degree__c,	Expected_Grad_Term__c,	Honors_Student__c,
#                          In_Progress_Units__c,	LastModifiedById,	LastModifiedDate, Location__c,	Location_Code__c,	Name,	OwnerId,	Plan__c,	Plan__r.Name, Primary_College__c,	Primary_College_Code__c,
#                          Program__c,	Program__r.Name, Program_Status__c,	Residency__c,	Student_Athlete__c,	Student_Enrollment_Key__c,	Subplan__r.Name,	Total_UA_only_Units_All__c,	Transfer_Units_All__c
#                     FROM Student_Enrollment__c")
# 
# stu_enr <- sf_query(soql_stu_enr, object_name="Student_Enrollment__c", api_type="Bulk 1.0")
# 
# # key indicators
# soql_ki <- sprintf("SELECT Contact__c,CreatedById,CreatedDate,Enrolled_Units_Flag__c,Id,LastModifiedDate,Recent_GPA_Drop_Flag__c,Withdrawn_Course_Flag__c 
#                           FROM Key_Indicators__c")
# 
# key_ind <- sf_query(soql_ki, object_name="Key_Indicators__c", api_type="Bulk 1.0")

# # Chat Transcript
# soqlchat_tran <- sprintf("SELECT Abandoned,AccountId,AverageResponseTimeOperator,AverageResponseTimeVisitor,Body,Browser,BrowserLanguage,
#                               CaseId,ChatDuration,ChatKey,
#                               ContactId,CreatedById,CreatedDate,EndedBy,EndTime,Id,IpAddress,IsChatbotSession,LastModifiedById,LastModifiedDate,
#                                 LiveChatButton.DeveloperName,LiveChatDeployment.DeveloperName,
#                               LiveChatVisitorId,Location,MaxResponseTimeOperator,MaxResponseTimeVisitor,Name,
#                               OperatorMessageCount,OwnerId,Platform,ReferrerUri,RequestTime,
#                               ScreenResolution,StartTime,Status,UserAgent,VisitorMessageCount,VisitorNetwork,WaitTime
# FROM LiveChatTranscript")
# 
# chat_transcript <- sf_query(soqlchat_tran, object_name="LiveChatTranscript", api_type="Bulk 1.0")
# # Chat session
# soqlchat_act <- sprintf("SELECT ChatReqAssigned,ChatReqDeclined,ChatReqEngaged,ChatReqTimedOut,CreatedById,CreatedDate,Id,LastModifiedById,LastModifiedDate,LoginTime,LogoutTime,Name,
#                          TimeAtCapacity,TimeIdle,TimeInAwayStatus,TimeInChats,TimeInOnlineStatus 
#                          FROM LiveAgentSession")
# 
# chat_act <- sf_query(soqlchat_act, object_name="LiveAgentSession", api_type="Bulk 1.0")
# # Chat visitor
# soql_chatvisit <- sprintf("SELECT Id FROM LiveChatVisitor")
# 
# chat_visit <- sf_query(soql_chatvisit, object_name="LiveChatVisitor", api_type="Bulk 1.0")

#########################################
#######################
#import csv files to deidentify

setwd("U:/WorkSpaces/fkm_data/SF to deidentify")

filenames <- list.files(path=getwd())  
numfiles <- length(filenames) 

for (i in filenames) {  
  name <- gsub("-",".",i)
  name <- gsub(".csv","",name)  
  i <- paste(".\\",i,sep="")
  assign(name,read.csv(i, header=TRUE))
}

# STUDENT ENROLLMENT DATA
studentenrollments <-read.csv("studentenrollments.csv")
# # 
# ADVNetids$studentid<-as.character(ADVNetids$studentid)
# # ###for Brad
# names(appointments)
# appts_bh<-merge(appointments, ADVNetids, by.x = 'Contact__r.Emplid__c', by.y = 'studentid', all.y =  TRUE)
# 
# names(appts_bh)
# appts_bh<-appts_bh[,c(1,5, 10:13, 16)]
# # 
# # ##keep only attended appointments
#  appts_bh<-subset(appts_bh, (appts_bh$Appointment_Status__c == "Attended") | (appts_bh$Appointment_Status__c == "Scheduled") | (appts_bh$Appointment_Status__c == "Checked In"))
#  appts_bh<-subset(appts_bh, appts_bh$Appointment_Date__c<as.Date("2020-08-30") & appts_bh$Appointment_Date__c>as.Date("2019-08-01"))
#  
# appts_bh$counter<-1
# names(appts_bh)
# 
# appt_count_byemplid<-appts_bh %>% group_by(Contact__r.Emplid__c) %>% tally()
# appt_count_byappttopic<-appts_bh %>% group_by(Contact__r.Emplid__c, Appointment_Type__c) %>% tally()
# appt_count_byappttype<-appts_bh %>% group_by(Contact__r.Emplid__c, RecordType.Name) %>% tally()
# appt_count_bymeetingtype<-appts_bh %>% group_by(Contact__r.Emplid__c, Meeting_Type__c) %>% tally()
# 
# library(writexl)
# sheets <- list("appt_count_byemplid" = appt_count_byemplid, "appt_count_byappttopic" = appt_count_byappttopic,
#                "appt_count_byappttype" = appt_count_byappttype, "appt_count_bymeetingtype" = appt_count_bymeetingtype) #assume sheet1 and sheet2 are data frames
# write_xlsx(sheets, "U:/WorkSpaces/fkm_data/SF to deidentify/AppointmentCounts.xlsx")
# 
# #########
# ####
# ##Cases Plus Appts for Brad Hensley
# names(cases)
# emplid_contactId<-contacts[,c(1, 8)]
# emplid_contactId$Emplid__cnum<-as.numeric(emplid_contactId$Emplid__c)
#     appointments_appcase<-subset(appointments, appointments$Appointment_Date__c<as.Date("2020-08-30") & appointments$Appointment_Date__c>as.Date("2019-08-01"))
#     appointments_appcase<-subset(appointments_appcase, 
#     (appointments_appcase$Appointment_Status__c == "Attended") | (appointments_appcase$Appointment_Status__c == "Scheduled") | (appointments_appcase$Appointment_Status__c == "Checked In"))
#     cases_appcase<-subset(cases, cases$Interaction_Date__c<as.Date("2020-08-30") & cases$Interaction_Date__c>as.Date("2019-08-01"))
#     cases_appcase<-subset(cases_appcase, cases_appcase$RecordType.Name=="Note")
#     
#     cases_appcase<-merge(cases_appcase, emplid_contactId, by.x = "ContactId", by.y = "Id", all.x = TRUE)
#     appointments_appcase<-merge(appointments_appcase, emplid_contactId, by.x = "Contact__r.Id", by.y = "Id", all.x = TRUE)
#     
#     appt_case<-merge(appointments_appcase, cases_appcase, by.x="Id", by.y = "Appointment__c", all = TRUE)
#     names(appt_case)
#     ac_emp<-subset(appt_case, appt_case$Contact__r.Emplid__c!=appt_case$Emplid__c.y)
#     ac_emp2<-subset(appt_case, appt_case$Emplid__c.x!=appt_case$Emplid__c.y)
#     
#     appt_case$Emplid__c.x<-as.numeric(appt_case$Emplid__c.x)
#     ac_emp3<-subset(appt_case, is.na(appt_case$Emplid__cnum.x))
#     ac_emp4<-subset(appt_case, is.na(appt_case$Emplid__cnum.y))
#     appt_case$Emplid__cnum.x[is.na(appt_case$Emplid__cnum.x)]<-appt_case$Emplid__cnum.y
#     
#     appt_case<-merge(appt_case, ADVNetids, by.x = "Emplid__cnum.x", by.y = "studentid", all.y = TRUE )
# 
# names(appt_case)
# appt_case %>% group_by(RecordType.Name.y) %>% tally()
# length(unique(appt_case$Id))
# 
# apptcase_count_byemplid<-appt_case %>% group_by(Emplid__cnum.x) %>% tally()
# apptcase_count_byappttopic<-appt_case %>% group_by(Emplid__cnum.x, Appointment_Type__c) %>% tally()
# apptcase_count_byappttype<-appt_case %>% group_by(Emplid__cnum.x, RecordType.Name.y) %>% tally()
# apptcase_count_bymeetingtype<-appt_case %>% group_by(Emplid__cnum.x, Meeting_Type__c) %>% tally()
# 
# library(writexl)
# sheets <- list("apptcase_count_byemplid" = apptcase_count_byemplid,
#                "apptcase_count_byappttype" = apptcase_count_byappttype) #assume sheet1 and sheet2 are data frames
# write_xlsx(sheets, "U:/WorkSpaces/fkm_data/SF to deidentify/AppointmentPlusNotesCounts.xlsx")

###############
#################
##deidentfy all files
#get out all the unique ids
# names(appointments) ##netid, contactid, emplid, advisoremail
# names(availabilities) ##CreatedbyId
# names(cases) ## CreatedById, ContactId
# names(contacts) ## Id, Emplid__c
# names(ChatSessionTranscript) ## ContactId, Chat_Net_Id
# names(EPRs) ##Student__c
# names(keyindicators) ## Contact__c
# names(LiveAgentSession) ## CreatedById
# names(LiveChatVisitor) ## Id
# names(studentenrollments) ## Id
# 
# ##contacts
(unique_ids <- unique(contacts$Id))

(num_of_unique_ids <- length(unique_ids))
#for each unique id assign a random integer
# set seed for reproducibility
set.seed(42)
(rand_ids<-sample.int(n = 10^10, #many possible integers
                      size=num_of_unique_ids,
                      replace=FALSE))

(ids_lookups <- data.frame(Id=unique_ids,
                           new_ID=rand_ids))

contacts_did <- merge(contacts, ids_lookups, by.x = "Id", by.y = "Id")

names(contacts_did)
contacts_did<-contacts_did[-c(1,8)] # this could be problematic based on how I set up the dataset.

###didentify other files using this new random id

##appointments
appointments_did <- merge(appointments,
                      ids_lookups, by.x = "Contact__r.Id", by.y = "Id")
names(appointments_did)
appointments_did<-appointments_did[-c(13:15, 18,19)]

##cases
cases_did <- merge(cases,
                          ids_lookups, by.x = "ContactId", by.y = "Id")
names(cases_did)
cases_did<-cases_did[-c(1,6)]

##EPRs
eprs_did <- merge(EPRs,
                   ids_lookups, by.x = "Student__c", by.y = "Id")
names(eprs_did)
eprs_did<-eprs_did[-c(22:25, 16, 15, 1,11)]
##chat sessions
chattran_did <- merge(ChatTranscriptv2,
                  ids_lookups, by.x = "ContactId", by.y = "Id")
names(chattran_did)
chattran_did<-chattran_did[-c(1,3,12)]

##key indicators
ki_did <- merge(keyindicators,
                      ids_lookups, by.x = "Contact__c", by.y = "Id")
names(ki_did)
ki_did<-ki_did[-c(1,5)]

##student enrollments
stuen_did <- merge(studentenrollments,
                ids_lookups, by.x = "Contact__c", by.y = "Id")
names(stuen_did)
stuen_did<-stuen_did[-c(1)]

################################
######################
##write to csv
write.csv(contacts_did, "contacts_did.csv")
write.csv(cases_did, "cases_did.csv")
write.csv(appointments_did, "appointments_did.csv")
write.csv(eprs_did, "eprs_did.csv")
write.csv(chattran_did, "chattran_did.csv")
write.csv(ki_did, "keyindicators_did.csv")
write.csv(stuen_did, "studentenrollments_did.csv")