library(ggplot2)
library(plyr)
library(shinydashboard)
library(shiny)
library(dplyr)
library(data.table)
library(DT)

setwd("C:/Users/Francois/Desktop/Fitz/2. Entrepreneurial projects/1. Clinical trials/1. Data")

#Reformat initial dataset
CT=read.csv("CT_clean.csv",na.strings=c("","NA"))
CT$firstreceived_date<-as.Date(CT$firstreceived_date)
CT$completion_date<-as.Date(CT$completion_date)
CT$lastchanged_date<-as.Date(CT$lastchanged_date)
CT$primary_completion_date<-as.Date(CT$primary_completion_date)
CT$start_date<-as.Date(CT$start_date)

#Building Duration variable
CT$Beg_date<-if_else(CT$start_date<CT$firstreceived_date,CT$start_date,CT$firstreceived_date)
CT$End_date<-if_else(is.na(CT$completion_date),CT$primary_completion_date,CT$completion_date)
#CT$Duration<-difftime(CT$End_date ,CT$Beg_date , units = c("days"))
CT$Duration<-as.Date(CT$End_date,format="%Y/%m/%d") - as.Date(CT$Beg_date,format="%Y/%m/%d")
length(CTsimple$Duration)

View(CT[c(120:144)])

head(CT$Duration,100)
tail(sort(CT$Duration),100)

#Creating a smaller database
CTsimple=CT[which(CT$firstreceived_date >"2008-01-01" & CT$enrollment < 1000000),]
CTsimple=subset(CTsimple, !is.na(enrollment))
CTsimple=subset(CTsimple, !is.na(Duration))
setDT(CTsimple)

#Test graphs
ggplot(CTsimple, aes(x=enrollment))+ 
  geom_histogram(binwidth=100,fill="white", color="black")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(enrollment)), color="orange", linetype="dashed", size=1)


ggplot(CTsimple, aes(x=Duration))+ 
  geom_histogram(binwidth=365,fill="white", color="black")+
  theme_classic()+
  geom_vline(xintercept=mean(CTsimple$Duration), color="orange", linetype="dashed", size=1)

round(mean(CTsimple$Duration, na.rm=TRUE))

CTsimple2 = CTsimple[sample(nrow(CTsimple), 1000), ]
CTsimple2 = CTsimple[sample(nrow(CTsimple), 1000),c("id_info.nct_id","condition.text","enrollment.text","eligibility.criteria","eligibility.gender")]
show_vars=c("id_info.nct_id","condition.text","enrollment.text","eligibility.criteria","eligibility.gender")
colnames(CTsimple2)
DT::datatable(CTsimple2[show_vars])

#Getting the variables' values
head(CTsimple$study_type.text,100)
levels(CTsimple$firstreceived_results_date.text)

head(CTsimple[which(CTsimple$last_known_status.text=="Recruiting"),])
c(1,6)[2]

head(CTsimple[grep("Phase 1", CTsimple$phase.text),])

#Others
colnames(CTsimple)
mean(CT$Duration, na.rm=TRUE)
sum(is.na(CTsimple$firstreceived_results_date.text))

class(CT$enrollment)





