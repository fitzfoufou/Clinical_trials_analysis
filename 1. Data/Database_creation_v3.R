
install.packages("XML")
library("XML")
library("methods")
install.packages("gtools")
library(gtools)
library(tools)

#1- Building temporary files of 2 trials
setwd("C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test")
Vect<-seq(1,length(list.files()),2)
Vect[length(Vect)]<-length(list.files())

for (i in seq(1,length(Vect)-1)){
  for (j in seq(Vect[i]+1,Vect[i+1])){
        #To make sure the XML is retrieved well
        xmltemp=xmlRoot(xmlTreeParse(list.files()[j]))
        #To retrieve the data from the XML and put it as a dataframe
        dataset_j <- unlist(xmlSApply(xmltemp, function(x) xmlSApply(x, xmlValue)), recursive=FALSE)
        dataset_j <- data.frame(t(dataset_j),row.names=NULL)
        #To bind the line with the agregated dataset
        if (j==Vect[i]+1){
          dataset_i<-dataset_j
        } else{
          dataset_i<-smartbind(dataset_i, dataset_j)
        }
  }
  save(dataset_i,
       file=paste("C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test - output/Phase3_",i,".Rda",sep=""))
}

#2- Aggregate the temporary files together
setwd("C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test - output")
#dataset<-get(load("C:/Users/ffitzpatrick/Documents/R/Clinical trials/Temp files/Phase0_1.Rda"))
details = file.info(list.files(pattern="*.Rda"))
details = details[with(details, order(as.POSIXct(mtime))), ]
dataset<-data.frame()
for (filetemp in rownames(details)){
  datatemp<-get(load(filetemp))
  datatemp$name<-filetemp
  dataset<-smartbind(dataset,datatemp)
}


#3- Check that all clinical trials have been added to the database
common_trials<-Reduce(intersect, list(file_path_sans_ext(list.files(path ="C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test")),dataset$id_info.nct_id))
diff_trials<-setdiff(file_path_sans_ext(list.files(path ="C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test")),dataset$id_info.nct_id)
duplicate_trials<-dataset[duplicated(dataset$id_info.nct_id),]$id_info.nct_id

length(common_trials)
diff_trials
duplicate_trials



test<-get(load("C:/Users/ffitzpatrick/Desktop/Documents perso/Perso/Clinical trials - project/1. Data/0. Raw data/Test - output/Phase3_10.Rda"))
