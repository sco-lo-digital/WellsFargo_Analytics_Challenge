#install.packages(c("servr","lda","LDAvis","devtools", "tm", "rjson", "bit64", "httr","rJava","RWekajars","RWeka","SnowballC","wordcloud","stm"))
#Point your working directory to the location of the wells text file
setwd("~/Data_Science")#change this to your own
library(dplyr)
library(tm)

#Read in the txt document
wells <- read.delim("WellsText.txt", header=T, sep="|", stringsAsFactors=F)
#Eplore the object
str(wells)

#Filter it so we only keep records about Banks A, B, C, and D 
wells_banks <- filter(wells, grepl('BankB|BankA|BankC|BankD',wells$FullText))

#Use as.Date on Date metadata 
wells_banks$Date<-as.Date(wells_banks$Date, format="%m/%d/%Y")

#Filter out replacement words
wells_banks$FullText <- gsub("ADDRESS"," ",wells_banks$FullText, ignore.case=F)
wells_banks$FullText <- gsub("Name"," ", wells_banks$FullText, ignore.case=F)
wells_banks$FullText <- gsub("INTERNET"," ", wells_banks$FullText, ignore.case=F)
wells_banks$FullText <- gsub("twit_hndl"," ", wells_banks$FullText, ignore.case=F)
wells_banks$FullText <- gsub("PHONE"," ", wells_banks$FullText, ignore.case=F)
wells_banks$FullText  <- gsub("dlvr", "", wells_banks$FullText ) # remove dlvr

#Pre-Processing
wells_banks$FullText <-  gsub('[[:digit:]]+', '', wells_banks$FullText) #remove numbers
wells_banks$FullText <- gsub("http\\w+","", wells_banks$FullText ) #remove links
wells_banks$FullText <- gsub("'", "", wells_banks$FullText )  # remove apostrophes
wells_banks$FullText <- gsub("/", "", wells_banks$FullText )  # remove backslash
wells_banks$FullText <- gsub("[[:punct:]]", "", wells_banks$FullText )  # remove punctuation 
wells_banks$FullText <- gsub("[[:cntrl:]]", "", wells_banks$FullText )  # remove control characters 
wells_banks$FullText <- gsub("^[[:space:]]+", "", wells_banks$FullText ) # remove whitespace at beginning of documents
wells_banks$FullText <- gsub("[[:space:]]+$", "", wells_banks$FullText ) # remove whitespace at end of documents
wells_banks$FullText <- tolower(wells_banks$FullText )  # force to lowercase

#need to remove stop words
wells_banks$FullText <- removeWords(wells_banks$FullText, stopwords("english"))
#explore object
str(wells_banks)
head(wells_banks)

#wells_banks is a data frame now with a column of clean text, called FullText
saveRDS(wells_banks, "wells_banks.rds")