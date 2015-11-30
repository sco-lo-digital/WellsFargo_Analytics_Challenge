#install.packages("doParallel")

library(tm)
library(ggplot2)
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)
library(wordcloud)
library(doParallel)


registerDoParallel(cores=2)
##################read in the data frame created in PreProc_Wells.R##########
wb <- readRDS("wells_banks.rds")
wb <- wb$FullText
wb_noBanks <-wb
wb_noBanks <- gsub("banka","",wb_noBanks, ignore.case=F)
wb_noBanks <- gsub("bankb","",wb_noBanks, ignore.case=F)
wb_noBanks <- gsub("bankc","",wb_noBanks, ignore.case=F)
wb_noBanks <- gsub("bankd","",wb_noBanks, ignore.case=F)
wb_noBanks <- gsub("^[[:space:]]+", "", wb_noBanks ) # remove whitespace at beginning of documents
wb_noBanks <- gsub("[[:space:]]+$", "", wb_noBanks ) # remove whitespace at end of documents

###############################Exploratory Analysis############################
#create corpus data structure for use with tm package
myCorpus <- Corpus(VectorSource(wb))
myCorpus_noBanks <- Corpus(VectorSource(wb_noBanks))
#create document term matrix
myTdm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
myTdm_noBanks <- TermDocumentMatrix(myCorpus_noBanks, control = list(minWordLength = 1))

#######################Word Association###############
# which words are associated with "____" with lower correlation of .10?
#twitter is associated with "checkmark" and "communicating"
findtwitter<-findAssocs(myTdm, "twitter", 0.15)
#bankb has something about a stadium
findbankb<-findAssocs(myTdm, 'bankb', 0.10)
#nothing comes up
findbankc<-findAssocs(myTdm, 'bankc', 0.10)
#some interesting associations
findbankd<-findAssocs(myTdm, 'bankd', 0.10)
#banka associated with contest and apparently a hashtag #getcollegeready
findbanka<-findAssocs(myTdm, 'banka', 0.10)
###########################################
#Explore a tdm with no bank names
tdm.noBanks.96 <- removeSparseTerms(myTdm_noBanks, sparse = 0.96)
freq.terms.noBanks<-findFreqTerms(tdm.noBanks.96, lowfreq=50)
m.96.noBanks <-as.matrix(tdm.noBanks.96)
#coerce to matrix
m <- m.96.noBanks
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
#create data frame
d <- data.frame(word=myNames, freq=v)
#change colors, 8 colors in palette, palette name Dark2
pal <- brewer.pal(8, "Dark2")
pal <- pal[-(1:2)]
#create wordcould from this raw data
rawcloud<-wordcloud(d$word, d$freq, scale=c(4,.5),min.freq=50,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
rawcloud
##########################################

#remove sparse terms at varying levels, explore differences
#sparsity = .99 then word must occur at least .01*# of records in order to keep it
tdm.98 <- removeSparseTerms(myTdm, sparse = 0.98)
tdm.95 <- removeSparseTerms(myTdm, sparse = 0.95)
tdm.85 <- removeSparseTerms(myTdm, sparse = 0.85)
tdm.75 <- removeSparseTerms(myTdm, sparse = 0.75)

m.98 <- as.matrix(tdm.98)
m.95 <- as.matrix(tdm.95)
m.85 <- as.matrix(tdm.85)
##########################Find freq terms #######################
freq.terms<-findFreqTerms(tdm.95, lowfreq=100)
term.freq<-rowSums(as.matrix(tdm.98))
term.freq<-subset(term.freq, term.freq>=200)
df<-data.frame(terms=names(term.freq),freq=term.freq)
########################Create frequency plot #############
ggplot(df, aes(x = reorder(terms,-freq), y = freq)) + geom_bar(aes(fill=terms), stat = "identity") + 
    xlab("Terms") + ylab("Count") + coord_flip()+ guides(fill=FALSE)
########################Create association plot ###########
#plot tdm with no bank names
plot(myTdm_noBanks, term = freq.terms.noBanks, corThreshold = .1, weighting = T)#requires Rgraphviz
dev.off(dev.list()["RStudioGD"])
#plot tdm with banks names
plot(myTdm, term= freq.terms, corThreshold = .08, weighting = T)
dev.off(dev.list()["RStudioGD"])

# ########################Create Dendogram###############
# 
#cluster terms
distMatrix <- dist(scale(m.98))
dendofit <- hclust(distMatrix, method = "ward")
plot(dendofit)
rect.hclust(dendofit, k=6) #cut tree into 6 clusters

##########################################################



save.image("~/Data_Science/WellsAnalytics.RData")