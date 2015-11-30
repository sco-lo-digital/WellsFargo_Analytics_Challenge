library(qdap)

#Use function polarity to characterize sentiment of hot topics
sentimentTopic2 <- polarity(topic2)
sentimentTopic3 <- polarity(topic3)
sentimentTopic6 <- polarity(topic6)
sentimentTopic12 <-polarity(topic12)
sentimentTopic19 <-polarity(topic19)
sentimentAll <- polarity(alltopics)

sentimentDf <- rbind(sentimentAll$group$stan.mean.polarity,
                     sentimentTopic2$group$stan.mean.polarity,
                     sentimentTopic3$group$stan.mean.polarity,
                     sentimentTopic6$group$stan.mean.polarity,
                     sentimentTopic12$group$stan.mean.polarity,
                     sentimentTopic19$group$stan.mean.polarity)
rownames(sentimentDf)=c("All Topics",
                        "Topic 2",
                        "Topic 3",
                        "Topic 6",
                        "Topic 12",
                        "Topic 19")
colnames(sentimentDf)="Standardized Polarity"


topic2SentimentLines<-htruncdf(counts(sentimentTopic2), 50, 10)#Sample output
plot(sentimentTopic2) #Appendix plots
plot(sentimentTopic3)
plot(sentimentTopic6)
plot(sentimentTopic12)
plot(sentimentTopic19)

####Topic 2
banka_topic2<-topic2[grepl('banka',topic2)]
bankb_topic2<-topic2[grepl('bankb',topic2)]
bankc_topic2<-topic2[grepl('bankc',topic2)]
bankd_topic2<-topic2[grepl('bankd',topic2)]
#Topic 2 polarity by bank name
polarBaT2 <- polarity(banka_topic2)
polarBbT2 <- polarity(bankb_topic2)
polarBcT2<- polarity(bankc_topic2)
polarBdT2<- polarity(bankd_topic2)
polarT2Df <- rbind(polarBaT2$group$stan.mean.polarity, 
                   polarBbT2$group$stan.mean.polarity, 
                   polarBcT2$group$stan.mean.polarity, 
                   polarBdT2$group$stan.mean.polarity)
rownames(polarT2Df)=c("Bank A", "Bank B", "Bank C", "Bank D")
colnames(polarT2Df)="Standardized Polarity T2"

####Topic 3
banka_topic3<-topic3[grepl('banka',topic3)]
bankb_topic3<-topic3[grepl('bankb',topic3)]
bankc_topic3<-topic3[grepl('bankc',topic3)]
bankd_topic3<-topic3[grepl('bankd',topic3)]
#Topic 3 polarity by bank name
polarity(banka_topic3)
polarity(bankb_topic3)
polarity(bankc_topic3)
polarity(bankd_topic3)


#create corpus data structure for use with tm package
topic2Corpus <- Corpus(VectorSource(topic2))
topic3Corpus <- Corpus(VectorSource(topic3))
topic19Corpus <- Corpus(VectorSource(topic19))
#create document term matrix
topic2Tdm <- TermDocumentMatrix(topic2Corpus, control = list(minWordLength = 1))
topic3Tdm <- TermDocumentMatrix(topic3Corpus, control = list(minWordLength = 1))
topic19Tdm <- TermDocumentMatrix(topic19Corpus, control = list(minWordLength = 1))
#Remove Sparse terms
topic2TdmS <- removeSparseTerms(topic2Tdm, sparse = 0.97)
topic3TdmS <- removeSparseTerms(topic3Tdm, sparse = 0.97)
topic19TdmS <- removeSparseTerms(topic19Tdm, sparse = 0.97)
##########################Find freq terms #######################
freq.termsT2<-findFreqTerms(topic2TdmS, lowfreq=10)
term.freqT2<-rowSums(as.matrix(topic2TdmS))
term.freqT2<-subset(term.freqT2, term.freqT2>=20)
dfT2<-data.frame(terms=names(term.freqT2),freq=term.freqT2)


# ########################Create Dendogram###############
#coerce TDM to matrix
m.T2<- as.matrix(topic2TdmS)
#cluster terms
distMatrixT2 <- dist(scale(m.T2))
dendofitT2 <- hclust(distMatrixT2, method = "ward.D")
plot(dendofitT2)
rect.hclust(dendofitT2, k=6) #cut tree into 6 clusters (arbitrary)
##########################################################
#Find correlations between bank names and words in topic 2
findbanka_topic2<-findAssocs(topic2Tdm, 'banka', 0.10)
findbankb_topic2<-findAssocs(topic2Tdm, 'bankb', 0.10)
findbankc_topic2<-findAssocs(topic2Tdm, 'bankc', 0.10)
findbankd_topic2<-findAssocs(topic2Tdm, 'bankd', 0.10)

# ########################Create Dendogram###############
#coerce TDM to matrix
m.T3<- as.matrix(topic3TdmS)
#cluster terms
distMatrixT3 <- dist(scale(m.T3))
dendofitT3 <- hclust(distMatrixT3, method = "ward.D2")
plot(dendofitT3)
rect.hclust(dendofitT3, k=10) #cut tree into 10 clusters (arbitrary)


#Find correlations between bank names and words in topic 3
findbanka_topic3<-findAssocs(topic3Tdm, 'banka', 0.10)
findbankb_topic3<-findAssocs(topic3Tdm, 'bankb', 0.10)
findbankc_topic3<-findAssocs(topic3Tdm, 'bankc', 0.10)
findbankd_topic3<-findAssocs(topic3Tdm, 'bankd', 0.10)


# ########################Create Dendogram###############
#coerce TDM to matrix
m.T19<- as.matrix(topic19TdmS)
#cluster terms
distMatrixT19 <- dist(scale(m.T19))
dendofitT19 <- hclust(distMatrixT19, method = "ward.D2")
plot(dendofitT19)
rect.hclust(dendofitT19, k=4) #cut tree into 4 clusters (arbitrary)


#Find correlations between bank names and words in topic 19
findbanka_topic19<-findAssocs(topic19Tdm, 'banka', 0.15)
findbankb_topic19<-findAssocs(topic19Tdm, 'bankb', 0.15)
findbankc_topic19<-findAssocs(topic19Tdm, 'bankc', 0.15)
findbankd_topic19<-findAssocs(topic19Tdm, 'bankd', 0.15)
