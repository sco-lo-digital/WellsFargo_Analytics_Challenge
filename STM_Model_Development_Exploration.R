#STM
#structural topic model creation
library(stm)
library(stmBrowser)

#devtools::install_github("timelyportfolio/stmBrowser@htmlwidget")
devtools::install_github("timelyportfolio/stmCorrViz@htmlwidget")
#install.packages("htmlwidgets")
library(htmlwidgets)

#load clean data frame object from PreProc_Wells.R
wells_banks<-readRDS("wells_banks.rds")

#begin STM processing
processed<-textProcessor(wells_banks$FullText, metadata= wells_banks)
out<-prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocabs <- out$vocab
meta <-out$meta
#obtain the lower threshold by visually investigating effect of trshld on docs, words, tokens
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(docs, vocabs, meta, lower.thresh = 15)
#fit the stm
PrevFit <- stm(docs, vocabs, K = 20,  prevalence =~ MediaType, max.em.its = 75, data = meta, init.type = "Spectral")
#Establish Optimum Number of topics
set.seed(02138)
storage <- searchK(out$documents, out$vocab, K = c(10, 20, 30), data=meta, ngroups=6)
#Compare visually the use of different numbers of topics
plot(storage)
dev.off()
#Appears that 20 topics is best number based time constraints to build model vs quality of model
#create basket of 4 models based on K=20
Select <- selectModel(out$documents, out$vocab, K = 20, prevalence =~ MediaType, max.em.its = 75, data = out$meta, runs = 20, seed = 8458159)
#objects took a long time to compute, save them
saveRDS(PrevFit, "PrevFit.rds")
saveRDS(Select, "Select.rds")
#Plot created models to explore Semantic Coherence
plotModels(Select)
#Not totally clear based on plot, but Model 3 appears to be the best model on these measures
selectmodel<-Select$runout[[3]]
#Plot topics in the model
plot.STM(selectmodel,type = "labels", n=10)
dev.off()
#Plot Summary of most frequent topics
plot.STM(selectmodel,type = "summary")
dev.off()
#Topic 5 is most pervasive. Look at top records fr Topic 5
findThoughts(selectmodel, texts=out$meta$FullText, topics=5, n=5)$docs[[1]]
#Topic 5 appears to be about financials stock trading news
#Topic 7 is second most pervasive.
findThoughts(selectmodel, texts=out$meta$FullText, topics=7, n=5)$docs[[1]]
#Appears to be a marketing related topic with the hashtage getcollegeready

#Visually explore how topics are clustered 
stmCorrViz_htmlwidget(selectmodel,"stmCorrViz.html",out$meta$FullText, out$documents, title="STM")
scv<-stmCorrViz_htmlwidget(
    stmJSON(selectmodel, documents_raw=out$meta$FullText, 
            documents_matrix=out$documents,verbose=TRUE))
scv
library(htmltools)
browsable(tagList(scv,scv))
#Visually explore topic relations
stmBrowser_widget(selectmodel, data=out$meta, c("MediaType","Month"), text="FullText",n=5000)

#Visually explore words in topics
toLDAvis(mod=selectmodel, docs=out$documents)



#Explore all 20 topics in the model
labelTopics(selectmodel, c( seq(1:20)))
#This shows us that potential customer service topics are 2,3,and perhaps 19


#findTopic() - i identified a bug in the package. function not exported. use following
stm:::findTopic(selectmodel, "banka", verbose=T)
#banka is present in topic 7, investigate topic 7
findThoughts(selectmodel, texts=out$meta$FullText, topics=7, n=5)$docs[[1]]
alltopics <- out$meta$FullText
##
stm:::findTopic(selectmodel, "bankb", verbose=T)
#banka is present in topic 15, investigate topic 15
findThoughts(selectmodel, texts=out$meta$FullText, topics=15, n=5)$docs[[1]]
##
stm:::findTopic(selectmodel, "bankc", verbose=T)
findThoughts(selectmodel, texts=out$meta$FullText, topics=5, n=5)$docs[[1]]
stm:::findTopic(selectmodel, "bankd", verbose=T)
#bankd shows up in topics 5, 10, 16, and  17
findThoughts(selectmodel, texts=out$meta$FullText, topics=5, n=5)$docs[[1]]
findThoughts(selectmodel, texts=out$meta$FullText, topics=10, n=5)$docs[[1]]
findThoughts(selectmodel, texts=out$meta$FullText, topics=16, n=5)$docs[[1]]
findThoughts(selectmodel, texts=out$meta$FullText, topics=17, n=5)$docs[[1]]

####"Customer Service / "Hot" Topics ####
topic2<-findThoughts(selectmodel, texts=out$meta$FullText, topics=2, n=1000)$docs[[1]]
topic3<-findThoughts(selectmodel, texts=out$meta$FullText, topics=3, n=1000)$docs[[1]]
topic6<-findThoughts(selectmodel, texts=out$meta$FullText, topics=6, n=1000)$docs[[1]]
topic12<-findThoughts(selectmodel, texts=out$meta$FullText, topics=12, n=1000)$docs[[1]]
topic19<-findThoughts(selectmodel, texts=out$meta$FullText, topics=19, n=1000)$docs[[1]]

#create a vector of counts for each bank in each topic
banksTopic2<-c(length(grep("banka",topic2)), length(grep("bankb",topic2)), length(grep("bankc",topic2)), length(grep("bankd",topic2)))
banksTopic3<-c(length(grep("banka",topic3)), length(grep("bankb",topic3)), length(grep("bankc",topic3)), length(grep("bankd",topic3)))
banksTopic6<-c(length(grep("banka",topic6)), length(grep("bankb",topic6)), length(grep("bankc",topic6)), length(grep("bankd",topic6)))
banksTopic12<-c(length(grep("banka",topic12)), length(grep("bankb",topic12)), length(grep("bankc",topic12)), length(grep("bankd",topic12)))
banksTopic19<-c(length(grep("banka",topic19)), length(grep("bankb",topic19)), length(grep("bankc",topic19)), length(grep("bankd",topic19)))

rnames<-c("Topic 2: Branch Cust Serv", "Topic 3: Overall Cust Serv", "Topic 6: ATM Fees", "Topic 12:Transaction Fees", "Topic 19:Rants cursing out Banks")
topicDF<-rbind(banksTopic2, banksTopic3, banksTopic6, banksTopic12, banksTopic19)
topicDF<-as.data.frame(rbind(banksTopic2, banksTopic3, banksTopic6, banksTopic12, banksTopic19),row.names=rnames)
colnames(topicDF)=c("Bank A", "Bank B", "Bank C", "Bank D")
save.image("~/Data_Science/WellsAnalytics/BackupWells.RData")


###Appendix###
#plot wordcloud
cloud(selectmodel, topic=9, scale = c(3,.5))
dev.off()
#Plot pervasiveness
plot.STM(selectmodel,type = "hist", topics=c(seq(1:10)))
dev.off()
plot.STM(selectmodel,type = "hist", topics=c(seq(11,20, by=1)))
#Correlations
tcorr<-topicCorr(selectmodel)
plot(tcorr, main="Topic Correlation Graph",vertex.color = "blue")