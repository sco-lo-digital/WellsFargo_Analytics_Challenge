library(lda)
library(LDAvis)

#load clean data frame object from PreProc_Wells.R
wells_banks<-readRDS("wells_banks.rds")

#LDA Model Creation
#Load stop words
stop_words <- c(stopwords("SMART"), "")
# tokenize on space and output as a list:
doc.list <- strsplit(wells_banks$FullText, "[[:space:]]+")
# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (5,000)
W <- length(vocab)  # number of terms in the vocab (1,411)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data (40,135)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 

# MCMC and model tuning parameters:
K <- 25 #Number of topics
G <- 3000 #Number of iterations
alpha <- 0.02
eta <- 0.02

# Fit the lda model:
set.seed(357)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
#Model takes a while to compute, saveRDS to serialize
saveRDS(fit, "LDA_Fit.rds")
#Load object back into environment
fit<-readRDS("LDA_Fit.rds")


#Ref https://cran.r-project.org/web/packages/lda/lda.pdf
sums<-fit$document_sums
kvmatrix<-fit$topics
#create a table of the top 20 words in each topic
top.topic.words(kvmatrix, num.words=20,by.score=F)
#create predictions for the first 8 documents
predictions<-predictive.distribution(sums[,1:8],kvmatrix,.1,.1)
## Use top.topic.words to show the top 20 predictions in each document.
top.topic.words(t(predictions), 20)

#ldaVis
#define parameters for list/json
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
#create list for json
bankNotes <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)



# create the JSON object to feed the visualization:
json <- createJSON(phi = bankNotes$phi, 
                   theta = bankNotes$theta, 
                   doc.length = bankNotes$doc.length, 
                   vocab = bankNotes$vocab, 
                   term.frequency = bankNotes$term.frequency)

#output JSON to screen.
#select pop-out from the 'Viewer' window to open in default web browser
serVis(json, out.dir = 'vis', open.browser = T)
