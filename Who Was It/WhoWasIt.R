# initialize required libraries

libs <- c("tm","plyr","class")
lapply(libs,require,character.only=TRUE)

# Set options
options(stringsAsFactors = FALSE)

# Set parameters
pathname <- "D:/twitterChallange/corpus"
candidates <- c("DonaldTrump","HillaryClinton")

# Pre-processing the text
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus,removePunctuation)
  corpus.tmp <- tm_map(corpus,stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus,removeWords, stopwords("english"))
  
  return(corpus.tmp)
}

# Build Matrix to do the Math

generateTDM <- function(cand,path){
  s.dir <- sprintf("%s/%s",path,cand)
  s.cor <- Corpus(DirSource(directory = s.dir), readerControl = list(reader=readPlain))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm,0.7)
  
  result<-list(name=cand, tdm=s.tdm)
}

tdm<-lapply(candidates,generateTDM, path=pathname)

# Attach name of the candidate with Matrix

bindCandidateTOTDM <- function(tdm){
  s.mat <-t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat,stringsASFactors= FALSE)
  
  s.df <- cbind(s.df, rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "candidatename"
  return(s.df)
}

candTDM <- lapply(tdm, bindCandidateTOTDM)

# Take Matrix and stack on top of each other
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0

# cross-validation
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)*0.67))
test.idx <- (1:nrow(tdm.stack)) [- train.idx]

# model-you can use Naive Bayes/SVM/Random Forest/KNN/logistic regression
tdm.cand <- tdm.stack[,"candidatename"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "candidatename"]

knn.pred <- knn(tdm.stack.nl[train.idx,], tdm.stack.nl[test.idx,], tdm.cand[train.idx])


# Checking accuracy of our model

conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
(accuracy <- sum(diag(conf.mat)) /length(test.idx)*100)