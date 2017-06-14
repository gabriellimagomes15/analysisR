## Author: Gabriel Lima Gomes - 06/2017
## R file with differents libraries and Functions to make analysis and text mining about

library(Rfacebook)
library(data.table)
library(tm)
library(tidytext)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(knitr)
library(gridExtra)

#### GLOBAL VARIABLES ###
pal = brewer.pal(8, "Dark2") ##palete of colors


###########################################################################################################################
#### ---- FUNCTIONS ---- ####

## Function to read comments in CSV FILE
readComments <- function(urlPath, clear = FALSE){
  comments <- fread(urlPath)
  comments <- comments %>% 
                select(message) ## SELECT JUST COLUMN MESSAGE
  names(comments) <- "text"    ## RENAME COLUMN
  if(clear){
    commentsClear <- cleanComments(comments) ## CLEAR MESSAGES
    return(commentsClear)
  }
  return(comments)
}


## Function to do cluster of the words. Data can be DataFrame
clusterWords <- function(data){
  corpus   <- Corpus(VectorSource(data))
  tdm      <- TermDocumentMatrix(corpus)
  tdm 	   <- removeSparseTerms(tdm, sparse = 0.99)
  df       <- as.data.frame(inspect(tdm))
  dfScale  <- scale(df)
  dist	   <- dist(dfScale, method = "euclidean")
  fitWard2 <- hclust(dist, method = "ward.D2")
  return(fitWard2)
}

## Function to do the words graph. Data need be CHAR ARRAY. 
## This function will return a list with graph and layout of the graph 
graphWords <- function(data, numCluster){
  corpus <- Corpus(VectorSource(data))
  tdm    <- TermDocumentMatrix(corpus)
  matrix    <-  as.matrix(tdm)
  wordCount   <- rowSums(matrix)
  lim  <- quantile(wordCount, probs=0.95)
  good <- matrix[wordCount > lim,]
  good <- good[,colSums(good)!=0]
  
  # adjacency matrix
  adjMatrix <- good %*% t(good)
  
  # set zeroes in diagonal
  diag(adjMatrix) <- 0
  
  # graph adjacency
  graphAdj <- graph.adjacency(adjMatrix, weighted=TRUE, mode="undirected",
                              add.rownames=TRUE)
  # layout of the graph
  glay <- layout.fruchterman.reingold(graphAdj)
  
  # let's superimpose a cluster structure with k-means clustering
  kmg <- kmeans(adjMatrix, centers = numCluster)
  gk  <- kmg$cluster
  
  # create nice colors for each cluster
  gbrew <- c("red", brewer.pal(8, "Dark2"))
  gpal  <- rgb2hsv(col2rgb(gbrew))
  gcols <- rep("", length(gk))
  
  for (k in 1:numCluster) {
    gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
  }
  
  # Prepare variables for plot
  V(graphAdj)$size   <- 10
  V(graphAdj)$label  <- V(graphAdj)$name
  V(graphAdj)$degree <- degree(graphAdj)
  V(graphAdj)$label.color <- hsv(0, 0, 0.2, 0.55)
  V(graphAdj)$frame.color <- NA
  V(graphAdj)$color <- gcols
  E(graphAdj)$color <- hsv(0, 0, 0.7, 0.3)
  
  return(list(graph = graphAdj, label = glay))
}

## Function to clear comments. Need be a data frame
cleanComments <- function(data, stopWords = ""){
  if(class(data)[1] == "list"){
    commentsDF = do.call("rbind", lapply(data, as.data.frame));
  }else{
    commentsDF <- data
  }
  commentsDF = subset(commentsDF, select = c(text));
  
  # Tweet Cleasing
  commentsDF$text = gsub('http.* *', '', commentsDF$text);
  commentsDF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  commentsDF$text);
  commentsDF$text = gsub(":", "", commentsDF$text);
  # remove at people
  commentsDF$text = gsub("@\\w+", "", commentsDF$text)
  # remove punctuation
  commentsDF$text = gsub("[[:punct:]]", "", commentsDF$text)
  # remove numbers
  commentsDF$text = gsub("[[:digit:]]", "", commentsDF$text)
  # remove html links
  commentsDF$text = gsub("http\\w+", "", commentsDF$text)
  # remove not word
  commentsDF$text <- gsub("\\W"," ",commentsDF$text)
  # remove unnecessary spaces
  commentsDF$text = gsub("[ \t]{2,}", "", commentsDF$text)
  commentsDF$text = gsub("^\\s+|\\s+$", "", commentsDF$text)
  commentsDF$text = tolower(commentsDF$text)
  commentsDF$text = sapply(commentsDF$text, function(x) removeWords(x,c(stopWords,stopwords("pt"))))
  
  # Removing Duplicate tweets and Removing null line
  commentsDF[,"DuplicateFlag"] = duplicated(commentsDF$text);
  commentsDF = subset(commentsDF, commentsDF$DuplicateFlag =="FALSE" & trimws(commentsDF$text) != "");
  commentsDF = subset(commentsDF, select = -c(DuplicateFlag))
  
  return(commentsDF)
}


## Function to acess API Sentiments Analytics Microsoft AZRE
sentimentalAPIMs <- function(commentsDF){ ## commentsDF need be a Data Frame
  Microsoft_API_Key <- 'YOUR API KEY';
  
  # Creating the request body for Text Analytics API
  commentsDF["language"] = "pt";
  commentsDF["id"] = seq.int(nrow(commentsDF));
  request_body_twitter = commentsDF[commentsDF$text != "", c(2,3,1)];
  
  # Converting tweets dataframe into JSON
  request_body_json_twitter = jsonlite::toJSON(list(documents = request_body_twitter));
  
  # Calling text analytics API
  result_twitter_sentimental = POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment", 
                                    body = request_body_json_twitter, 
                                    add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"= Microsoft_API_Key)))
  
  Output_Sentimental = httr::content(result_twitter_sentimental);
  attach(Output_Sentimental);
  rows <- length(documents);
  score_twitter = data.frame(matrix(unlist(Output_Sentimental), nrow=rows, byrow=T));
  
  score_twitter$X1 =  as.numeric(as.character(score_twitter$X1));
  names(score_twitter)[names(score_twitter) == 'X1'] <- 'Score';
  names(score_twitter)[names(score_twitter) == 'X2'] <- 'Id';
  request_body_twitter$Score <- c(score_twitter$Score);
  detach(Output_Sentimental);
  rm(score_twitter);
  
  return(request_body_twitter)
}

## Function to do cluster of the words
clusterDend <- function(data){
  corpus <- Corpus(VectorSource(data))
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, sparse = 0.10)
  df  <- as.data.frame(inspect(tdm))
  df.scale <- scale(df)
  d <- dist(df.scale, method = "euclidean")
  fit.ward2 <- hclust(d, method = "ward.D2")
  return(fit.ward2)
}

## Function to count the number of the tokens
countTokens <- function(data, numberToken = 1){
  cTokens <- data %>%
    unnest_tokens(word, text, token = "ngrams", n = numberToken) %>% ## COUNT TOKEN. PACKAGE TIDYTEX
    count(word, sort = TRUE)
  return(cTokens)
}

## FUNCTION TO DO PLOT
plotCountTokens <- function(data, team = ""){
  plot <- ggplot(data[1:10,], aes(reorder(word,n), n)) +
    geom_bar(stat = "identity", fill='#FCCB85', width=0.5) +
    labs(y=NULL, x=NULL, title=paste('Time: ',team, sep='')) +
    coord_flip() + titles.format +
    theme(plot.title = element_text(size=13),
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          plot.background = element_rect(fill = "white"))
  return(plot)
}


## FUNCTION TO DESCRIBE SCORE OF SENTIMENTALS
descSentimental <- function(sentiments){
  ## Percent score posit sentiment
  sentPosit <- sentiments %>%
    group_by(time) %>%
    mutate(total = n()) %>%
    filter(Score > 0.7) %>%
    mutate(freq  = round(  (n()/total) * 100,3) ) %>%
    group_by(time,freq) %>%
    summarise()%>%
    mutate(sentimento = "Positivo")
  
  ## Percent score negative sentiment
  sentNeg <- sentiments %>%
    group_by(time) %>%
    mutate(total = n()) %>%
    filter(Score <  0.3) %>%
    mutate(freq  = round(  (n()/total) * 100,3) ) %>%
    group_by(time,freq) %>%
    summarise()%>%
    mutate(sentimento = "Negativo")
  
  ## Percent score neutral sentiment
  sentNeut <- sentiments %>%
    group_by(time) %>%
    mutate(total = n()) %>%
    filter(Score > 0.3 & Score < 0.7) %>%
    mutate(freq  = round( (n()/total) * 100,3) ) %>%
    group_by(time,freq) %>%
    summarise() %>%
    mutate(sentimento = "Neutro")
  
  ## Merge percent sentimentals
  sentPercent <- rbind(sentNeg, sentPosit, sentNeut)
}