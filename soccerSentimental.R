# Author: GABRIEL LIMA GOMES - 06/2017
# Script to make analysis about comments on the Facebook of the soccer team of Rio de Janeiro - Brazil

library(Rfacebook)
library(data.table)
library(tm)
library(wordcloud)
library(dplyr)
library(plotly)
library(ggplot2)
###########################################################################################################################
#### ---- FUNCTIONS ---- ####

## Function to do corpus
corpus <- function(df){
  corpus <- Corpus(VectorSource(df))
  (latin <- content_transformer(function(x) iconv(x, to = "latin1", sub = "byte")))
  corpus <- tm_map(corpus, latin)
  
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, sparse = 0.10)
  df  <- as.data.frame(inspect(tdm))
  
  df.scale <- scale(df)
  d <- dist(df.scale, method = "euclidean")
  return(d)
}


## Function to clear commentes. Need be a data frame
cleanComments <- function(data){
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
  commentsDF$text = sapply(commentsDF$text, function(x) removeWords(x,c("pra",stopwords("pt"))))
  
  # Removing Duplicate tweets and Removing null line
  commentsDF[,"DuplicateFlag"] = duplicated(commentsDF$text);
  commentsDF = subset(commentsDF, commentsDF$DuplicateFlag =="FALSE" & trimws(commentsDF$text) != "");
  commentsDF = subset(commentsDF, select = -c(DuplicateFlag))
  
  return(commentsDF)
}


## Function to acess API Sentiments Analytics Microsoft AZRE
sentimentalAPIMs <- function(commentsDF){ ## commentsDF is in Json
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
###########################################################################################################################
###########################################################################################################################





## Authentication Facebook Developer
appId  = "YOUR APP ID"
appSec = "YOUR APP SECURITY"
fb_oauth  <- fbOAuth(app_id = appId, app_secret= appSec,extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

## Temporary Authentication Facebook Developer
token = "YOUR TEMPORY TOKEN"


## Get posts and comments of the Fluminense's page
fluPage    <- getPage(page="FluminenseFC", token=token, since = "2017-05-07", until = "2017-05-08") ##FINAL CARIOCA
post       <- getPost(post = fluPage$id[2], n = 1000, token = token) ## SELECT POST ESPECIFC
fluComents <- post$comments ## GET COMMENT OF THE POST

fluPage <- getPage(page="FluminenseFC", token=token, since = "2017-05-31", until = "2017-06-02") ##COPA BRASIL
post    <- getPost(post = fluPage$id[9], n = 1000, token = token)
fluComents <- rbind(fluComents,post$comments) ## MERGE BETWEEN POSTS

fluMessag  <- fluComents %>% 
                select(message) ## SELECT JUST COLUMN MESSAGE
names(fluMessag) <- "text"    ## RENAME COLUMN
fluClear <- cleanComments(fluMessag) ## CLEAR MESSAGES

fitFlu <- clusterDend(fluClear) ## DOING CLUSTER OF WORDS
plot(fitFlu) ## PLOT CLUSTER OF WORDS
rect.hclust(fitFlu, k=5)

fluClear2 <- fluClear %>%
              slice(c(1:500,1001:1500)) ## SELECT JUST 1000 ROWS
fluSent <- sentimentalAPIMs(fluClear2) ## DOING ANALYTICS SENTIMENTS WITH MICROSOFT


## Get posts and comments of the Vasco's page
vascoPage <- getPage(page="vascodagama", token=token, since = "2017-04-22", until = "2017-04-23") ##SEMI CARIOCA
post      <- getPost(post = vascoPage$id[1], n = 1000, token = token) ## GET COMMENTS OF THE POST
vascoComents <- post$comments ## GET COMMENT OF THE POST

vascoPage <- getPage(page="vascodagama", token=token, since = "2017-03-16", until = "2017-03-18") ##COPA DO BRASIL
post      <- getPost(post = vascoPage$id[4], n = 1000, token = token) ## GET COMMENTS OF THE POST
vascoComents <- rbind(vascoComents,post$comments) ## MERGE BETWEEN POSTS

vascoMessag  <- vascoComents %>% 
                  select(message) ## SELECT JUST COLUMN MESSAGE
names(vascoMessag) <- "text"    ## RENAME COLUMN
vascoClear <- cleanComments(vascoMessag) ## CLEAR MESSAGES

fitVasco <- clusterDend(vascoClear) ## DOING CLUSTER OF WORDS
plot(fitVasco) ## PLOT CLUSTER OF WORDS
rect.hclust(fitVasco, k=5)

vascoClear2 <- vascoClear %>%
                slice(c(1:500,1001:1500)) ## SELECT JUST 1000 ROWS
vascoSent <- sentimentalAPIMs(vascoClear2) ## DOING ANALYTICS SENTIMENTS WITH MICROSOFT


## Get posts and comments of the Botafogo's page
botaPage <- getPage(page="BotafogoOficial", token=token, since = "2017-04-23", until = "2017-04-24") ##SEMI CARIOCA
post     <- getPost(post = botaPage$id[1], n = 1000, token = token)
botaComents <- post$comments ## GET COMMENT OF THE POST

botaPage  <- getPage(page="BotafogoOficial", token=token, since = "2017-05-02", until = "2017-05-04") ##LIBERTADORES
post      <- getPost(post = botaPage$id[12], n = 1000, token = token) ## GET COMMENTS OF THE POST
botaComents <- rbind(botaComents,post$comments) ## MERGE BETWEEN POSTS

botaMessag  <- botaComents %>% 
                select(message) ## SELECT JUST COLUMN MESSAGE
names(botaMessag) <- "text"    ## RENAME COLUMN
botaClear <- cleanComments(botaMessag) ## CLEAR MESSAGES

fitBota <- clusterDend(botaClear) ## DOING CLUSTER OF WORDS
plot(fitBota) ## PLOT CLUSTER OF WORDS
rect.hclust(fitBota, k=5)

botaClear2 <- botaClear %>%
                slice(c(1:500,1001:1500)) ## SELECT JUST 1000 ROWS
botaSent <- sentimentalAPIMs(botaClear2) ## DOING ANALYTICS SENTIMENTS WITH MICROSOFT


## Get posts and comments of the Flamengo's page
flaPage <- getPage(page="FlamengoOficial", token=token, since = "2017-05-17", until = "2017-05-19") ##ULTIMA RODADA LIBERT
post    <- getPost(post = flaPage$id[2], n = 1000, token = token)
flaComents <- post$comments ## GET COMMENT OF THE POST

flaPage <- getPage(page="FlamengoOficial", token=token, since = "2017-04-26", until = "2017-04-28") ##LIBERT atletico paran
post    <- getPost(post = flaPage$id[7], n = 1000, token = token)
flaComents <- rbind(flaComents,post$comments) ## MERGE BETWEEN POSTS

flaMessag  <- flaComents %>% 
                  select(message) ## SELECT JUST COLUMN MESSAGE
names(flaMessag) <- "text"    ## RENAME COLUMN
flaClear <- cleanComments(flaMessag) ## CLEAR MESSAGES

fitFla <- clusterDend(flaClear) ## DOING CLUSTER OF WORDS
plot(fitFla) ## PLOT CLUSTER OF WORDS
rect.hclust(fitFla, k=5)

flaClear2 <- flaClear %>%
              slice(c(1:500,1001:1500)) ## SELECT JUST 1000 ROWS
flaSent  <- sentimentalAPIMs(flaClear2) ## DOING ANALYTICS SENTIMENTS WITH MICROSOFT

## PLOT WORD CLOUD
par(mfrow = c(2,2)) ## GRID FOR PLOT WORDCLUSTER
colFlu   <- brewer.pal(n = 8, name = "Dark2")  ## SET SPECIFIC COLOR
colBota  <- brewer.pal(n = 8, name = "Paired") ## SET SPECIFIC COLOR
colFla   <- brewer.pal(n = 8, name = "RdBu")   ## SET SPECIFIC COLOR
colVasco <- brewer.pal(n = 8, name = "BrBG")   ## SET SPECIFIC COLOR
pal2     <- brewer.pal(n = 8, name = "Dark2")

wordcloud(c(fluClear$text,flaClear$text,vascoClear$text,botaClear$text), min.freq=2, max.words=500, random.order=F, colors = pal2)
wordcloud(fluClear$text,   min.freq=2, max.words=500, random.order=F, colors=colFlu)
wordcloud(flaClear$text,   min.freq=2, max.words=500, random.order=F, colors=colFla)
wordcloud(vascoClear$text, min.freq=2, max.words=500, random.order=F, colors=colVasco)
wordcloud(botaClear$text,  min.freq=2, max.words=500, random.order=F, colors=colBota)

## SENTIMENTAL ANALYSIS
botaSent["time"]  <- "Botafogo"
fluSent["time"]   <- "Fluminense"
vascoSent["time"] <- "Vasco"
flaSent["time"]   <- "Flamengo"

sentiments <- rbind(botaSent, fluSent, vascoSent, flaSent)

## Mean score sentiment
sentMean <- sentiments %>%
                group_by(time) %>%
                summarise(mean = round(mean(Score), 3))
meanGeneral <- sentiments %>%
                summarise(mean = sum(Score)/n()) ## MEAN GENERAL. EVERY TEAM
cat(" ===> mean general:",meanGeneral$mean)


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

## Score IN GENERAL BY TEAM AND SENTIMENTAL
sentGeneral <- sentPercent %>%
                group_by(sentimento) %>%
                  summarise(freqs = round(sum(freq)/n(),3) ) ##

ggplot(sentGeneral, aes(x = sentimento, y = freqs,fill = sentimento)) + 
  geom_bar(stat = "identity",colour="black") + guides(fill = F) + theme_bw() + 
  geom_text(aes(label=freqs), position=position_dodge(width=0.9), size=5, vjust = -0.2) + 
  theme(axis.text.x = element_text(face="bold",size=14)) +
  scale_fill_manual(values=c("red1","gold1","forestgreen")) +
  ggtitle("Média de Score por Sentimento (%)") 



## PLOT MEAN SOCRE
plotMeand <- ggplot(sentMean, aes(x = time, y = mean, fill = time, label = mean)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="black") +
  scale_fill_manual(values=c("gray13", "red1","forestgreen","mediumblue")) +
  geom_text(aes(label=mean), position=position_dodge(width=0.9), size=5, vjust = -0.2) + 
  theme_bw() + guides(fill = F) +
  theme(axis.text.x = element_text(face="bold",size=14)) +
  ggtitle("Média de Score por Time  (%)") 
plotMeand

## PLOT SOCRE BY TEAM
plotSent <- ggplot(sentPercent,aes(x = time, y = freq, fill = sentimento,label = freq, group = freq) ) + 
  geom_bar(position=position_dodge(),stat="identity",colour="black") +
  scale_fill_manual(values=c("firebrick2", "gold1","chartreuse4")) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), size=5,vjust = -0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(face="bold",size=14)) + 
  ggtitle("Análise de Sentimento em (%) por Time")

plotSent


## SAVING DATA IN CSV FILES
fwrite(botaComents, "YOUR PATH",quote = T, row.names = F)
fwrite(botaSent, "YOUR PATH",quote = T, row.names = F)
fwrite(flaComents, "YOUR PATH",quote = T, row.names = F)
fwrite(flaSent, "YOUR PATH",quote = T, row.names = F)
fwrite(fluComents, "YOUR PATH",quote = T, row.names = F)
fwrite(fluSent, "YOUR PATH",quote = T, row.names = F)
fwrite(vascoComents, "YOUR PATH",quote = T, row.names = F)
fwrite(vascoSent, "YOUR PATH",quote = T, row.names = F)
