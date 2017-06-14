## Author: Gabriel Lima Gomes - 14/06/2017
## Script to make analysis about game AUS x BRA in 13/06/2017

library(twitteR)

## Exporting global variables and functions
source("/media/gabrielgomes/CFA5-83C0/Analise\ de\ sentimentos/analysisR/Functions.R")

###### --------------------- ANALISE DE TWITTER ######
consumer_key    <- ""
consumer_secret <- ""
access_token    <- ""
access_secret   <- ""
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


#####--------------- ANALYSIS ABOUT TAG "David Luiz" --------------#########
tweetsDavid  <- searchTwitter("David Luiz", n = 2000, lang = 'pt')
dfDavid      <- twListToDF(tweetsDavid)
cleanDavid   <- cleanComments(dfDavid,c("pra","david luiz","david","luiz"))

## Wordcloud
wordcloud(cleanDavid$text, min.freq = 3, max.words = 1000, random.order = F, colors = pal)
title(xlab = "Wordcloud tag \"David Luiz\"")

sentDavid <- sentimentalAPIMs(cleanDavid)

## Percent score posit sentiment
sentPosit <- sentDavid %>%
  mutate(total = n()) %>%
  filter(Score > 0.7) %>%
  mutate(freq  = round(  (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise() %>%
  mutate(sentimento = "Positivo")

## Percent score negative sentiment
sentNeg <- sentDavid %>%
  mutate(total = n()) %>%
  filter(Score <  0.3) %>%
  mutate(freq  = round(  (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise()%>%
  mutate(sentimento = "Negativo")

## Percent score neutral sentiment
sentNeut <- sentDavid %>%
  mutate(total = n()) %>%
  filter(Score > 0.3 & Score < 0.7) %>%
  mutate(freq  = round( (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise() %>%
  mutate(sentimento = "Neutro")

## Merge percent sentimentals
sentPercent <- rbind(sentNeg, sentPosit, sentNeut)

## PLOT OF THE SENTIMENTAL ABOUT DAVID LUIZ
ggplot(sentPercent, aes(x = sentimento, y = freq,fill = sentimento)) + 
  geom_bar(stat = "identity",colour="black") + guides(fill = F) + theme_bw() + 
  geom_text(aes(label=freq), position=position_dodge(width=0.9), size=5, vjust = -0.2) + 
  theme(axis.text.x = element_text(face="bold",size=14)) +
  scale_fill_manual(values=c("red1","gold1","forestgreen")) +
  ggtitle("Média de Score por Sentimento (%) para tag David Luiz") 



#####--------------- ANALYSIS ABOUT TAG "AUSvBRA" --------------#########
tweetsGame  <- searchTwitter("Brasil x Austrália", n = 2000, lang = 'pt')
dfGame      <- twListToDF(tweetsGame)
cleanGame   <- cleanComments(dfGame, c("brasil","austrália","brasilxaustrália","austráliaxbrasil"))

## Wordcloud
wordcloud(cleanGame$text, min.freq = 2, max.words = 1000, random.order = F, colors = pal)
title(xlab = "Wordcloud tag \"Brasil x Austrália\"")

sentGame <- sentimentalAPIMs(cleanGame)

## Percent score posit sentiment
sentPosit <- sentGame %>%
  mutate(total = n()) %>%
  filter(Score > 0.7) %>%
  mutate(freq  = round(  (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise() %>%
  mutate(sentimento = "Positivo")

## Percent score negative sentiment
sentNeg <- sentGame %>%
  mutate(total = n()) %>%
  filter(Score <  0.3) %>%
  mutate(freq  = round(  (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise()%>%
  mutate(sentimento = "Negativo")

## Percent score neutral sentiment
sentNeut <- sentGame %>%
  mutate(total = n()) %>%
  filter(Score > 0.3 & Score < 0.7) %>%
  mutate(freq  = round( (n()/total) * 100,3) ) %>%
  group_by(freq) %>%
  summarise() %>%
  mutate(sentimento = "Neutro")

## Merge percent sentimentals
sentPerGame <- rbind(sentNeg, sentPosit, sentNeut)

## PLOT OF THE SENTIMENTAL ABOUT DAVID LUIZ
ggplot(sentPerGame, aes(x = sentimento, y = freq,fill = sentimento)) + 
  geom_bar(stat = "identity",colour="black") + guides(fill = F) + theme_bw() + 
  geom_text(aes(label=freq), position=position_dodge(width=0.9), size=5, vjust = -0.2) + 
  theme(axis.text.x = element_text(face="bold",size=14)) +
  scale_fill_manual(values=c("red1","gold1","forestgreen")) +
  ggtitle("Média de Score por Sentimento (%) para tag AUSvBRA") 


