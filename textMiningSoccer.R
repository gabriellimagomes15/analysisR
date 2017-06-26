## Author: Gabriel Lima Gomes - 06/2017
## Script to make text mining on the comments about the Soccer Teams of the RJ 

source("/media/gabrielgomes/CFA5-83C0/Artigos\ Linkedin/analysisR/Functions.R")
setwd("/media/gabrielgomes/CFA5-83C0/Artigos\ Linkedin")

#### ------------------ TEXT MINING OF COMMENTS -----#########

####---BOTAFOGO--####
botaComents <- readComments("/media/gabrielgomes/CFA5-83C0/Analise\ de\ sentimentos/botaComents.csv", FALSE)
botaClear   <- cleanComments(botaComents, c("pra","q","time","é","tá","botafogo")) 
fitBota     <- clusterWords(botaClear)
plot(fitBota, main="Clusterização Hierárquica Facebook \n Botafogo")
rect.hclust(fitBota, k=8)

## Get UNI GRAM 
botaUniGram <- countTokens(botaClear)
biGrams     <-  countTokens(botaClear, numberToken = 2)
triGrams    <- countTokens(botaClear, numberToken = 3)

plotBotaUni <- plotCountTokens(botaUniGram, "Botafogo")
plotBotaBi  <- plotCountTokens(biGrams, "Botafogo")
plotBotaTri <- plotCountTokens(triGrams, "Botafogo")
grid.arrange(plotBotaUni, plotBotaBi, plotBotaTri, ncol=2)

## GRAPH OF WORDS
gBota <- graphWords(data = botaClear[c(1:100)]$text, numCluster = 6)
plot(gBota$graph, layout = gBota$label)
title("\nGraph of comments about Botafogo on the Facebook",col.main="gray40", cex.main=1.5, family="serif")

####---FLAMENGO--####
flaComents <- readComments("flaComents.csv", TRUE)
flaClear   <- cleanComments(flaComents,stopWords = c("pra","kkkkkk","é","ze ricardo",
                                                     "flamengo","pro"))

flaFit <- clusterWords(flaClear)
plot(flaFit, main="Clusterização Hierárquica Facebook \n Flamengo")
rect.hclust(flaFit, k=8)

## Get UNI GRAM 
flaUniGram  <- countTokens(flaClear)
flaBiGrams  <-  countTokens(flaClear, numberToken = 2)
flaTriGrams <- countTokens(flaClear, numberToken = 3)

flaPlotUni <- plotCountTokens(flaUniGram, "Flamengo")
flaPlotBi  <- plotCountTokens(flaBiGrams, "Flamengo")
flaPlotTri <- plotCountTokens(flaTriGrams, "Flamengo")
grid.arrange(flaPlotUni, flaPlotBi, flaPlotTri, ncol=2)

## GRAPH OF WORDS
flaGraph <- graphWords(data = flaClear[c(1:350)]$text, numCluster = 7)
plot(flaGraph$graph, layout = flaGraph$label)
title("\nGraph of comments about Flamengo on the Facebook",col.main="gray40", cex.main=1.5, family="serif")

####--- FLUMINENSE --####
fluComents <- readComments("/media/gabrielgomes/CFA5-83C0/Analise\ de\ sentimentos/fluComents.csv", TRUE)
fluClear <- cleanComments(fluComents, stopWords = c("pra","é","pro","chorachora",
                                                    "fluminense","time"))

fluFit <- clusterWords(fluClear)
plot(fluFit, main="Clusterização Hierárquica Facebook \n Fluminense")
rect.hclust(flaFit, k=8)

## Get UNI GRAM 
fluUniGram  <- countTokens(fluClear)
fluBiGrams  <-  countTokens(fluClear, numberToken = 2)
fluTriGrams <- countTokens(fluClear, numberToken = 3)

fluPlotUni <- plotCountTokens(fluUniGram, "Fluminense")
fluPlotBi  <- plotCountTokens(fluBiGrams, "Fluminense")
fluPlotTri <- plotCountTokens(fluTriGrams, "Fluminense")
grid.arrange(fluPlotUni, fluPlotBi, fluPlotTri, ncol=2)

## GRAPH OF WORDS
fluGraph <- graphWords(data = fluClear[c(1:500)]$text, numCluster = 7)
plot(fluGraph$graph, layout = fluGraph$label)
title("\nGraph of comments about Fluminense on the Facebook",col.main="gray40", cex.main=1.5, family="serif")

####--- VACOS --####
vascoComents <- readComments("/media/gabrielgomes/CFA5-83C0/Analise\ de\ sentimentos/vascoComents.csv", TRUE)
vascoClear <- cleanComments(vascoComents, stopWords = c("pra","pro","tomarcu","time","é","m","i",
                                                        "nd","vasco",""))
vascoClear$text <- gsub("k{2,}","",x = vascoClear$text, ignore.case = T)

vascoFit <- clusterWords(vascoClear)
plot(vascoFit, main="Clusterização Hierárquica Facebook \n Vasco")
rect.hclust(flaFit, k=8)

## Get UNI GRAM 
vascoUniGram  <- countTokens(vascoClear)
vascoBiGrams  <- countTokens(vascoClear, numberToken = 2)
vascoTriGrams <- countTokens(vascoClear, numberToken = 3)

vascoPlotUni <- plotCountTokens(vascoUniGram, "Vasco")
vascoPlotBi  <- plotCountTokens(vascoBiGrams, "Vasco")
vascoPlotTri <- plotCountTokens(vascoTriGrams, "Vasco")
grid.arrange(vascoPlotUni, vascoPlotBi, vascoPlotTri, ncol=2)

## GRAPH OF WORDS
vascoGraph <- graphWords(data = vascoClear[c(1:500)]$text, numCluster = 7)
plot(vascoGraph$graph, layout = vascoGraph$label)
title("\nGraph of comments about Vasco on the Facebook",col.main="gray40", cex.main=1.5, family="serif")
