library(haven)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tmap)
library(sf)
library(plotly)
library(tm)
library(wordcloud)


#' Function to get distinct Names
#' \code{GetDistinctNames} this function get the distinct names and their count
#' @param DataFrame contains the single column with market names or state names
#' 
#' @return returns a df containing Distinct names and count
GetDistinctNames <- function(df){
  
  ndist <-tapply(1:NROW(df), df[1], function(x) length(unique(x)))
  
  ndist<- data.frame(ndist)
  return(ndist)
}

#' Function to get unique column names
#' \code{GetUniqueColumnNames} this function get unique column names
#' @param DataFrame contains the Individual satisfaction questions.
#' 
#' @return returns a list with contains unique column names.
GetUniqueColumnNames <- function(DF){
  
  ColNames <- sub('\\_.*', "", colnames(DF))
  
  return(unique(ColNames))
}

#' Function to create empty df
#' \code{CreateEmptyDF} this function creates an empty dataframe with 2303 rows
#' 
#' @return returns the dataframe with 2303 rows filled with numbers 1 to 2303.
CreateEmptyDF <- function(){
  
  return(data.frame(a=1:2303))
}

#' Function to calculate rowmean
#' \code{GetRowMeanOfAllTheSubQuestions} this function appends a column with row Means
#' 
#' @param DataFrame contains the Individual satisfaction questions(80 features)
#' @return returns the row means of all the questions.
GetRowMeanOfAllTheSubQuestions <- function(df){
  
  df <- df %>% mutate(RowMeans =rowMeans(df  ,na.rm = TRUE) )
  
  return(df[ncol(df)])
  
}

#' Function to Give stateNames
#' \code{AddingStateNamesForMap} this function checks the condition if condition is true it incerts the corresponding name
#' 
#' @param DataFrame contains the State Names column S3_Bundesland
#' @return returns the df with statenames in it
AddingStateNamesForMap <- function(df){
  df$S3_Bundesland<-as.character(df$S3_Bundesland)
  df$S3_Bundesland[df$S3_Bundesland=="1"]<-"Baden-Württemberg"
  df$S3_Bundesland[df$S3_Bundesland=="2"]<-"Bavaria"
  df$S3_Bundesland[df$S3_Bundesland=="3"]<-"Berlin"
  df$S3_Bundesland[df$S3_Bundesland=="4"]<-"Brandenburg"
  df$S3_Bundesland[df$S3_Bundesland=="5"]<-"Bremen"
  df$S3_Bundesland[df$S3_Bundesland=="6"]<-"Hamburg"
  df$S3_Bundesland[df$S3_Bundesland=="7"]<-"Hesse"
  df$S3_Bundesland[df$S3_Bundesland=="8"]<-"Mecklenburg-Vorpommern"
  df$S3_Bundesland[df$S3_Bundesland=="9"]<-"Lower Saxony"
  df$S3_Bundesland[df$S3_Bundesland=="10"]<-"North Rhine-Westphalia"
  df$S3_Bundesland[df$S3_Bundesland=="11"]<-"Rhineland-Palatinate"
  df$S3_Bundesland[df$S3_Bundesland=="12"]<-"Saarland"
  df$S3_Bundesland[df$S3_Bundesland=="13"]<-"Saxony"
  df$S3_Bundesland[df$S3_Bundesland=="14"]<-"Saxony-Anhalt"
  df$S3_Bundesland[df$S3_Bundesland=="15"]<-"Schleswig-Holstein"
  df$S3_Bundesland[df$S3_Bundesland=="16"]<-"Thuringia"
  names(df)[names(df)=="S3_Bundesland"] <- "name"
  
  return(df)
}

#' PATH TO THE SHAPE FILE
path <- paste0(getwd(),"FinalReport/Igismap/Germany_Polygon.shp")

#' Saving directory path
SavingFilePath <- getwd()

#' Reading the files
germany<- st_read(path)
data_set<-read_sav(file.choose())



MarketNames <- data_set %>% select(starts_with("F3_"))

MarketNames <- as_factor(MarketNames)

DistinctMarketNames <- GetDistinctNames(MarketNames)

DistinctMarketNames <- rownames_to_column(DistinctMarketNames, "SuperMarketNames")

StateNames <- data_set %>% select(starts_with("S3_"))
# 
StateNames <- as_factor(StateNames)

DistinctStateNames <- GetDistinctNames(StateNames)
# 
# DistinctStateNames <- rownames_to_column(DistinctStateNames, "StateNames")

IndividualOverallSatisfaction <- data_set %>% select(starts_with("F10_"))


IndividualOverallSatisfactionRowMean<- GetRowMeanOfAllTheSubQuestions(IndividualOverallSatisfaction)

IndividualOverallSatisfactionRowMean <- cbind(StateNames , IndividualOverallSatisfactionRowMean)

IndividualOverallSatisfactionRowMean <- AddingStateNamesForMap(IndividualOverallSatisfactionRowMean)

IndividualOverallSatisfactionRowMean <- cbind(MarketNames , IndividualOverallSatisfactionRowMean)



write.csv(IndividualOverallSatisfactionRowMean, paste0(SavingFilePath,"/OverallSatisfactionMap.csv"), row.names=FALSE, quote=FALSE)

rNames <- c("Baden-Wuttemberg","Bavaria","Berlin","Brandenburg","Bremen","Hamburg","Hesse","Mecklenburg-Vorpommern",
            "Lower Saxony","North Rhine-Westphalia","Rhineland-Palatinate","Saarland","Saxony","Saxony-Anhalt",
            "Schleswig-Holstein","Thuringia")

#Filtering out the data of edeka
Edeka<-data_set %>% 
  filter(F3_Haupteinkaufsstaette==1) %>% 
  select(F3_Haupteinkaufsstaette,S3_Bundesland,ends_with("_Global"))

#Finding out the mean

groupping<-Edeka %>% 
  group_by(S3_Bundesland) %>% 
  summarize(Mean_value=mean(F10_1_SORTIMENT_Global))

EdekaRowmean<-Edeka %>% mutate(RowMeanOfColumns = rowMeans(Edeka[-(1:2)]  ,na.rm = TRUE))

groupping<-Edeka %>% 
  group_by(S3_Bundesland) %>% 
  summarize(Mean_value=mean(F10_1_SORTIMENT_Global))


GroupEdeka <- EdekaRowmean %>% group_by(S3_Bundesland) %>% summarize(Mean_value=mean( RowMeanOfColumns  ,na.rm = TRUE))

GroupRewe <- IndividualOverallSatisfactionRowMean %>% filter(F3_Haupteinkaufsstaette %in% 'Rewe') 

selectedGroup <- GroupRewe %>% group_by(name) %>% summarize(Mean_value = mean(RowMeans , na.rm = TRUE))

selectedGroup100 <- IndividualOverallSatisfactionRowMean %>% 
  filter(F3_Haupteinkaufsstaette %in% 'Rewe') %>% group_by(name) %>% summarize(Mean_value = mean(RowMeans , na.rm = TRUE))

#Renaming the data according to .shp file


GroupEdeka <- AddingStateNames(GroupEdeka)

groupping <- AddingStateNames(groupping)


germany <- data.frame(germany)

my_map_data<-inner_join(germany,GroupEdeka)

tm_shape(my_map_data)+ tm_polygons("Mean_value",id="name")

p

####
#  Word Cloud DF
#####
WordCloudDF <- data_set %>%  select(F7_2_ERWARTUNGEN_1,F7_1.1_ZUFR_BEGR_offen)

WordCloudDF <- cbind(MarketNames , WordCloudDF)

write.csv2(WordCloudDF, paste0(SavingFilePath,"/WordCloudDF.csv"), row.names=FALSE, quote=FALSE)

Edeka<-WordCloudDF %>% filter(F3_Haupteinkaufsstaette %in% "Edeka") %>% select(F7_2_ERWARTUNGEN_1,F7_1.1_ZUFR_BEGR_offen)

top_ratings<-Edeka %>% 
  filter(F7_2_ERWARTUNGEN_1 %in% c(4,5)) %>% 
  select(F7_1.1_ZUFR_BEGR_offen)

corpus<-Corpus(VectorSource(top_ratings))
corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,removeWords,stopwords("german"))
corpus<-tm_map(corpus,stripWhitespace)
tdm<-TermDocumentMatrix(corpus)
a<-as.matrix(tdm)
v<-sort(rowSums(a),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
Edeka_topword<-wordcloud(d$word,d$freq,random.order = FALSE,rot.per = 0.3,scale = c(2.4,.4),max.words = 105,colors=brewer.pal(8,"Dark2"))


lower_ratings<-Edeka %>% 
  filter(F7_2_ERWARTUNGEN_1 %in% c(1,2,3)) %>% 
  select(F7_1.1_ZUFR_BEGR_offen)

lower_corpus<-Corpus(VectorSource(lower_ratings))
lower_corpus<-tm_map(lower_corpus,content_transformer(tolower))
lower_corpus<-tm_map(lower_corpus,removeWords,stopwords("german"))
lower_corpus<-tm_map(lower_corpus,stripWhitespace)
lower_tdm<-TermDocumentMatrix(corpus)
a2<-as.matrix(lower_tdm)
v2<-sort(rowSums(a2),decreasing = TRUE)
d2<-data.frame(word=names(v2),freq=v2)
Edeka_lowword<-wordcloud(d2$word,d2$freq,random.order = FALSE,rot.per = 0.3,scale = c(2.4,.4),max.words = 105,colors=brewer.pal(8,"Dark2"))

######
# Distribution of income
#####
IncomeDistributionDF <- data_set %>%  select(F44_HEK ,S3_Bundesland)

IncomeDistributionDF <- AddingStateNames(IncomeDistributionDF)


IncomeDistributionDF <- cbind(MarketNames,IncomeDistributionDF)


write.csv(IncomeDistributionDF, paste0(SavingFilePath,"/IncomeDistributionDF.csv"), row.names=FALSE, quote=FALSE)


Ed<-IncomeDistributionDF %>% 
  filter(F3_Haupteinkaufsstaette %in% 'Edeka') %>% 
  select(F44_HEK,S3_Bundesland) %>% 
  filter(F44_HEK!=99)

View(Ed)


