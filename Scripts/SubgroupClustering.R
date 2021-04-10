
#' Install these following packages 
#' Importing them into your environment

library(haven)
library(tidyverse)
library(mclust)
library(dplyr)
library(tidyr)
library(cluster)    # clustering algorithms
library(factoextra)
library(ggplot2)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(plotly)
library(reshape2)
library(imputeTS)

#' Set you working Directory to the folder containing this scripts
#' Reading the Market Data file
path = file.path( "MarketData.sav")
dataset = read_sav(path)

#' Function to get the clustering results
#' \code{GetClusterResults} this function generates the clusters from the question subgroups for eg:Vegan_1 ,Vegan_2
#' 
#' @param DataFrame contains the Individual satisfaction questions(80 features)
#' @param LoopOverList contains the unique Column names.
#' @param addToDF contains an empty df for cbind. 
#' @return returns the cluster results of all the questions.
GetClusterResults <- function(df,LoopOverlist , addToDF){
  
  for (i in 1:length(LoopOverlist)){
    
    Z <- df %>% select(starts_with(LoopOverlist[i]))
    Z.clustering <- kmeans(Z,5)
    addToDF <- cbind(addToDF,Z.clustering$cluster)
    colnames(addToDF)[ncol(addToDF)]<- paste(LoopOverlist[i],"clustering")
  }
  return(addToDF)
  
}

#' Function to calculate rowmean
#' \code{GetRowMeanOfEachRow} this function generates the rowmeans from the question subgroups for eg:Vegan_1 ,Vegan_2
#' 
#' @param DataFrame contains the Individual satisfaction questions(80 features)
#' @param LoopOverList contains the unique Column names.
#' @param addToDF contains an empty df for cbind. 
#' @return returns the row means of all the questions.
GetRowMeanOfEachRow <- function(df,LoopOverlist , addToDF){
  
  for (i in 1:length(LoopOverlist)){
    
    Z <- df %>% select(starts_with(LoopOverlist[i]))
    Z.rowmean <- rowMeans(Z)
    addToDF <- cbind(addToDF,Z.rowmean)
    colnames(addToDF)[ncol(addToDF)]<- paste(LoopOverlist[i],"clustering")
  }
  return(addToDF)
}

#' Function to create empty df
#' \code{CreateEmptyDF} this function creates an empty dataframe with 2303 rows
#' 
#' @return returns the dataframe with 2303 rows filled with numbers 1 to 2303.
CreateEmptyDF <- function(){
  
  return(data.frame(a=1:2303))
}

#' Function to replace na
#' \code{ReplaceNaInDF} this function replaces nan with 0
#' @param DataFrame contains the Individual satisfaction questions.
#' 
#' @return returns the dataframe replaced na with 0.
ReplaceNaInDF <- function(DF){
  
  ColumnNamesList<- setNames(lapply(vector("list", ncol(DF)), function(x) x <- 0), names(DF))
  return( DF %>% replace_na(ColumnNamesList))
  
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

#' Function to get optimum clusters
#' \code{GetOptimumClustersElbowMethod} this function calculates the optimum clusters which can be formed for a df using elbow method
#' i.e mean squared errors. and draws a plot with error on y axis and k values in x axis
#' @param DataFrame contains the Individual satisfaction questions.
#' 
#' @return returns a a df excluding 1st column.
GetOptimumClustersElbowMethod <- function(df){
  mydata <- df[,-1]
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 1:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  return(mydata)
}

#' Function performs kmeans
#' \code{KmeansClustering} this function performs the Kmeans clustering on the df
#' @param DataFrame contains the Individual satisfaction questions.
#' 
KmeansClustering<- function(df ){
  kmeans(df, centers = 2, nstart = 25)
}

#' Function to plot
#' \code{GGplotting} this function plots ggplots for various stores 
#' @param DataFrame contains the Individual satisfaction questions.
GGplotting <- function(df){
  p <- ggplot(df, aes(x=F3_Haupteinkaufsstaette, y=value,fill=variable))+
    geom_boxplot()+
    facet_grid(.~variable)+
    labs(x="")+
    theme(axis.title.x=element_text(angle=0, vjust=0.4,hjust=1),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    # theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1),
    #       axis.ticks.x=element_blank())
  fig<- ggplotly(p)
  fig
}


#' Function to Non Na's in the df
#' \code{GetCountOfNonNAInDF} this function calculates the Non Na's in the df by looping over every column
#' @param DataFrame contains the Individual satisfaction questions.
#' @param BindDf contains an empty df with 2303 rows for cbind
#' 
#' @return returns a  df excluding 1st column.
GetCountOfNonNAInDF <- function(df , BindDf){
  columnNames<-c()
  columnSum <- c()
  for (i in IndividualGroupsSatisfactioncOLnAMES) {
    c <- tapply(1:NROW(df), df[i], function(x) length(unique(x)))
    
    # columnNames<-c(columnNames , c)
    # columnSum <- c(columnSum , sum(c))
    BindDf[nrow(BindDf)+1,] <- sum(c)

  }
  return(BindDf )
  
}

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

#' converts df to 2 columns i.e Values and Variables
#' \code{GetMeltDF} this function Gets the into 2 column shape
#' @param DataFrame contains the filtered df with respect to cluster results
#' 
#' @return returns a melted df
GetMeltDF <- function(DF){
  dfmelt<- melt(DF, measure.vars = 3:ncol(DF))
  return(dfmelt)
}

#' subsetting shoping mall and getting distinct names

MarketNames <- data_set %>% select(starts_with("F3_"))

MarketNames <- as_factor(MarketNames)

DistinctMarketNames <- GetDistinctNames(MarketNames)

DistinctMarketNames <- rownames_to_column(DistinctMarketNames, "SuperMarketNames")


#' Extracting columns related to customer satisfaction from dataset
IndividualGroupsSatisfaction <- dataset %>% select(F10_1_SORTIMENT_Global:F10_6_WARENPRÄSENTATION_Global ,
                                                   F12_1_SORTIMENT_Regional:F12_3_WARENVERFUEGBARKEIT_Regional,
                                                   F15_1_SORTIMENT_Eigenmarke:F22_6_WARENPRAESENTATION_Suess , 
                                                   F24_1_SORTIMENT_Alkoholfrei:F24_5_WARENPRAESENTATION_Alkoholfrei)

#' Extracting columns related to customer satisfaction from dataset
IndividualGroupsSatisfaction <- dataset %>% select(F10_1_SORTIMENT_Global:F12_3_WARENVERFUEGBARKEIT_Regional,
                                                   F14_1_SORTIMENT_Vegan:F26_5_WARENPRAESENTATION_Gegenstaende)

                                                  
#' Extracting columns related to Branch satisfaction satisfaction from dataset
IndvidualBranchSatisfaction  <- dataset %>% select(F27_1_MITARBEITER_1:F35_5_ATMOSPHAERE_EINLADEND_1)

#' Extracting columns related to customer Loyality from dataset
customersLoyalty <- dataset %>% select(F8_1_EMPFEHLUNG_1:F9_NPS_1)

#' Extracting columns related to PriceAndQuality  from dataset
PriceAndQuality <- dataset %>% select(F40_1_REDUZIERTER_PREIS_1:F41_7_ERSTBESTE_1_invertiert)



dfColumnNames <-GetUniqueColumnNames(IndividualGroupsSatisfaction)

dfColumnNames <-data.frame(dfColumnNames)

DistintDFColumnNames <- GetDistinctNames(dfColumnNames)


#' 
rnames <- c('Overall','Snacks','Regional','Organic','Vegan','Private','Branded','FruitVeggies',
  'BreadnBaked','Flesh','Sausages','Dairy','Sweets','AlcoholicDrinks',  'SoftDrinks' ,'Cosmetics' , 'Utensils' )

rownames(unq) <- rnames

unq<- rownames_to_column(unq)

colnames(unq) <- c("names" , "count")


#' writing to csv
write.csv(unq, paste0(getwd() ,"/DatasetInspection.csv" ), row.names=FALSE, quote=FALSE)

#' Plotly ggplot
fig <- plot_ly(unq, x = ~names, y = ~count, type = 'bar',
               text = ~count, textposition = 'auto',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = "Non NA's in the Questions",
                      xaxis = list(title = "Count of Non NA's"),
                      yaxis = list(title = "Product Groups"))

fig


#' Calculates col mean
IndividualGroupsSatisfaction <- na_mean(IndividualGroupsSatisfaction)

IndividualGroupsSatisfactionUNiqColNames <- GetUniqueColumnNames(IndividualGroupsSatisfaction)

INdividualSatisfactionClusters <- CreateEmptyDF()

q <- subset(IndividualGroupsSatisfactionUNiqColNames,IndividualGroupsSatisfactionUNiqColNames != "F13")


INdividualSatisfactionClusters<- GetRowMeanOfEachRow(IndividualGroupsSatisfaction,q,INdividualSatisfactionClusters)

INdividualSatisfactionClusters$a<-NULL

#'1 replacing Col names
colnames(INdividualSatisfactionClusters) <- c('Overall','Regional','Private','Branded','FruitVeggies',
                       'BreadnBaked','Flesh','Sausages','Dairy','Sweets',  'SoftDrinks' )

#' 2 replacing Col names
colnames(INdividualSatisfactionClusters) <- c('Overall','Snacks','Regional','Private','Vegan','Branded','FruitVeggies',
                                              'BreadnBaked','Flesh','Sausages','Dairy','Sweets','Alcoholic','SoftDrinks',
                                              'Cosmetics','Utensils')




#' writing to a csv
write.csv(INdividualSatisfactionClusters, paste0(getwd() ,"/INdividualSatisfactionClusters_abc.csv" ), row.names=FALSE, quote=FALSE)

INdividualSatisfactionClusters <- cbind(MainShoppingMall , INdividualSatisfactionClusters)

#' writing to a csv
write.csv(INdividualSatisfactionClusters, paste0(getwd(),"/INdividualSatisfactionClusters_abc.csv"), row.names=FALSE, quote=FALSE)

AllShoppingMarketNames <- rownames(ndist)

#' filtering supermarket which have more than 75 customers
FilteredNdist <- ndist%>% filter(ndist >= 75)

FilteredShoppingMarketNames <- FilteredNdist$SuperMarketNames

FilteredMainShoopingMarkets<- INdividualSatisfactionClusters %>% filter(F3_Haupteinkaufsstaette %in% c('Edeka' , 'Kaufland'))

#' GetOptimumClusters
DF <- GetOptimumClustersElbowMethod(INdividualSatisfactionClusters)

k2 <- KmeansClustering(DF )

INdividualSatisfactionClustersk2 <- k2$cluster

INdividualSatisfactionClustersk2 <- data.frame(INdividualSatisfactionClustersk2)

#' combine cluster results to df
INdividualSatisfactionClusters <- cbind(INdividualSatisfactionClustersk2 , INdividualSatisfactionClusters)

write.csv(INdividualSatisfactionClusters,paste0(getwd(), "/BasicIndividualGroupsSatisfactionClustersWithFiltered.csv"), row.names=FALSE, quote=FALSE)


#' Filters customers fall in 1st cluster
fil1<- INdividualSatisfactionClusters %>% filter(INdividualSatisfactionClustersk2==1)

#' Box plot
p <- ggplot(GetMeltDF(fil1), aes( variable, value, fill = variable)) + 
  geom_boxplot(size = 1) + 
  theme(
    axis.text.x=element_text(angle=-45, vjust=0.4,hjust=1)
  )
  ggtitle("BoxPlot of Customers in 1st Cluster")

# Need to modify the plotly object to make sure line width is larger than default
fig <- plotly_build(p)

fig$data <- lapply(fig$data, FUN = function(x){
  x$line = list(width = 10)
  return(x)
})

fig

#' Filters customers fall in 2nd cluster
fil2 <- INdividualSatisfactionClusters %>% filter(INdividualSatisfactionClustersk2 %in% 2)

#BoxPLOT
p <- ggplot(GetMeltDF(fil2), aes( variable, value, fill = variable)) + 
  geom_boxplot(size = 1) + 
  theme(
    axis.text.x=element_text(angle=-45, vjust=0.4,hjust=1)
  )
  ggtitle("BoxPlot of Customers in 2st Cluster")

# Need to modify the plotly object to make sure line width is larger than default
fig <- plotly_build(p)

fig$data <- lapply(fig$data, FUN = function(x){
  x$line = list(width = 10)
  return(x)
})

fig

p<- ggplot(data = INdividualSatisfactionClusters[,-1], aes_string(x="F3_Haupteinkaufsstaette", y="Overall", fill="F3_Haupteinkaufsstaette")) +
geom_boxplot() +
scale_fill_viridis(discrete = True, alpha=0.5) +
geom_jitter(color="black", size=0.4, alpha=0.9) +
theme_ipsum() +
theme(
  legend.position="none",
  axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1),
  plot.title = element_text(size=11)
) +
ggtitle("A boxplot with jitter") +
xlab("")

fig <- ggplotly(p)
fig


########
#various GGPLOTTINGS
########
p1 <- ggplot(data = INdividualSatisfactionClusters,aes(x = F3_Haupteinkaufsstaette, y =Regional , fill = F3_Haupteinkaufsstaette)) +  
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.5))

f <- ggplotly(p1)
f



p10 <- ggplot(data = INdividualSatisfactionClusters, aes(x = F3_Haupteinkaufsstaette, 
                                                          y = Regional)) +
  geom_boxplot(fill = fill, colour = line,
               outlier.colour = "#1F3552", outlier.shape = 20) +
  scale_y_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_x_discrete(name = "Month") +
  ggtitle("Boxplot of mean ozone by month")7

p10

# fig <- plot_ly(y = c(rnorm(50) , rnorm(100)), type = "box", boxpoints = "all", jitter = 0.3,
#                pointpos = -1.8)
# 
# fig

fig <- plot_ly(data = INdividualSatisfactionClusters, x= INdividualSatisfactionClusters$F3_Haupteinkaufsstaette, y = INdividualSatisfactionClusters["Snacks"], type = "box", boxpoints = "all", jitter = 0.3,
               pointpos = -1.8)


fig <- plot_ly(ggplot2::diamonds, y = ~price, color = ~cut, type = "box")

fig


#####
# Branch Satisfaction#
#####
INdividualBranchSatisfactionClusters <- CreateEmptyDF()

IndividualBranchUniqColNames <-GetUniqueColumnNames(IndvidualBranchSatisfaction)

IndvidualBranchSatisfactionNa0 <- ReplaceNaInDF(IndvidualBranchSatisfaction)

INdividualBranchSatisfactionClusters <- GetClusterResults(IndvidualBranchSatisfactionNa0,IndividualBranchUniqColNames,INdividualBranchSatisfactionClusters)

INdividualBranchSatisfactionClusters$a <-NULL

colnames(INdividualBranchSatisfactionClusters) <- c('Overall' , 'Employees' , 'CashRegisters' , 'SelfCheckouts' ,
                                                    'Location' , 'Construction' , 'Findability/Orientation' , 'PriceLabeling',
                                                    'Ambience')


INdividualBranchSatisfactionClusters <- cbind(MainShoppingMall , INdividualBranchSatisfactionClusters)

DFrame<- GetOptimumClustersElbowMethod(INdividualBranchSatisfactionClusters)

k3 <- KmeansClustering(DFrame )

INdividualBranchSatisfactionClustersk3 <- k3$cluster

INdividualBranchSatisfactionClustersk3 <- data.frame(INdividualBranchSatisfactionClustersk3)

INdividualBranchSatisfactionClusters <- cbind(INdividualBranchSatisfactionClustersk3 , INdividualBranchSatisfactionClusters)


df <- INdividualBranchSatisfactionClusters %>%
  filter(F3_Haupteinkaufsstaette %in% "Rewe")

dfmelt<-melt(df, measure.vars = 3:ncol(df))

GGplotting(dfmelt)


write.csv(INdividualBranchSatisfactionClusters, "~/OVGU_Courses/DataScienceWithR/SubgroupClustering/RMDIndividualBranchSatisfaction.csv", row.names=FALSE, quote=FALSE)
# CustomerFrequentlyVisitedStore1 <- dataset %>% select(starts_with("F1_"))
# 
# CustomerFrequentlyVisitedStore <- data.frame(lapply(seq_along(CustomerFrequentlyVisitedStore), function(x) ifelse(CustomerFrequentlyVisitedStore[[x]] == 1, names(CustomerFrequentlyVisitedStore)[x], CustomerFrequentlyVisitedStore[[x]])))
#####
# Branch Satisfaction End
#####

#######################
# combined Clustering #
#######################
x=data.frame(a=1:2303)
for (i in 1:length(myListColNamesUniq)){
  
  table = data.frame()
  table <- IndividualGroupsSatisfactionNa0 %>% select(starts_with(myListColNamesUniq[i])) 
  table <- table %>% mutate(numOfCol = 1/ncol(table) ) 
  
  table <- table[["numOfCol"]] * table[(1:ncol(table)-1)]
  
  d<- table %>%
    mutate(Total = rowSums(table[,colnames(table)]))
  
  colnames(d)[ncol(d)]<- myListColNamesUniq[i]
  x <- cbind(x,d[ncol(d)])
}

x$a <- NULL

k<- 2


x<- data.frame(x)
mod5 <- densityMclust(x)

mod3 <- MclustDA(x)

mydata <- x
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



k10 <- kmeans(x, centers = 10, nstart = 25)

fviz_cluster(k10, data = x)

fviz_nbclust(head(x,500), kmeans, method='silhouette')


silhouette_score <- function(k){
  km <- kmeans(x, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(x))
  mean(ss[, 3])
}
k <- 2:20
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

km.final <- kmeans(x, 2)

plot(x, col = km.final$cluster)
## Total Within cluster sum of square
km.final$tot.withinss
km.final$size
data$cluster <- km.final$cluster
head(data, 6)

#######
# End #
#######






