## @knitr age

#customerdata <- haven::read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")
data <- customerdata #haven::read_sav('marketdata.sav')
data <- as.data.frame(data)


# Age Visualization

age = data$Alterskategorien
age = as.factor(age)


ggplot(mapping = aes(x = age, fill = age)) + 
  geom_bar(stat= 'count') + 
  scale_fill_discrete(name = "Age Category",
                      breaks = c("2","3","4","5","6","7","8","9"),
                      labels = c("2 (18-25 years)","3 (26-35 years)","4 (36-45 years)","5 (46-55 years)","6 (56-65 years)","7 (66-75 years)","8 (76-85 years)","9 (86+ years)"))+
  labs(title="Distribution of Age")


## @knitr gender

# Gender visualization

gender = data$S2_Geschlecht
gender = as.factor(gender)

ggplot(mapping = aes(x = gender, fill = gender)) + 
  geom_bar(stat= 'count') +
  scale_fill_discrete(name = "Gender",
                      breaks = c("1","2"),
                      labels = c("1 (Male)","2 (Female)"))+
  labs(title="Distribution of Gender")

## @knitr income

# Income visualization

#customerdata <- haven::read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")
data <- customerdata #haven::read_sav('marketdata.sav')
data <- as.data.frame(data)

income <- data$F44_HEK
income <- as.factor(income)

ggplot(mapping = aes(x = income, fill = income)) + 
  geom_bar(stat= 'count') + 
  scale_fill_discrete(name = "Income",
                      breaks = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","99"),
                      labels = c("1 (<1.250)","2 (1.250-1.500)","3 (1.500-1.750)", "4 (1.750-2.000)","5 (2.000-2.250)",
                                 "6 (2.250-2.500)", "7 (2.500-2.750)","8 (2.750-3.000)","9 (3.250-3.500)","10 (3.500-3.750)", "11 (3.750-4.000)","12 (4.000-4.500)",
                                 "13 (4.500-5.000)", "14 (5.000-5.500)","15 (5.500-6.000)","16 (>6.000)","99 (Keine Angabe)")) +
  labs(title="Distribution of Income")

## @knitr children

# Childern in household

children = data$F43_ANZAHL_KINDER
children = as.factor(children)


ggplot(mapping = aes(x = children, fill = children)) + 
  geom_bar(stat= 'count') +
  scale_fill_discrete(name = "Children in household",
                      breaks = c("1","2","3","4","5","6"),
                      labels = c("1 (0 child)","2 (1 child)","3(2 children)","4 (3 children)","5 (4 children)", "6 (above 4)"))+
  labs(title="Number of children in household")


## @knitr people

# people in household  

people = data$F42_HH_GROESSE
people = as.factor(people)

ggplot(mapping = aes(x = people, fill = people)) + 
  geom_bar(stat= 'count') +
  scale_fill_discrete(name = "People in household",
                      breaks = c("1","2","3","4","5","6"),
                      labels = c("1","2","3","4","5", "6 (above 5)"))+
  labs(title="Number of people in household")

## @knitr state

#Number of People belong to a state
state = data$S3_Bundesland

state = as.factor(state)

ggplot(mapping = aes(x = state, fill = state)) + 
  geom_bar(stat= 'count')+
  scale_fill_discrete(name = "Federal state",
                      breaks = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
                      labels = c("1 (Baden-Württemberg)","2 (Bayern)","3 (Berlin)", "4 (Brandenburg)","5 (Bremen)",
                                 "6 (Hamburg)", "7 (Hessen)","8 (Mecklenburg-Vorpommern)","9 (Niedersachsen)","10 (Nordrhein-Westfalen)", "11 (Rheinland-Pfalz)","12 (Saarland)",
                                 "13 (Sachsen)", "14 (Sachsen-Anhalt)","15 (Schleswig-Holstein)","16 (Thüringen)")) +labs(title="Distribution of people across Federal state")

## @knitr silhouette

df = data[c('Alterskategorien','S2_Geschlecht','F44_HEK','F43_ANZAHL_KINDER', 'F42_HH_GROESSE','S3_Bundesland')]


#Replace 99 value with mean w.r.t age

mean18_25 = df %>% filter(Alterskategorien==2 & !F44_HEK ==99)
mean18_25 = mean(as.numeric(mean18_25$F44_HEK)) %>% round()

mean26_35 = df %>% filter(Alterskategorien==3 & !F44_HEK ==99)
mean26_35 = mean(as.numeric(mean26_35$F44_HEK)) %>% round()

mean36_45 = df %>% filter(Alterskategorien==4 & !F44_HEK ==99)
mean36_45 = mean(as.numeric(mean36_45$F44_HEK)) %>% round()

mean46_55 = df %>% filter(Alterskategorien==5 & !F44_HEK ==99)
mean46_55 = mean(as.numeric(mean46_55$F44_HEK)) %>% round()


mean56_65 = df %>% filter(Alterskategorien==6 & !F44_HEK ==99)
mean56_65 = mean(as.numeric(mean56_65$F44_HEK)) %>% round()

mean66_75 = df %>% filter(Alterskategorien==7 & !F44_HEK ==99)
mean66_75 = mean(as.numeric(mean66_75$F44_HEK)) %>% round()

mean76_85 = df %>% filter(Alterskategorien==8 & !F44_HEK ==99)
mean76_85 = mean(as.numeric(mean76_85$F44_HEK)) %>% round()

mean86 = df %>% filter(Alterskategorien==9 & !F44_HEK ==99)
mean86 = mean(as.numeric(mean86$F44_HEK)) %>% round()


for(i in 1:nrow(df))
{
  if(df[i,3] == 99)
  {
    if(df[i,1] == 2)
    {
      df[i,3] = mean18_25
    }
    if(df[i,1] == 3)
    {
      df[i,3] = mean26_35
    }
    if(df[i,1] == 4)
    {
      df[i,3] = mean36_45
    }
    if(df[i,1] == 5)
    {
      df[i,3] = mean46_55
    }
    if(df[i,1] == 6)
    {
      df[i,3] = mean56_65
    }
    if(df[i,1] == 7)
    {
      df[i,3] = mean66_75
    }
    if(df[i,1] == 8)
    {
      df[i,3] = mean76_85
    }
    if(df[i,1] == 9)
    {
      df[i,3] = mean86
    }
  }
}

#Visualizing different clustering results

#Silhouette method

fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## @knitr elbow

#Elbow method

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


## @knitr pca


#K-means clustering

km.res = kmeans(df, 3, nstart = 100)


#Visulaization Using PCA

#library(factoextra)
#pcclust=prcomp(df,scale=FALSE) #principal component analysis


#dignm<-as.character(km.res$cluster)


#plot(pcclust$x[,1:2], col = km.res$cluster,xlab ="PC-1",ylab="PC-2", main = "Visualzing clustering results using PCA")
#legend("bottomright",title = "clusters",legend = unique(dignm), fill=unique(km.res$cluster))


## @knitr tsne
#Visulaization Using T-SNE


label = as.factor(km.res$cluster)

colors = rainbow(length(unique(label)))
names(colors) = unique(label)

tsne <- Rtsne(df, dims = 2, check_duplicates = FALSE,perplexity=30, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(df, dims = 2, check_duplicates = FALSE,perplexity=30, max_iter = 500))

plot(tsne$Y, t='n', xlab = 'tsne1', ylab = 'tsne2', main="tsne visualization of the clustering result")
text(tsne$Y, labels=label,col=colors[label])


#######################################
## @knitr table

#Summary statisics of the clusters
df2 = data
df2$cluster = km.res$cluster
df2$F44_HEK = df$F44_HEK

cluster1 = df2 %>% filter(cluster==1)
cluster2 = df2 %>% filter(cluster==2)
cluster3 = df2 %>% filter(cluster==3)

dr = data.frame()

mean1_age = round(mean(cluster1$Alterskategorien))
mean2_age = round(mean(cluster2$Alterskategorien))
mean3_age = round(mean(cluster3$Alterskategorien))

mean1_income = round(mean(cluster1$F44_HEK))
mean2_income = round(mean(cluster2$F44_HEK))
mean3_income = round(mean(cluster3$F44_HEK))

mean1_child = round(mean(cluster1$F43_ANZAHL_KINDER))
mean2_child = round(mean(cluster2$F43_ANZAHL_KINDER))
mean3_child = round(mean(cluster3$F43_ANZAHL_KINDER))

mean1_people = round(mean(cluster1$F42_HH_GROESSE))
mean2_people = round(mean(cluster2$F42_HH_GROESSE))
mean3_people = round(mean(cluster3$F42_HH_GROESSE))

group1 = c(1, mean1_age, mean1_income, mean1_child, mean1_people, nrow(cluster1))
group2 = c(2, mean2_age, mean2_income, mean2_child, mean2_people,  nrow(cluster2))
group3 = c(3, mean3_age, mean3_income, mean3_child, mean3_people, nrow(cluster3))

dr = rbind(group1, group2, group3)
library(knitr)
knitr::kable(dr,col.names = c("Groups","Avg_Age","Avg_Income","Avg_children","Avg_people","count") ,caption = "Summary of the clustering results")

## @knitr incomevsstate
library(plotly)
groups <- as.factor(km.res$cluster)
g =ggplot(df, aes(S3_Bundesland, F44_HEK, color = groups)) +
  geom_count() +
  scale_size_area(max_size = 20)+
  ggtitle("Income vs state")

ggplotly(g)

