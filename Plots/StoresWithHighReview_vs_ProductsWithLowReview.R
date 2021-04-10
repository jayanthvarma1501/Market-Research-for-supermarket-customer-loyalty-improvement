## @knitr StoresWithHighReview_vs_ProductsWithLowReview

if (!require("dplyr")) install.packages("dplyr") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("haven")) install.packages("haven") 
if (!require("ggplot2")) install.packages("ggplot2") 

library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

#customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

calculate_s6_ratings <- function(customerdata, metric_colnames) {
  metric_cols <- customerdata[metric_colnames]
  result <- colMeans(metric_cols, na.rm = TRUE, dims = 1)
}

calculate_s6_store_metric <- function(customerdata) {
  snacks_ratings <- calculate_s6_ratings(customerdata, c("F11_1_SORTIMENT_Snacks","F11_2_PREISLEISTUNG_Snacks","F11_3_WARENVERFUEGBARKEIT_Snacks"))
  bioprod_ratings <- calculate_s6_ratings(customerdata, c("F13_1_SORTIMENT_BioProd","F13_2_PREISLEISTUNG_BioProd","F13_3_WARENVERFUEGBARKEIT_BioProd"))
  vegan_ratings <- calculate_s6_ratings(customerdata, c("F14_1_SORTIMENT_Vegan","F14_2_PREISLEISTUNG_Vegan","F14_3_WARENVERFUEGBARKEIT_Vegan"))
  alkohol_ratings <- calculate_s6_ratings(customerdata, c("F23_1_SORTIMENT_Alkohol","F23_2_QUALITAET_Alkohol","F23_3_PREISLEISTUNG_Alkohol","F23_4_WARENVERFUEGBARKEIT_Alkohol","F23_5_WARENPRAESENTATION_Alkohol"))
  koerperpflege_ratings <- calculate_s6_ratings(customerdata, c("F25_1_SORTIMENT_Koerperpflege","F25_2_QUALITAET_Koerperpflege","F25_3_PREISLEISTUNG_Koerperpflege","F25_4_WARENVERFUEGBARKEIT_Koerperpflege","F25_5_WARENPRAESENTATION_Koerperpflege"))
  gegenstaende_ratings <- calculate_s6_ratings(customerdata, c("F26_1_SORTIMENT_Gegenstaende","F26_2_QUALITAET_Gegenstaende","F26_3_PREISLEISTUNG_Gegenstaende","F26_4_WARENVERFUEGBARKEIT_Gegenstaende","F26_5_WARENPRAESENTATION_Gegenstaende"))

  metric_frame <- data.frame(mean(snacks_ratings),
                           mean(bioprod_ratings),
                           mean(vegan_ratings),
                           mean(alkohol_ratings),
                           mean(koerperpflege_ratings),
                           mean(gegenstaende_ratings))

  result <- metric_frame  
}

edeka_metric <- calculate_s6_store_metric(filter(customerdata, F1_Edeka == 1))
rewe_metric <- calculate_s6_store_metric(filter(customerdata, F1_Rewe == 1))
lidl_metric <- calculate_s6_store_metric(filter(customerdata, F1_Lidl == 1))
kaufland_metric <- calculate_s6_store_metric(filter(customerdata, F1_Kaufland == 1))
aldinord_metric <- calculate_s6_store_metric(filter(customerdata, F1_AldiNord == 1))
real_metric <- calculate_s6_store_metric(filter(customerdata, F1_Real == 1))
dm_metric <- calculate_s6_store_metric(filter(customerdata, F1_DM == 1))
aldisud_metric <- calculate_s6_store_metric(filter(customerdata, F1_AldiSüd == 1))
nettorot_metric <- calculate_s6_store_metric(filter(customerdata, F1_NettoRot == 1))
amazon_metric <- calculate_s6_store_metric(filter(customerdata, F1_Amazon == 1))
penny_metric <- calculate_s6_store_metric(filter(customerdata, F1_Penny == 1))
rossmann_metric <- calculate_s6_store_metric(filter(customerdata, F1_Rossmann == 1))
norma_metric <- calculate_s6_store_metric(filter(customerdata, F1_Norma == 1))


dataset1 <- data.frame(c('Snacks', 'Bioprod' ,'Vegan','Alkohol','Koerperpflege','Gegenstaende'))
dataset1<- cbind(dataset1, t(edeka_metric))
dataset1 <- cbind(dataset1, t(rewe_metric))
dataset1 <- cbind(dataset1, t(lidl_metric))
dataset1 <- cbind(dataset1, t(kaufland_metric))
dataset1 <- cbind(dataset1, t(aldinord_metric))
dataset1 <- cbind(dataset1, t(real_metric))
dataset1 <- cbind(dataset1, t(dm_metric))
dataset1 <- cbind(dataset1, t(aldisud_metric))
dataset1 <- cbind(dataset1, t(nettorot_metric))
dataset1 <- cbind(dataset1, t(amazon_metric))
dataset1 <- cbind(dataset1, t(penny_metric))
dataset1 <- cbind(dataset1, t(rossmann_metric))
dataset1 <- cbind(dataset1, t(norma_metric))

colnames(dataset1) <- c('Sections','Edeka','Rewe','Lidl','Kaufland','Aldinord','Real','Dm','Aldisud','Nettorot','Amazon','Penny','Rossmann','Norma')
rownames(dataset1) <- c('Snacks', 'Bioprod' ,'Vegan','Alkohol','Koerperpflege','Gegenstaende')


d <- dataset1[-2,] %>% 
  rownames_to_column() %>% 
  gather(key = key, value = value, 'Edeka':'Norma') %>% 
  mutate(rowname = factor(rowname))

color.gradient <- function(x, colors=c("maroon","yellow","darkgreen"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

ggplot(d, aes(x = reorder(Sections, -value), y= value)) +
  geom_line(aes(group = key)) +
  geom_point(col=color.gradient(d$value), size=3) +
  facet_wrap( ~key, ncol=5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab('')+
  ylab('Satisfaction')

