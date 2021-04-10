##  @knitr products_vs_reviews

if (!require("dplyr")) install.packages("dplyr") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("haven")) install.packages("haven") 
if (!require("ggplot2")) install.packages("ggplot2") 

library(haven)
library(dplyr)
library(ggplot2)

#customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

calculate_product_rating <- function(customerdata_, metric_colnames) {
  metric_cols <- customerdata[metric_colnames]
  result <- sum(!is.na(metric_cols))/ncol(metric_cols)  
}

calculate_store_rating <- function(customerdata) {
  snacks_ratings <- calculate_product_rating(customerdata, c("F11_1_SORTIMENT_Snacks","F11_2_PREISLEISTUNG_Snacks","F11_3_WARENVERFUEGBARKEIT_Snacks"))
  regional_ratings <- calculate_product_rating(customerdata, c("F12_1_SORTIMENT_Regional", "F12_2_PREISLEISTUNG_Regional","F12_3_WARENVERFUEGBARKEIT_Regional"))
  bioprod_ratings <- calculate_product_rating(customerdata, c("F13_1_SORTIMENT_BioProd","F13_2_PREISLEISTUNG_BioProd","F13_3_WARENVERFUEGBARKEIT_BioProd"))
  vegan_ratings <- calculate_product_rating(customerdata, c("F14_1_SORTIMENT_Vegan","F14_2_PREISLEISTUNG_Vegan","F14_3_WARENVERFUEGBARKEIT_Vegan"))
  eigenmarke_ratings <- calculate_product_rating(customerdata, c("F15_1_SORTIMENT_Eigenmarke","F15_2_PREISLEISTUNG_Eigenmarke","F15_3_WARENVERFUEGBARKEIT_Eigenmarke"))
  marken_ratings <- calculate_product_rating(customerdata, c("F16_1_SORTIMENT_Marken","F16_2_PREISLEISTUNG_Marken","F16_3_WARENVERFUEGBARKEIT_Marken"))
  og_ratings <- calculate_product_rating(customerdata, c("F17_1_SORTIMENT_OG","F17_2_QUALITAET_OG","F17_3_FRISCHE_OG","F17_4_PREISLEISTUNG_OG","F17_5_WARENVERFUEGBARKEIT_OG","F17_6_WARENPRAESENTATION_OG"))
  bb_ratings <- calculate_product_rating(customerdata, c("F18_1_SORTIMENT_BB","F18_2_QUALITAET_BB","F18_3_FRISCHE_BB","F18_4_PREISLEISTUNG_BB","F18_5_WARENVERFUEGBARKEIT_BB","F18_6_WARENPRAESENTATION_BB"))
  fleish_ratings <- calculate_product_rating(customerdata, c("F19_1_SORTIMENT_Fleisch","F19_2_QUALITAET_Fleisch","F19_3_FRISCHE_Fleisch","F19_4_PREISLEISTUNG_Fleisch","F19_5_WARENVERFUEGBARKEIT_Fleisch", "F19_6_WARENPRAESENTATION_Fleisch"))
  wurst_ratings <- calculate_product_rating(customerdata, c("F20_1_SORTIMENT_Wurst","F20_2_QUALITAET_Wurst","F20_3_FRISCHE_Wurst","F20_4_PREISLEISTUNG_Wurst","F20_5_WARENVERFUEGBARKEIT_Wurst","F20_6_WARENPRAESENTATION_Wurst"))
  molke_ratings <- calculate_product_rating(customerdata, c("F21_1_SORTIMENT_Molke","F21_2_QUALITAET_Molke","F21_3_FRISCHE_Molke","F21_4_PREISLEISTUNG_Molke","F21_5_WARENVERFUEGBARKEIT_Molke","F21_6_WARENPRAESENTATION_Molke"))
  suess_ratings <- calculate_product_rating(customerdata, c("F22_1_SORTIMENT_Suess","F22_2_QUALITAET_Suess","F22_3_FRISCHE_Suess","F22_4_PREISLEISTUNG_Suess","F22_5_WARENVERFUEGBARKEIT_Suess","F22_6_WARENPRAESENTATION_Suess"))
  alkohol_ratings <- calculate_product_rating(customerdata, c("F23_1_SORTIMENT_Alkohol","F23_2_QUALITAET_Alkohol","F23_3_PREISLEISTUNG_Alkohol","F23_4_WARENVERFUEGBARKEIT_Alkohol","F23_5_WARENPRAESENTATION_Alkohol"))
  alkoholfrei_ratings <- calculate_product_rating(customerdata, c("F24_1_SORTIMENT_Alkoholfrei","F24_2_QUALITAET_Alkoholfrei","F24_3_PREISLEISTUNG_Alkoholfrei","F24_4_WARENVERFUEGBARKEIT_Alkoholfrei","F24_5_WARENPRAESENTATION_Alkoholfrei"))
  koerperpflege_ratings <- calculate_product_rating(customerdata, c("F25_1_SORTIMENT_Koerperpflege","F25_2_QUALITAET_Koerperpflege","F25_3_PREISLEISTUNG_Koerperpflege","F25_4_WARENVERFUEGBARKEIT_Koerperpflege","F25_5_WARENPRAESENTATION_Koerperpflege"))
  gegenstaende_ratings <- calculate_product_rating(customerdata, c("F26_1_SORTIMENT_Gegenstaende","F26_2_QUALITAET_Gegenstaende","F26_3_PREISLEISTUNG_Gegenstaende","F26_4_WARENVERFUEGBARKEIT_Gegenstaende","F26_5_WARENPRAESENTATION_Gegenstaende"))
  
  metric_frame <- c(snacks_ratings, 
                    regional_ratings,
                    bioprod_ratings,
                    vegan_ratings,
                    eigenmarke_ratings,
                    marken_ratings,
                    og_ratings,
                    bb_ratings,
                    fleish_ratings,
                    wurst_ratings,
                    molke_ratings,
                    suess_ratings,
                    alkohol_ratings,
                    alkoholfrei_ratings,
                    koerperpflege_ratings,
                    gegenstaende_ratings)
  
  result <- metric_frame  
}

metric <- calculate_store_rating(customerdata)

dataset <- data.frame()
dataset<- rbind(dataset, metric)
dataset<- rbind(dataset, c('Snacks','Regional','Bioprod','Vegan','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkohol','Alkoholfrei','Koerperpflege','Gegenstaende'))

colnames(dataset) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
rownames(dataset) <- c("Reviews", "Sections")
dataset = as.data.frame(t(dataset))

ggplot(dataset, aes(y=reorder(Sections, as.numeric(Reviews)), x=as.numeric(Reviews), label=Reviews)) +
  geom_bar(stat = 'identity', aes(fill = as.numeric(Reviews))) +
  scale_fill_gradient2(low='red', mid='red', high='green', name='') +
  geom_text(size = 3, position = position_stack(vjust = 1), color='Black') +
  geom_vline(xintercept=1500, linetype="dashed", color = "blue", size=1) +
  xlab("Reviews") + 
  ylab("") +
  coord_cartesian(xlim = c(100,2303)) 
  #scale_x_continuous(breaks = c(200, 400, 800, 1200))