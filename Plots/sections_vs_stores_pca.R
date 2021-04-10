## @knitr sections_vs_stores_pca

if (!require("dplyr")) install.packages("dplyr") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("haven")) install.packages("haven") 
if (!require("devtools")) install.packages("devtools") 
if (!require("fs")) install.packages("fs") 
if (!require("devtools")) install.packages("devtools") 
if (!require("ggbiplot")) install_github("vqv/ggbiplot") 

library(dplyr)
library(tidyverse)
library(haven)
library(devtools) 
library(ggbiplot) 

#customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

calculate_product_rating <- function(customerdata, metric_colnames) {
  metric_cols <- customerdata[metric_colnames]
  result <- colMeans(metric_cols, na.rm = TRUE, dims = 1)
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
  
  metric_frame <- data.frame(mean(snacks_ratings), 
                             mean(regional_ratings),
                             mean(bioprod_ratings),
                             mean(vegan_ratings),
                             mean(eigenmarke_ratings),
                             mean(marken_ratings),
                             mean(og_ratings),
                             mean(bb_ratings),
                             mean(fleish_ratings),
                             mean(wurst_ratings),
                             mean(molke_ratings),
                             mean(suess_ratings),
                             mean(alkohol_ratings),
                             mean(alkoholfrei_ratings),
                             mean(koerperpflege_ratings),
                             mean(gegenstaende_ratings))
  
  result <- metric_frame  
}


edeka_metric <- calculate_store_rating(filter(customerdata, F1_Edeka == 1))
rewe_metric <- calculate_store_rating(filter(customerdata, F1_Rewe == 1))
lidl_metric <- calculate_store_rating(filter(customerdata, F1_Lidl == 1))
kaufland_metric <- calculate_store_rating(filter(customerdata, F1_Kaufland == 1))
aldinord_metric <- calculate_store_rating(filter(customerdata, F1_AldiNord == 1))
real_metric <- calculate_store_rating(filter(customerdata, F1_Real == 1))
dm_metric <- calculate_store_rating(filter(customerdata, F1_DM == 1))
globus_metric <- calculate_store_rating(filter(customerdata, F1_Globus == 1))
coop_metric <- calculate_store_rating(filter(customerdata, F1_Coop == 1))
jibimarkt_metric <- calculate_store_rating(filter(customerdata, F1_JibiMarkt == 1))
nettoschwarz_metric <- calculate_store_rating(filter(customerdata, F1_NettoSchwarz == 1))
bofrost_metric <- calculate_store_rating(filter(customerdata, F1_Bofrost == 1))
vmarkt_metric <- calculate_store_rating(filter(customerdata, F1_VMarkt == 1))
alnatura_metric <- calculate_store_rating(filter(customerdata, F1_Alnatura == 1))
feneberg_metric <- calculate_store_rating(filter(customerdata, F1_Feneberg == 1))
markant_metric <- calculate_store_rating(filter(customerdata, F1_Markant == 1))
mixmarkt_metric <- calculate_store_rating(filter(customerdata, F1_MixMarkt == 1))
aldisud_metric <- calculate_store_rating(filter(customerdata, F1_AldiSüd == 1))
nettorot_metric <- calculate_store_rating(filter(customerdata, F1_NettoRot == 1))
amazon_metric <- calculate_store_rating(filter(customerdata, F1_Amazon == 1))
penny_metric <- calculate_store_rating(filter(customerdata, F1_Penny == 1))
rossmann_metric <- calculate_store_rating(filter(customerdata, F1_Rossmann == 1))
norma_metric <- calculate_store_rating(filter(customerdata, F1_Norma == 1))
combi_metric <- calculate_store_rating(filter(customerdata, F1_Combi == 1))
hit_metric <- calculate_store_rating(filter(customerdata, F1_Hit == 1))
tegut_metric <- calculate_store_rating(filter(customerdata, F1_Tegut == 1))
klasskock_metric <- calculate_store_rating(filter(customerdata, F1_KlaasKock == 1))
denss_metric <- calculate_store_rating(filter(customerdata, F1_Denns == 1))
budnikwosky_metric <- calculate_store_rating(filter(customerdata, F1_Budnikowsky == 1))
familia_metric <- calculate_store_rating(filter(customerdata, F1_Famila == 1))
nahfrish_metric <- calculate_store_rating(filter(customerdata, F1_nahfrisch == 1))
andere_metric <- calculate_store_rating(filter(customerdata, F1_Andere == 1))

dataset <- data.frame()
dataset<- rbind(dataset, edeka_metric)
dataset <- rbind(dataset, rewe_metric)
dataset <- rbind(dataset, lidl_metric)
dataset <- rbind(dataset, kaufland_metric)
dataset <- rbind(dataset, aldinord_metric)
dataset <- rbind(dataset, real_metric)
dataset <- rbind(dataset, dm_metric)
dataset <- rbind(dataset, globus_metric)
dataset <- rbind(dataset, coop_metric)
dataset <- rbind(dataset, jibimarkt_metric)
dataset <- rbind(dataset, nettoschwarz_metric)
dataset <- rbind(dataset, bofrost_metric)
dataset <- rbind(dataset, vmarkt_metric)
dataset <- rbind(dataset, alnatura_metric)
dataset <- rbind(dataset, feneberg_metric)
dataset <- rbind(dataset, markant_metric)
dataset <- rbind(dataset, mixmarkt_metric)
dataset <- rbind(dataset, aldisud_metric)
dataset <- rbind(dataset, nettorot_metric)
dataset <- rbind(dataset, amazon_metric)
dataset <- rbind(dataset, penny_metric)
dataset <- rbind(dataset, rossmann_metric)
dataset <- rbind(dataset, norma_metric)
dataset <- rbind(dataset, combi_metric)
dataset <- rbind(dataset, hit_metric)
dataset <- rbind(dataset, tegut_metric)
dataset <- rbind(dataset, klasskock_metric)
dataset <- rbind(dataset, denss_metric)
dataset <- rbind(dataset, budnikwosky_metric)
dataset <- rbind(dataset, familia_metric)
dataset <- rbind(dataset, nahfrish_metric)
dataset <- rbind(dataset, andere_metric)

rownames(dataset) <- c('Edeka','Rewe','Lidl','Kaufland','Aldinord','Real','Dm','Globus','Coop','Jibimarkt','Nettoschwarz','Bofrost','Vmarkt','Alnatura','Feneberg','Markant','Mixmarkt','Aldisud','Nettorot','Amazon','Penny','Rossmann','Norma','Combi','Hit','Tegut','Klasskock','Denss','Budnikwosky','Familia','Nahfrish','Andere')
colnames(dataset) <- c('Snacks','Regional','Bioprod','Vegan','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkohol','Alkoholfrei','Koerperpflege','Gegenstaende')

dataset <- subset(dataset, select = -c(Bioprod) )

dataset.pca <- prcomp(dataset, center = TRUE,scale. = TRUE)

summary(dataset.pca)

ggbiplot(dataset.pca, obs.scale = 0.8, var.scale = 1, choices=c(1,2), labels=rownames(dataset), ellipse = TRUE, 
         circle = TRUE) 
