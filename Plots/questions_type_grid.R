## @knitr questions_type_grid

if (!require("dplyr")) install.packages("dplyr") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("haven")) install.packages("haven") 
if (!require("ggplot2")) install.packages("ggplot2") 

library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)

#customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

calculate_product_rating <- function(customerdata, metric_colnames) {
  metric_cols <- customerdata[metric_colnames]
  result <- mean(colMeans(metric_cols, na.rm = TRUE, dims = 1))
}

calculate_store_rating <- function(customerdata) {
  diversity_ratings <- calculate_product_rating(customerdata, c("F11_1_SORTIMENT_Snacks","F12_1_SORTIMENT_Regional","F13_1_SORTIMENT_BioProd","F14_1_SORTIMENT_Vegan","F15_1_SORTIMENT_Eigenmarke","F16_1_SORTIMENT_Marken","F17_1_SORTIMENT_OG","F17_2_QUALITAET_OG","F18_1_SORTIMENT_BB","F19_1_SORTIMENT_Fleisch","F20_1_SORTIMENT_Wurst","F21_1_SORTIMENT_Molke","F22_1_SORTIMENT_Suess","F23_1_SORTIMENT_Alkohol","F24_1_SORTIMENT_Alkoholfrei","F25_1_SORTIMENT_Koerperpflege","F26_1_SORTIMENT_Gegenstaende"))
  price_ratings <- calculate_product_rating(customerdata, c("F11_2_PREISLEISTUNG_Snacks","F12_2_PREISLEISTUNG_Regional","F13_2_PREISLEISTUNG_BioProd","F14_2_PREISLEISTUNG_Vegan","F15_2_PREISLEISTUNG_Eigenmarke","F16_2_PREISLEISTUNG_Marken","F17_4_PREISLEISTUNG_OG","F18_4_PREISLEISTUNG_BB","F19_4_PREISLEISTUNG_Fleisch","F20_4_PREISLEISTUNG_Wurst","F21_4_PREISLEISTUNG_Molke","F22_4_PREISLEISTUNG_Suess","F23_3_PREISLEISTUNG_Alkohol","F24_3_PREISLEISTUNG_Alkoholfrei","F25_3_PREISLEISTUNG_Koerperpflege","F26_3_PREISLEISTUNG_Gegenstaende"))
  availability_ratings <- calculate_product_rating(customerdata, c("F11_3_WARENVERFUEGBARKEIT_Snacks","F12_3_WARENVERFUEGBARKEIT_Regional","F13_3_WARENVERFUEGBARKEIT_BioProd","F14_3_WARENVERFUEGBARKEIT_Vegan","F15_3_WARENVERFUEGBARKEIT_Eigenmarke","F16_3_WARENVERFUEGBARKEIT_Marken","F17_5_WARENVERFUEGBARKEIT_OG","F18_5_WARENVERFUEGBARKEIT_BB","F19_5_WARENVERFUEGBARKEIT_Fleisch","F20_5_WARENVERFUEGBARKEIT_Wurst","F21_5_WARENVERFUEGBARKEIT_Molke","F22_5_WARENVERFUEGBARKEIT_Suess","F23_4_WARENVERFUEGBARKEIT_Alkohol","F24_4_WARENVERFUEGBARKEIT_Alkoholfrei","F25_4_WARENVERFUEGBARKEIT_Koerperpflege","F26_4_WARENVERFUEGBARKEIT_Gegenstaende"))
  quality_ratings <- calculate_product_rating(customerdata, c("F17_2_QUALITAET_OG","F18_2_QUALITAET_BB","F19_2_QUALITAET_Fleisch","F20_2_QUALITAET_Wurst","F21_2_QUALITAET_Molke","F22_2_QUALITAET_Suess","F23_2_QUALITAET_Alkohol","F24_2_QUALITAET_Alkoholfrei","F25_2_QUALITAET_Koerperpflege","F26_2_QUALITAET_Gegenstaende"))
  freshness_ratings <- calculate_product_rating(customerdata, c("F17_3_FRISCHE_OG","F18_3_FRISCHE_BB","F19_3_FRISCHE_Fleisch","F20_3_FRISCHE_Wurst","F21_3_FRISCHE_Molke","F22_3_FRISCHE_Suess"))
  presentation_ratings <- calculate_product_rating(customerdata, c("F17_6_WARENPRAESENTATION_OG","F18_6_WARENPRAESENTATION_BB", "F19_6_WARENPRAESENTATION_Fleisch","F20_6_WARENPRAESENTATION_Wurst","F21_6_WARENPRAESENTATION_Molke","F22_6_WARENPRAESENTATION_Suess","F23_5_WARENPRAESENTATION_Alkohol","F24_5_WARENPRAESENTATION_Alkoholfrei","F25_5_WARENPRAESENTATION_Koerperpflege","F26_5_WARENPRAESENTATION_Gegenstaende"))


  metric_frame <- data.frame(diversity_ratings, 
                             price_ratings,
                             availability_ratings,
                             quality_ratings,
                             freshness_ratings,
                             presentation_ratings)
  
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
colnames(dataset) <- c('Diversity Range','Price and Performance','Goods Availability','Quality','Freshness','Presentation')

dataset = as.data.frame(t(dataset))

dataset1 <- data.frame(c('Diversity Range','Price and Performance','Goods Availability','Quality','Freshness','Presentation'))
dataset1 <- cbind(dataset1, t(edeka_metric))
dataset1 <- cbind(dataset1, t(rewe_metric))
dataset1 <- cbind(dataset1, t(lidl_metric))
dataset1 <- cbind(dataset1, t(kaufland_metric))
dataset1 <- cbind(dataset1, t(aldinord_metric))
dataset1 <- cbind(dataset1, t(real_metric))
dataset1 <- cbind(dataset1, t(dm_metric))
dataset1 <- cbind(dataset1, t(globus_metric))
dataset1 <- cbind(dataset1, t(coop_metric))
dataset1 <- cbind(dataset1, t(jibimarkt_metric))
dataset1 <- cbind(dataset1, t(nettoschwarz_metric))
dataset1 <- cbind(dataset1, t(bofrost_metric))
dataset1 <- cbind(dataset1, t(vmarkt_metric))
dataset1 <- cbind(dataset1, t(alnatura_metric))
dataset1 <- cbind(dataset1, t(feneberg_metric))
dataset1 <- cbind(dataset1, t(markant_metric))
dataset1 <- cbind(dataset1, t(mixmarkt_metric))
dataset1 <- cbind(dataset1, t(aldisud_metric))
dataset1 <- cbind(dataset1, t(nettorot_metric))
dataset1 <- cbind(dataset1, t(amazon_metric))
dataset1 <- cbind(dataset1, t(penny_metric))
dataset1 <- cbind(dataset1, t(rossmann_metric))
dataset1 <- cbind(dataset1, t(norma_metric))
dataset1 <- cbind(dataset1, t(combi_metric))
dataset1 <- cbind(dataset1, t(hit_metric))
dataset1 <- cbind(dataset1, t(tegut_metric))
dataset1 <- cbind(dataset1, t(klasskock_metric))
dataset1 <- cbind(dataset1, t(denss_metric))
dataset1 <- cbind(dataset1, t(budnikwosky_metric))
dataset1 <- cbind(dataset1, t(familia_metric))
dataset1 <- cbind(dataset1, t(nahfrish_metric))
dataset1 <- cbind(dataset1, t(andere_metric))

colnames(dataset1) <- c('Sections','Edeka','Rewe','Lidl','Kaufland','Aldinord','Real','Dm','Globus','Coop','Jibimarkt','Nettoschwarz','Bofrost','Vmarkt','Alnatura','Feneberg','Markant','Mixmarkt','Aldisud','Nettorot','Amazon','Penny','Rossmann','Norma','Combi','Hit','Tegut','Klasskock','Denss','Budnikwosky','Familia','Nahfrish','Andere')
rownames(dataset1) <- c('Diversity Range','Price and Performance','Goods Availability','Quality','Freshness','Presentation')

d <- dataset1 %>% 
  rownames_to_column() %>% 
  gather(key = key, value = value, 'Edeka':'Andere') %>% 
  mutate(rowname = factor(rowname))

ggplot(d, aes(x = reorder(key, -value), y= round(value, digits=2), label=round(value, digits=2))) +
  geom_bar(stat='identity', aes(group = Sections, fill = value)) +
  geom_line(aes(group = Sections), color='red', size=1) +
  geom_point(color='darkgreen', size=2) +
  geom_text(size = 3, angle=90, position = position_stack(vjust = 0.5), color='black') +
  scale_fill_gradient(low='red', high='green') +
  facet_wrap( ~Sections, ncol=1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey50")) +
  xlab('') +
  ylab('Satisfaction')

#ggplotly(x)
