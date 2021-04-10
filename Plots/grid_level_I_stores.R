library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

calculate_s6_ratings <- function(customerdata, metric_colnames) {
  metric_cols <- customerdata[metric_colnames]
  
  
  result <- mean(colMeans(metric_cols, na.rm = TRUE, dims = 1))
}

calculate_s6_store_metric <- function(customerdata) {
  snacks_ratings <- calculate_s6_ratings(customerdata, c("F11_1_SORTIMENT_Snacks","F11_2_PREISLEISTUNG_Snacks","F11_3_WARENVERFUEGBARKEIT_Snacks"))
  regional_ratings <- calculate_s6_ratings(customerdata, c("F12_1_SORTIMENT_Regional", "F12_2_PREISLEISTUNG_Regional","F12_3_WARENVERFUEGBARKEIT_Regional"))
  bioprod_ratings <- calculate_s6_ratings(customerdata, c("F13_1_SORTIMENT_BioProd","F13_2_PREISLEISTUNG_BioProd","F13_3_WARENVERFUEGBARKEIT_BioProd"))
  vegan_ratings <- calculate_s6_ratings(customerdata, c("F14_1_SORTIMENT_Vegan","F14_2_PREISLEISTUNG_Vegan","F14_3_WARENVERFUEGBARKEIT_Vegan"))
  eigenmarke_ratings <- calculate_s6_ratings(customerdata, c("F15_1_SORTIMENT_Eigenmarke","F15_2_PREISLEISTUNG_Eigenmarke","F15_3_WARENVERFUEGBARKEIT_Eigenmarke"))
  marken_ratings <- calculate_s6_ratings(customerdata, c("F16_1_SORTIMENT_Marken","F16_2_PREISLEISTUNG_Marken","F16_3_WARENVERFUEGBARKEIT_Marken"))
  og_ratings <- calculate_s6_ratings(customerdata, c("F17_1_SORTIMENT_OG","F17_2_QUALITAET_OG","F17_3_FRISCHE_OG","F17_4_PREISLEISTUNG_OG","F17_5_WARENVERFUEGBARKEIT_OG","F17_6_WARENPRAESENTATION_OG"))
  bb_ratings <- calculate_s6_ratings(customerdata, c("F18_1_SORTIMENT_BB","F18_2_QUALITAET_BB","F18_3_FRISCHE_BB","F18_4_PREISLEISTUNG_BB","F18_5_WARENVERFUEGBARKEIT_BB","F18_6_WARENPRAESENTATION_BB"))
  fleish_ratings <- calculate_s6_ratings(customerdata, c("F19_1_SORTIMENT_Fleisch","F19_2_QUALITAET_Fleisch","F19_3_FRISCHE_Fleisch","F19_4_PREISLEISTUNG_Fleisch","F19_5_WARENVERFUEGBARKEIT_Fleisch", "F19_6_WARENPRAESENTATION_Fleisch"))
  wurst_ratings <- calculate_s6_ratings(customerdata, c("F20_1_SORTIMENT_Wurst","F20_2_QUALITAET_Wurst","F20_3_FRISCHE_Wurst","F20_4_PREISLEISTUNG_Wurst","F20_5_WARENVERFUEGBARKEIT_Wurst","F20_6_WARENPRAESENTATION_Wurst"))
  molke_ratings <- calculate_s6_ratings(customerdata, c("F21_1_SORTIMENT_Molke","F21_2_QUALITAET_Molke","F21_3_FRISCHE_Molke","F21_4_PREISLEISTUNG_Molke","F21_5_WARENVERFUEGBARKEIT_Molke","F21_6_WARENPRAESENTATION_Molke"))
  suess_ratings <- calculate_s6_ratings(customerdata, c("F22_1_SORTIMENT_Suess","F22_2_QUALITAET_Suess","F22_3_FRISCHE_Suess","F22_4_PREISLEISTUNG_Suess","F22_5_WARENVERFUEGBARKEIT_Suess","F22_6_WARENPRAESENTATION_Suess"))
  alkohol_ratings <- calculate_s6_ratings(customerdata, c("F23_1_SORTIMENT_Alkohol","F23_2_QUALITAET_Alkohol","F23_3_PREISLEISTUNG_Alkohol","F23_4_WARENVERFUEGBARKEIT_Alkohol","F23_5_WARENPRAESENTATION_Alkohol"))
  alkoholfrei_ratings <- calculate_s6_ratings(customerdata, c("F24_1_SORTIMENT_Alkoholfrei","F24_2_QUALITAET_Alkoholfrei","F24_3_PREISLEISTUNG_Alkoholfrei","F24_4_WARENVERFUEGBARKEIT_Alkoholfrei","F24_5_WARENPRAESENTATION_Alkoholfrei"))
  koerperpflege_ratings <- calculate_s6_ratings(customerdata, c("F25_1_SORTIMENT_Koerperpflege","F25_2_QUALITAET_Koerperpflege","F25_3_PREISLEISTUNG_Koerperpflege","F25_4_WARENVERFUEGBARKEIT_Koerperpflege","F25_5_WARENPRAESENTATION_Koerperpflege"))
  gegenstaende_ratings <- calculate_s6_ratings(customerdata, c("F26_1_SORTIMENT_Gegenstaende","F26_2_QUALITAET_Gegenstaende","F26_3_PREISLEISTUNG_Gegenstaende","F26_4_WARENVERFUEGBARKEIT_Gegenstaende","F26_5_WARENPRAESENTATION_Gegenstaende"))
  
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


customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

edeka_metric <- calculate_s6_store_metric(filter(customerdata, F1_Edeka == 1))
rewe_metric <- calculate_s6_store_metric(filter(customerdata, F1_Rewe == 1))
lidl_metric <- calculate_s6_store_metric(filter(customerdata, F1_Lidl == 1))
kaufland_metric <- calculate_s6_store_metric(filter(customerdata, F1_Kaufland == 1))
aldinord_metric <- calculate_s6_store_metric(filter(customerdata, F1_AldiNord == 1))
real_metric <- calculate_s6_store_metric(filter(customerdata, F1_Real == 1))
dm_metric <- calculate_s6_store_metric(filter(customerdata, F1_DM == 1))
globus_metric <- calculate_s6_store_metric(filter(customerdata, F1_Globus == 1))
coop_metric <- calculate_s6_store_metric(filter(customerdata, F1_Coop == 1))
jibimarkt_metric <- calculate_s6_store_metric(filter(customerdata, F1_JibiMarkt == 1))
nettoschwarz_metric <- calculate_s6_store_metric(filter(customerdata, F1_NettoSchwarz == 1))
bofrost_metric <- calculate_s6_store_metric(filter(customerdata, F1_Bofrost == 1))
vmarkt_metric <- calculate_s6_store_metric(filter(customerdata, F1_VMarkt == 1))
alnatura_metric <- calculate_s6_store_metric(filter(customerdata, F1_Alnatura == 1))
feneberg_metric <- calculate_s6_store_metric(filter(customerdata, F1_Feneberg == 1))
markant_metric <- calculate_s6_store_metric(filter(customerdata, F1_Markant == 1))
mixmarkt_metric <- calculate_s6_store_metric(filter(customerdata, F1_MixMarkt == 1))
aldisud_metric <- calculate_s6_store_metric(filter(customerdata, F1_AldiSüd == 1))
nettorot_metric <- calculate_s6_store_metric(filter(customerdata, F1_NettoRot == 1))
amazon_metric <- calculate_s6_store_metric(filter(customerdata, F1_Amazon == 1))
penny_metric <- calculate_s6_store_metric(filter(customerdata, F1_Penny == 1))
rossmann_metric <- calculate_s6_store_metric(filter(customerdata, F1_Rossmann == 1))
norma_metric <- calculate_s6_store_metric(filter(customerdata, F1_Norma == 1))
combi_metric <- calculate_s6_store_metric(filter(customerdata, F1_Combi == 1))
hit_metric <- calculate_s6_store_metric(filter(customerdata, F1_Hit == 1))
tegut_metric <- calculate_s6_store_metric(filter(customerdata, F1_Tegut == 1))
klasskock_metric <- calculate_s6_store_metric(filter(customerdata, F1_KlaasKock == 1))
denss_metric <- calculate_s6_store_metric(filter(customerdata, F1_Denns == 1))
budnikwosky_metric <- calculate_s6_store_metric(filter(customerdata, F1_Budnikowsky == 1))
familia_metric <- calculate_s6_store_metric(filter(customerdata, F1_Famila == 1))
nahfrish_metric <- calculate_s6_store_metric(filter(customerdata, F1_nahfrisch == 1))
andere_metric <- calculate_s6_store_metric(filter(customerdata, F1_Andere == 1))
## Section 6

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

dataset <- subset(dataset, select = -c(bioprod) )

dataset = as.data.frame(t(dataset))

# d <- dataset %>% 
#   rownames_to_column() %>% 
#   gather(key = key, value = value, 'edeka':'norma') %>% 
#   mutate(rowname = factor(rowname))
# 
# mg <-  ggplot(d, aes(value, as.numeric(rowname), color = key)) + 
#   geom_point(size=3, aes(shape=key)) + 
#   scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
#   #  geom_line() +
#   scale_y_continuous(name = "Class", labels = c('regional','eigenmarke','marken','og','bb','fleish','wurst','molke','suess','alkoholfrei'),
#                      breaks = c(1,2,3,4,5,6,7,8,9,10)) +
#   scale_x_continuous(name = "Scale", labels = c(4.0,4.05,4.1,4.15,4.2,4.25,4.3,4.35),
#                      breaks = c(4.0,4.05,4.1,4.15,4.2,4.25,4.3,4.35)) +
#   theme_bw() +
#   #theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   scale_color_manual(values=c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975"))
# mg







####
dataset1 <- data.frame(c('Snacks','Regional','Bioprod','Vegan','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkohol','Alkoholfrei','Koerperpflege','Gegenstaende'))
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
rownames(dataset1) <- c('Snacks','Regional','Bioprod','Vegan','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkohol','Alkoholfrei','Koerperpflege','Gegenstaende')


####

# dataset1 %>% 
#   ggplot(aes(x = Sections)) +
#   geom_bar(stat = "count") # default

dataset1[-3,]

d <- dataset1[-3,] %>% 
  rownames_to_column() %>% 
  gather(key = key, value = value, 'Edeka':'Norma') %>% 
  mutate(rowname = factor(rowname))

# ggplot(d, aes(x = reorder(key, -value), y= value)) +
#   geom_line(aes(group = sections)) +
#   geom_point() +
#   facet_wrap( ~sections, ncol=3) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(d, aes(x = reorder(key, -value), y= round(value, digits=2), label=round(value, digits=2))) +
  geom_bar(stat='identity', aes(group = Sections, fill = value)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color='black') +
  scale_fill_gradient(low='red', high='green') +
  facet_wrap( ~Sections, ncol=3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey50")) +
  xlab('') +
  ylab('Satisfaction')
