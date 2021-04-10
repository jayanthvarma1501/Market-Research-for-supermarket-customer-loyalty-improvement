library(haven)
library(dplyr)
library(ggplot2)



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

"  metric_frame <- data.frame(snacks_ratings, 
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
"
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

library(tidyverse)
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

dataset <- subset(dataset, select = -c(b=Bioprod) )

dataset = as.data.frame(t(dataset))

d <- dataset %>% 
  rownames_to_column() %>% 
  gather(key = key, value = value, 'Edeka':'Andere') %>% 
  mutate(rowname = factor(rowname))
d
min(dataset)
mg <-  ggplot(d, aes(as.numeric(rowname),value, color = key)) + 
  geom_point(size=1) + 
 geom_line() +
  scale_x_continuous(name= '', labels = c('Snacks','Regional','Vegan','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkohol','Alkoholfrei','Koerperpflege','Gegenstaende'),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_y_continuous(name = "Likert Scale", labels = c('0','0.4','0.8','1.2','1.6','2','2.4','2.8','3.2','3.6','4','4.4','4.8','5'),
                     breaks = c(0,0.4,0.8,1.2,1.6,2,2.4,2.8,3.2,3.6,4,4.4,4.8,5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(values=c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975"))
mg



library(ggplot2)
#install.packages("ggiraph")
library(ggiraph)
#my_gg <- mg + geom_line_interactive(aes(x= x, y=y, colour = strata, tooltip= tooltip ), size = .75)
#my_gg <- mg + geom_line_interactive(size = 2)

my_gg <- mg + geom_point_interactive(aes(tooltip = key, data_id = key), size = 2)
x <- girafe(ggobj = my_gg, options = list(
  opts_hover(css = "fill:yellow;")
))
x


# gg <- ggplot(d,
#              aes(d, value, colour = key, tooltip = key, data_id = value,
#                  hover_css = "fill:none;")) +
#   geom_line_interactive(data=d, size = .75)
# x <- girafe(ggobj = gg)
# x <- girafe_options(x = x,
#                     opts_hover(css = "stroke:orange;fill:white") )
# print(x)
# 
# 
# my_gg <- mg + geom_line_interactive(size = 2)
# x <- girafe(ggobj = my_gg)
# x <- girafe_options(x = x,
#                     opts_hover(css = "stroke:red;fill:orange") )
# x
# 
# 
# 
# 
# 
# 
# 
# rlang::last_error()
# 
# areas <- c('a','b','c')
# scores <- c(0,1,2)
# scores2 <- c(01,12,32)
# scores3 <- c(04,51,22)
# f = data.frame("Name", "1", "2")
# f[nrow(f)+1,] = c("a", 2, 3)
# f[nrow(f)+1,] = c("aaa", 12, 53)
# f[nrow(f)+1,] = c("aa", 22, 43)
# ggplot(f,aes(x="Name", y=10)) + geom_line() 
# 
# mean(edeka_metric$snacks_ratings)
# 
# hist(edeka_metric$snacks_ratings, main="Satisfaction over Snacks", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$regional_ratings, main="Satisfaction over Regional", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$bioprod_ratings, main="Satisfaction over Bio-Prod", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$vegan_ratings, main="Satisfaction over Vegan", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$eigenmarke_ratings, main="Satisfaction over Eigenmarke", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$marken_ratings, main="Satisfaction over Marken", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$og_ratings, main="Satisfaction over Og", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$bb_ratings, main="Satisfaction over Bb", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$fleish_ratings, main="Satisfaction over Fleish", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$wurst_ratings, main="Satisfaction over Wurst", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$molke_ratings, main="Satisfaction over Molke", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$suess_ratings, main="Satisfaction over Suess", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$alkohol_ratings, main="Satisfaction over Alkohol", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$alkoholfrei_ratings, main="Satisfaction over Alkohofrei", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$koerperpflege_ratings, main="Satisfaction over Koerperpflege", xlab = "Ratings", ylab = "Customers")
# hist(edeka_metric$gegenstaende_ratings, main="Satisfaction over Gegenstaende", xlab = "Ratings", ylab = "Customers")
# 
# 
# dataset2 <- data.frame(CW = c(52095, 28312, 46430, 20385),
#                       BF = c(12456, 1132, 39640, 5660),
#                       TF = c(2265, 0, 0, 0),
#                       CE = c(0, 0, 3397, 0),
#                       row.names = c("Cla", "Clb", "Clc", "Cld"))
# 
# 
# library(tidyverse)
# dataset <- data.frame()
# 
# 
# 
# CW <- c( 5095, 8312, 46430, 20385)
# CF <- c( 5205, 2312, 46430, 20385)
# TF <- c( 55, 2812, 46430, 20385)
# CE <- c( 595, 2831, 46430, 20385)
# dataset <- rbind(dataset, CW)
# dataset <- rbind(dataset,  BF)
# dataset <- rbind(dataset,  TF)
# dataset <- rbind(dataset,  CE)
# colnames(dataset) <- c('CW','BF','TF','CE')
# rownames(dataset) <- c('Cla','Clb','Clc','Cld')
# 
# 
# dataset %>% 
#   rownames_to_column() %>% 
#   gather(key = key, value = value, CW:CE) %>% 
#   mutate(rowname = factor(rowname)) 
# /%>% 
#   ggplot(aes(as.numeric(rowname), value, color = key)) + 
#   geom_point() + 
#   geom_line() +
#   scale_x_continuous(name = "Class", labels = c("Cla", "Clb", "Clc", "Cld")) +
#   theme_bw()
