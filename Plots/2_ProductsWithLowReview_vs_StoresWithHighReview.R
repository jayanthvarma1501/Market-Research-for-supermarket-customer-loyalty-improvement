## @knitr ProductsWithLowReview_vs_StoresWithHighReview

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
  regional_ratings <- calculate_s6_ratings(customerdata, c("F12_1_SORTIMENT_Regional", "F12_2_PREISLEISTUNG_Regional","F12_3_WARENVERFUEGBARKEIT_Regional"))
  eigenmarke_ratings <- calculate_s6_ratings(customerdata, c("F15_1_SORTIMENT_Eigenmarke","F15_2_PREISLEISTUNG_Eigenmarke","F15_3_WARENVERFUEGBARKEIT_Eigenmarke"))
  marken_ratings <- calculate_s6_ratings(customerdata, c("F16_1_SORTIMENT_Marken","F16_2_PREISLEISTUNG_Marken","F16_3_WARENVERFUEGBARKEIT_Marken"))
  og_ratings <- calculate_s6_ratings(customerdata, c("F17_1_SORTIMENT_OG","F17_2_QUALITAET_OG","F17_3_FRISCHE_OG","F17_4_PREISLEISTUNG_OG","F17_5_WARENVERFUEGBARKEIT_OG","F17_6_WARENPRAESENTATION_OG"))
  bb_ratings <- calculate_s6_ratings(customerdata, c("F18_1_SORTIMENT_BB","F18_2_QUALITAET_BB","F18_3_FRISCHE_BB","F18_4_PREISLEISTUNG_BB","F18_5_WARENVERFUEGBARKEIT_BB","F18_6_WARENPRAESENTATION_BB"))
  fleish_ratings <- calculate_s6_ratings(customerdata, c("F19_1_SORTIMENT_Fleisch","F19_2_QUALITAET_Fleisch","F19_3_FRISCHE_Fleisch","F19_4_PREISLEISTUNG_Fleisch","F19_5_WARENVERFUEGBARKEIT_Fleisch", "F19_6_WARENPRAESENTATION_Fleisch"))
  wurst_ratings <- calculate_s6_ratings(customerdata, c("F20_1_SORTIMENT_Wurst","F20_2_QUALITAET_Wurst","F20_3_FRISCHE_Wurst","F20_4_PREISLEISTUNG_Wurst","F20_5_WARENVERFUEGBARKEIT_Wurst","F20_6_WARENPRAESENTATION_Wurst"))
  molke_ratings <- calculate_s6_ratings(customerdata, c("F21_1_SORTIMENT_Molke","F21_2_QUALITAET_Molke","F21_3_FRISCHE_Molke","F21_4_PREISLEISTUNG_Molke","F21_5_WARENVERFUEGBARKEIT_Molke","F21_6_WARENPRAESENTATION_Molke"))
  suess_ratings <- calculate_s6_ratings(customerdata, c("F22_1_SORTIMENT_Suess","F22_2_QUALITAET_Suess","F22_3_FRISCHE_Suess","F22_4_PREISLEISTUNG_Suess","F22_5_WARENVERFUEGBARKEIT_Suess","F22_6_WARENPRAESENTATION_Suess"))
  alkoholfrei_ratings <- calculate_s6_ratings(customerdata, c("F24_1_SORTIMENT_Alkoholfrei","F24_2_QUALITAET_Alkoholfrei","F24_3_PREISLEISTUNG_Alkoholfrei","F24_4_WARENVERFUEGBARKEIT_Alkoholfrei","F24_5_WARENPRAESENTATION_Alkoholfrei"))
  
  metric_frame <- data.frame(mean(regional_ratings),
                             mean(eigenmarke_ratings),
                             mean(marken_ratings),
                             mean(og_ratings),
                             mean(bb_ratings),
                             mean(fleish_ratings),
                             mean(wurst_ratings),
                             mean(molke_ratings),
                             mean(suess_ratings),
                             mean(alkoholfrei_ratings))
  
  result <- metric_frame  
}

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
combi_metric <- calculate_s6_store_metric(filter(customerdata, F1_Combi == 1))
hit_metric <- calculate_s6_store_metric(filter(customerdata, F1_Hit == 1))
tegut_metric <- calculate_s6_store_metric(filter(customerdata, F1_Tegut == 1))
klasskock_metric <- calculate_s6_store_metric(filter(customerdata, F1_KlaasKock == 1))
denss_metric <- calculate_s6_store_metric(filter(customerdata, F1_Denns == 1))
budnikwosky_metric <- calculate_s6_store_metric(filter(customerdata, F1_Budnikowsky == 1))
familia_metric <- calculate_s6_store_metric(filter(customerdata, F1_Famila == 1))
nahfrish_metric <- calculate_s6_store_metric(filter(customerdata, F1_nahfrisch == 1))
andere_metric <- calculate_s6_store_metric(filter(customerdata, F1_Andere == 1))


dataset1 <- data.frame(c('Regional','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkoholfrei'))
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
dataset1 <- cbind(dataset1, t(combi_metric))
dataset1 <- cbind(dataset1, t(hit_metric))
dataset1 <- cbind(dataset1, t(tegut_metric))
dataset1 <- cbind(dataset1, t(klasskock_metric))
dataset1 <- cbind(dataset1, t(denss_metric))
dataset1 <- cbind(dataset1, t(budnikwosky_metric))
dataset1 <- cbind(dataset1, t(familia_metric))
dataset1 <- cbind(dataset1, t(nahfrish_metric))
dataset1 <- cbind(dataset1, t(andere_metric))

colnames(dataset1) <- c('Sections','Globus','Coop','Jibimarkt','Nettoschwarz','Bofrost','Vmarkt','Alnatura','Feneberg','Markant','Mixmarkt','Combi','Hit','Tegut','Klasskock','Denss','Budnikwosky','Familia','Nahfrish','Andere')
rownames(dataset1) <- c('Regional','Eigenmarke','Marken','Og','Bb','Fleish','Wurst','Molke','Suess','Alkoholfrei')

d <- dataset1 %>% 
  rownames_to_column() %>% 
  gather(key = key, value = value, 'Globus':'Andere') %>% 
  mutate(rowname = factor(rowname))

color.gradient <- function(x, colors=c("maroon","yellow","darkgreen"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

ggplot(d, aes(x = reorder(key, -value), y= value)) +
  geom_line(aes(group = Sections)) +
  geom_point(col=color.gradient(d$value), size=3) +
  facet_wrap( ~Sections, ncol=5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab('')+
  ylab('Satisfaction')