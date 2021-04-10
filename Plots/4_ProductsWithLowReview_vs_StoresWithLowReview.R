## @knitr ProductsWithLowReview_vs_StoresWithLowReview

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

dataset1 <- data.frame(c('Snacks', 'Bioprod' ,'Vegan','Alkohol','Koerperpflege','Gegenstaende'))
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
rownames(dataset1) <- c('Snacks', 'Bioprod' ,'Vegan','Alkohol','Koerperpflege','Gegenstaende')

d <- dataset1[-2,] %>% 
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
