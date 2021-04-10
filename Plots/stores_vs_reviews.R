## @knitr stores_vs_reviews

if (!require("dplyr")) install.packages("dplyr") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("haven")) install.packages("haven") 
if (!require("ggplot2")) install.packages("ggplot2") 

library(haven)
library(dplyr)
library(ggplot2)

#customerdata <- read_sav("D:\\Study\\OVGU\\DSwR\\Datensatz PASCAL_EP_18.02.20.sav")

calculate_store_rating <- function(customerdata) {
  result <- nrow(customerdata)  
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

dataset <- cbind(dataset, c('Edeka','Rewe','Lidl','Kaufland','Aldinord','Real','Dm','Globus','Coop','Jibimarkt','Nettoschwarz','Bofrost','Vmarkt','Alnatura','Feneberg','Markant','Mixmarkt','Aldisud','Nettorot','Amazon','Penny','Rossmann','Norma','Combi','Hit','Tegut','Klasskock','Denss','Budnikwosky','Familia','Nahfrish','Andere'))
colnames(dataset) <- c('Reviews','Sections')

ggplot(dataset, aes(y=reorder(Sections, Reviews), x=Reviews, label=Reviews)) +
  geom_bar(stat = 'identity', aes(fill = Reviews)) +
  scale_fill_gradient2(low='red', mid='red', high='green') +
  geom_text(size = 3, position = position_stack(vjust = 1), color='Black') +
  geom_vline(xintercept=200, linetype="dashed", color = "blue", size=1) +
  xlab("Reviews") + 
  ylab("") +
  coord_cartesian(xlim = c(60,1300)) +
  scale_x_continuous(breaks = c(200, 400, 800, 1200))
