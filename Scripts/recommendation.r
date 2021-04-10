library(dplyr)
library(tidyverse)
library(haven)
library(caret)
library(ellipse)

#Dataset Load
library(haven)
customerdata <- read_sav(choose.files())
attr(customerdata$F1_Edeka, "labels")


cust_loyality_cols <- c("F8_1_EMPFEHLUNG_1","F8_3_POS_SPRECHEN_1", "F8_4_SELTENER_EINK_invertiert")
br_loc_sat_cols <- c("F31_1_LAGE_ERREICHBARKEIT_1","F31_2_LAGE_NAHVERKEHR_1", "F31_3_LAGE_PARKMOEGLICHKEIT_1")
findability_cols <-c("F33_1_AUFFINDBARKEIT_SCHNELLIGKEIT_1","F33_2_AUFFINDBARKEIT_AUSSCHILDERUNG_1")
price_Lab_cols <- c("F34_1_PREIS_RICHTIG_1","F34_2_PREIS_ZUORDNUNG_1","F34_3_PREIS_INFORMATIONEN_1")
price_setting_cols <- c("F40_1_REDUZIERTER_PREIS_1","F40_2_NIEDRIGPREISE_1", "F40_3_PREISLEISTUNG_1","F40_4_PREISVERGLEICH_1")
reduced_products_cols <- c("F40_1_REDUZIERTER_PREIS_1")
cheap_products_choice_cols <- c("F40_2_NIEDRIGPREISE_1")
compare_prices_cols <- c("F40_4_PREISVERGLEICH_1")
quality_setting_cols <- c("F41_1_WICHTIG_1","F41_2_BESTE_1", "F41_3_BESTE_GESAMTQUALITAET_1","F41_4_BESTE_QUALITAET_1","F41_5_WENIG_GEDANKEN_1")
quality_setting_cols_1 <- c("F41_1_WICHTIG_1")
quality_setting_cols_2 <- c("F41_2_BESTE_1")
quality_setting_cols_3 <- c("F41_3_BESTE_GESAMTQUALITAET_1")
quality_setting_cols_4 <- c("F41_4_BESTE_QUALITAET_1")
quality_setting_cols_5 <- c("F41_5_WENIG_GEDANKEN_1")
assortment_overall_cols <- c("F10_1_SORTIMENT_Global")
quality_overall_cols <- c("F10_2_QUALITAET_Global")
frische_overall_cols <- c("F10_3_FRISCHE_Global")
value_for_money_cols <- c("F10_4_PREISLEISTUNG_Global")
availabity_overall_cols <- c("F10_5_WARENVERFUEGBARKEIT_Global")
presentation_goods_cols <- c("F10_6_WARENPRÄSENTATION_Global")

#dataFrame extraction

cust_loyality_df <- customerdata[cust_loyality_cols]
br_loc_sat_df <- customerdata[br_loc_sat_cols]
findability_df <- customerdata[findability_cols]
price_Label_sat_df <- customerdata[price_Lab_cols]
price_setting_df <- customerdata[price_setting_cols]
quality_setting_df <- customerdata[quality_setting_cols]
assortment_overall_df <- customerdata[assortment_overall_cols]
quality_overall_df <- customerdata[quality_overall_cols]
frische_overall_df <- customerdata[frische_overall_cols]
value_for_money_df <- customerdata[value_for_money_cols]
availabity_overall_df <- customerdata[availabity_overall_cols]
presentation_goods_df <- customerdata[presentation_goods_cols]
reduced_products_df <- customerdata[reduced_products_cols]
cheap_products_choice_df <- customerdata[cheap_products_choice_cols]
compare_prices_df <- customerdata[compare_prices_cols]

#Replacing NAs for needed dataframes
br_loc_sat_df$F31_2_LAGE_NAHVERKEHR_1<-br_loc_sat_df$F31_2_LAGE_NAHVERKEHR_1 %>% replace(is.na(.), 0)
head(br_loc_sat_df$F31_2_LAGE_NAHVERKEHR_1)

br_loc_sat_df$F31_3_LAGE_PARKMOEGLICHKEIT_1<-br_loc_sat_df$F31_3_LAGE_PARKMOEGLICHKEIT_1 %>% replace(is.na(.), 0)
head(br_loc_sat_df$F31_3_LAGE_PARKMOEGLICHKEIT_1)



# Customer Loyalty
cust_loyality_df <- cust_loyality_df %>%
  replace(is.na(.), 0) %>%
  mutate(cust_loyality = rowSums(cust_loyality_df)/ncol(cust_loyality_df))
cust_loyality<-(cust_loyality_df[c("cust_loyality")])

# Branch Location satisfaction
br_loc_sat_df <- br_loc_sat_df %>%
  replace(is.na(.), 0) %>%
  mutate(br_loc_sat = rowSums(br_loc_sat_df)/ncol(br_loc_sat_df))
br_loc_sat<-(br_loc_sat_df[c("br_loc_sat")])


# Findability in store
findability_df <- findability_df %>%
  replace(is.na(.), 0) %>%
  mutate(findability = rowSums(findability_df)/ncol(findability_df))
findability<-(findability_df[c("findability")])


#Customer price settings
price_setting_df <- price_setting_df %>%
  replace(is.na(.), 0) %>%
  mutate(price_setting = rowSums(price_setting_df)/ncol(price_setting_df))
price_setting<-(price_setting_df[c("price_setting")])


#Customer Quality Setting
quality_setting_df <- quality_setting_df %>%
  replace(is.na(.), 0) %>%
  mutate(quality_setting = rowSums(quality_setting_df)/ncol(quality_setting_df))
quality_setting<-(quality_setting_df[c("quality_setting")])

#Customer Quality Setting
assortment_overall_df <- assortment_overall_df %>%
  replace(is.na(.), 0) %>%
  mutate(assortment = rowSums(assortment_overall_df)/ncol(assortment_overall_df))
assortment<-(assortment_overall_df[c("assortment")])

#Customer Quality Setting
quality_overall_df <- quality_overall_df %>%
  replace(is.na(.), 0) %>%
  mutate(quality = rowSums(quality_overall_df)/ncol(quality_overall_df))
quality<-(quality_overall_df[c("quality")])

#Customer Quality Setting
frische_overall_df <- frische_overall_df %>%
  replace(is.na(.), 0) %>%
  mutate(frische = rowSums(frische_overall_df)/ncol(frische_overall_df))
frische<-(frische_overall_df[c("frische")])

#Customer Quality Setting
value_for_money_df <- value_for_money_df %>%
  replace(is.na(.), 0) %>%
  mutate(value_for_money = rowSums(value_for_money_df)/ncol(value_for_money_df))
value_for_money<-(value_for_money_df[c("value_for_money")])

#Customer Quality Setting
availabity_overall_df <- availabity_overall_df %>%
  replace(is.na(.), 0) %>%
  mutate(availabity = rowSums(availabity_overall_df)/ncol(availabity_overall_df))
availabity<-(availabity_overall_df[c("availabity")])

#Customer Quality Setting
presentation_goods_df <- presentation_goods_df %>%
  replace(is.na(.), 0) %>%
  mutate(presentation_goods = rowSums(presentation_goods_df)/ncol(presentation_goods_df))
presentation_goods<-(presentation_goods_df[c("presentation_goods")])

reduced_products_df <- reduced_products_df %>%
  replace(is.na(.), 0) %>%
  mutate(reduced_products = rowSums(reduced_products_df)/ncol(reduced_products_df))
reduced_products<-(reduced_products_df[c("reduced_products")])

cheap_products_choice_df <- cheap_products_choice_df %>%
  replace(is.na(.), 0) %>%
  mutate(cheap_products_choice = rowSums(cheap_products_choice_df)/ncol(cheap_products_choice_df))
cheap_products_choice<-(cheap_products_choice_df[c("cheap_products_choice")])

compare_prices_df <- compare_prices_df %>%
  replace(is.na(.), 0) %>%
  mutate(compare_prices = rowSums(compare_prices_df)/ncol(compare_prices_df))
compare_prices<-(compare_prices_df[c("compare_prices")])

store_cols <- c("F3_Haupteinkaufsstaette")
stores_df <- customerdata[store_cols]
stores_df = as_factor(stores_df)
head(stores_df)

data_set <- cbind(cust_loyality, br_loc_sat, findability, price_setting, quality_setting, 
                  assortment, quality, frische, value_for_money, availabity,
                  presentation_goods, 
                  stores_df)
data_set <- droplevels(data_set)
head(data_set)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(data_set$F3_Haupteinkaufsstaette, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- data_set[-validation_index,]
# use the remaining 80% of data to training and testing the models
data_set <- data_set[validation_index,]


################### Analysing Dataset ##########################
dim(data_set)

write.csv(data_set, paste0(getwd(),"/ClassDistrubution.csv"), row.names=FALSE, quote=FALSE)

# list types for each attribute
sapply(data_set, class)

#Summarize the class distribution
percentage <- prop.table(table(data_set$F3_Haupteinkaufsstaette)) * 100
cbind(freq=table(data_set$F3_Haupteinkaufsstaette), percentage=percentage)


# summarize attribute distributions
summary(data_set)

x <- data_set[,1:11]
y <- data_set[,12]
head(y)
levels(y)

# boxplot for each attribute on one image
par(mfrow=c(1,5))
for(i in 1:5) {
  boxplot(x[,i], main=names(data_set)[i])
}


# barplot for class breakdown

plot(y)


# Run algorithms using 20-fold cross validation
control <- trainControl(method="repeatedcv", number=30, repeats = 50)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(F3_Haupteinkaufsstaette~., data=data_set, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(F3_Haupteinkaufsstaette~., data=data_set, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(F3_Haupteinkaufsstaette~., data=data_set, method="knn", metric=metric, trControl=control)

control <- trainControl(method="repeatedcv", number=30, repeats = 2)
metric <- "Accuracy"
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(F3_Haupteinkaufsstaette~., data=data_set, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(F3_Haupteinkaufsstaette~., data=data_set, method="rf", metric=metric, trControl=control)



# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn))
summary(results)

results1 <- resamples(list(svm=fit.svm, rf=fit.rf))
summary(results1)


# compare accuracy of models
dotplot(results)
#dotPlot(results1)
print(fit.lda)
print(fit.cart)
print(fit.knn)
print(fit.svm)
print(fit.rf)



# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$F3_Haupteinkaufsstaette)