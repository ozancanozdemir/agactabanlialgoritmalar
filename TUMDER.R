## TUMDER Seminer 

## Aðaç Tabanlý Yapay Öðrenme Algoritmalarý 

## Ozancan Özdemir

library(caret) #model kütüphanesi
library(ggplot2) #veri görselleþtirmesi
library(tidyverse) #veri manipulasyonu 
library(GGally) #veri görselleþtirmesi
library(car) #veri setinin içeren kütüphane

data(Sacramento) #veri setini yükle
head(Sacramento) #ilk 6 satýrý görüntüle

## Keþifleyici Data Analizi 

str(Sacramento) #deðiþkenlerin yapýlarýný gösteriyor

sacramento_ev  <- Sacramento%>%subset(city =="SACRAMENTO")
head(sacramento_ev)
str(sacramento_ev)

summary(sacramento_ev) #tanýmlayýcý istatistikler ve sýklýk deðeleri 

sacramento_ev <- sacramento_ev%>%select(-c(city,zip)) #kullanýlmayacak deðiþkenleri kaldýr
head(sacramento_ev)

ggpairs(sacramento_ev) #korelasyon ve keþifleyici görsel 

## Veri Düzenleme 

## One Hot Encoding
dummy<-dummyVars(" ~ .", data=sacramento_ev)
sacramento_ev<-data.frame(predict(dummy, newdata =sacramento_ev))
head(sacramento_ev)

## Eðitim ve Test Verisi 

set.seed(1104)
egitim_index <- createDataPartition(sacramento_ev$price, p = .8,list=FALSE)
egitim <- sacramento_ev[egitim_index,]
test <- sacramento_ev[-egitim_index,]
head(names(getModelInfo())) #Caret paketi kullanarak kurulabilecek modellerin listesi

### CART 

## Ayarlanabilir parameterler

modelLookup("rpart") 

fitcontrol<- trainControl(method = "repeatedcv",
             number = 10,
             repeats = 5,
             summaryFunction = defaultSummary,
             search = "random")
set.seed(1104)
cartFit <- train(price~.,data = egitim,  method = "rpart",trControl = fitcontrol,metric="RMSE")

cartFit #eðitim sürecini ve parametreleri görüntüle 
 
cartFit$finalModel  #en iyi sonucu veren model detaylarýný görüntüle 

plot(cartFit$finalModel) #modeli grafikleþtir
text(cartFit$finalModel)


## Bagging 

modelLookup("treebag") 

set.seed(1104)
baggingFit<- train(price~.,data = egitim,  method = "treebag",trControl = fitcontrol,metric="RMSE")

baggingFit #eðitim sürecini ve parametreleri görüntüle 

baggingFit$finalModel  #en iyi sonucu veren model detaylarýný görüntüle 

## Random Forest 

modelLookup("rf") 

set.seed(1104)
rfFit<- train(price~.,data = egitim,  method = "rf",trControl = fitcontrol,metric="RMSE")

rfFit #eðitim sürecini ve parametreleri görüntüle 

rfFit$finalModel  #en iyi sonucu veren model detaylarýný görüntüle 

## Gradient Boosting 

modelLookup("gbm") 

set.seed(1104)
gbmFit<- train(price~.,data = egitim,  method = "gbm",trControl = fitcontrol,metric="RMSE")

gbmFit #eðitim sürecini ve parametreleri görüntüle 

gbmFit$finalModel  #en iyi sonucu veren model detaylarýný görüntüle 


## XGBoost 

modelLookup("xgbTree")

set.seed(1104)
xgbFit<- train(price~.,data = egitim,  method = "xgbTree",trControl = fitcontrol,metric="RMSE")

xgbFit #eðitim sürecini ve parametreleri görüntüle 

xgbFit$finalModel  #en iyi sonucu veren model detaylarýný görüntüle 


## Performans Karþýlaþtýrmasý 

cart_tahmin <- cartFit %>% predict(test)
bag_tahmin <-  baggingFit %>% predict(test)
rf_tahmin <- rfFit %>% predict(test)
gbm_tahmin <- gbmFit %>% predict(test)
xgb_tahmin <- xgbFit %>% predict(test)

# RMSE Hesaplama 
RMSE(cart_tahmin, test$price)
RMSE(bag_tahmin, test$price)
RMSE(rf_tahmin, test$price)
RMSE(gbm_tahmin, test$price)
RMSE(xgb_tahmin, test$price)


## Deðiþken Önem Grafiði 

plot(varImp(rfFit, scale = FALSE))

## Shapley Grafiði 

library(iml)
predictor = Predictor$new(rfFit, data = egitim[,-7], y = egitim$price)
shapley = Shapley$new(predictor, x.interest =  egitim[,-7])
plot(shapley)%>%labs(title = "Shap Grafiði",x = "Degiskenler")

