## TUMDER Seminer 

## A�a� Tabanl� Yapay ��renme Algoritmalar� 

## Ozancan �zdemir

library(caret) #model k�t�phanesi
library(ggplot2) #veri g�rselle�tirmesi
library(tidyverse) #veri manipulasyonu 
library(GGally) #veri g�rselle�tirmesi
library(car) #veri setinin i�eren k�t�phane

data(Sacramento) #veri setini y�kle
head(Sacramento) #ilk 6 sat�r� g�r�nt�le

## Ke�ifleyici Data Analizi 

str(Sacramento) #de�i�kenlerin yap�lar�n� g�steriyor

sacramento_ev  <- Sacramento%>%subset(city =="SACRAMENTO")
head(sacramento_ev)
str(sacramento_ev)

summary(sacramento_ev) #tan�mlay�c� istatistikler ve s�kl�k de�eleri 

sacramento_ev <- sacramento_ev%>%select(-c(city,zip)) #kullan�lmayacak de�i�kenleri kald�r
head(sacramento_ev)

ggpairs(sacramento_ev) #korelasyon ve ke�ifleyici g�rsel 

## Veri D�zenleme 

## One Hot Encoding
dummy<-dummyVars(" ~ .", data=sacramento_ev)
sacramento_ev<-data.frame(predict(dummy, newdata =sacramento_ev))
head(sacramento_ev)

## E�itim ve Test Verisi 

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

cartFit #e�itim s�recini ve parametreleri g�r�nt�le 
 
cartFit$finalModel  #en iyi sonucu veren model detaylar�n� g�r�nt�le 

plot(cartFit$finalModel) #modeli grafikle�tir
text(cartFit$finalModel)


## Bagging 

modelLookup("treebag") 

set.seed(1104)
baggingFit<- train(price~.,data = egitim,  method = "treebag",trControl = fitcontrol,metric="RMSE")

baggingFit #e�itim s�recini ve parametreleri g�r�nt�le 

baggingFit$finalModel  #en iyi sonucu veren model detaylar�n� g�r�nt�le 

## Random Forest 

modelLookup("rf") 

set.seed(1104)
rfFit<- train(price~.,data = egitim,  method = "rf",trControl = fitcontrol,metric="RMSE")

rfFit #e�itim s�recini ve parametreleri g�r�nt�le 

rfFit$finalModel  #en iyi sonucu veren model detaylar�n� g�r�nt�le 

## Gradient Boosting 

modelLookup("gbm") 

set.seed(1104)
gbmFit<- train(price~.,data = egitim,  method = "gbm",trControl = fitcontrol,metric="RMSE")

gbmFit #e�itim s�recini ve parametreleri g�r�nt�le 

gbmFit$finalModel  #en iyi sonucu veren model detaylar�n� g�r�nt�le 


## XGBoost 

modelLookup("xgbTree")

set.seed(1104)
xgbFit<- train(price~.,data = egitim,  method = "xgbTree",trControl = fitcontrol,metric="RMSE")

xgbFit #e�itim s�recini ve parametreleri g�r�nt�le 

xgbFit$finalModel  #en iyi sonucu veren model detaylar�n� g�r�nt�le 


## Performans Kar��la�t�rmas� 

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


## De�i�ken �nem Grafi�i 

plot(varImp(rfFit, scale = FALSE))

## Shapley Grafi�i 

library(iml)
predictor = Predictor$new(rfFit, data = egitim[,-7], y = egitim$price)
shapley = Shapley$new(predictor, x.interest =  egitim[,-7])
plot(shapley)%>%labs(title = "Shap Grafi�i",x = "Degiskenler")

