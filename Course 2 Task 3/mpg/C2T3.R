mpg <- read_csv("D:/Data Analytics Course Tasks/R Data/mpg.csv")
View(mpg)
view(mpg)
summary(mpg)
str(mpg)
View(mpg)
View(mpg)
mpg$horsepower<-as.numeric(mpg$horsepower)
str(mpg)
mpg$horsepower[is.na(mpg$horsepower)]<-mean(mpg$horsepower,na.rm=TRUE)
save.image("D:/Data Analytics Course Tasks/Course 2 Task 2- Predicting Profitability and Customer Preferences/Workspace/C2T2.RData")
mpg$origin<-factor(mpg$origin)
mpg$`model year`<-factor(mpg$`model year`)
mpg$cylinders<-factor(mpg$cylinders)
mpg$`car name`<-NULL
str(mpg)
mpg$weight<-as.numeric(mpg$weight)
str(mpg)
trainSize<-round(nrow(mpg)*0.7)
testSize<-nrow(mpg)-trainSize
set.seed(123)
training_indices<-sample(seq_len(nrow(mpg)),size=trainSize)
trainSet<-mpg[training_indices, ]
testSet<-mpg[-training_indices, ]
model<-lm(formula=trainSet$mpg~., data=trainSet)
summary(model)
predictions<-predict(model, testSet, interval="predict", level=.95)
head(predictions)
comparison<-cbind(testSet$mpg, predictions[,1])
colnames(comparison)<-c("actual", "predicted")
head(comparison)
summary(comparison)
mape<-(sum(abs(comparison[,1]-comparison[,2])/abs(comparison[,1]))/nrow(comparison))*100
mape
mapeTable<-cbind(comparison, abs(comparison[,1]-comparison[,2])/comparison[,1]*100)
colnames(mapeTable)[3]<-"absolute percent error"
head(mapeTable)
sum(mapeTable[,3])/nrow(comparison)
save.image("D:/Data Analytics Course Tasks/Course 2 Task 2- Predicting Profitability and Customer Preferences/Workspace/C2T2.RData")
