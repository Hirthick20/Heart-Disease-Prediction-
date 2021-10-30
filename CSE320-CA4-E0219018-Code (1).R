#Loading the dataset

data <- read.csv(
  "processed.cleveland.csv",
  header=FALSE
)
data
dim(data)

#Assigning the column names to the dataset

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
head(data)

data$num[data$num > 1] <- 1
head(data)

summary(data)
sapply(data, class)

#Data transformation

data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thal=as.factor(thal),
  num=as.factor(num)
)
sapply(data, class)
summary(data)

#Checking for missing values

data[ data == "?"] <- NA
colSums(is.na(data))

#Data Cleaning
data$thal[which(is.na(data$thal))] <- as.factor("3.0")
data <- data[!(data$ca %in% c(NA)),]
colSums(is.na(data))

data$ca <- factor(data$ca)
data$thal <- factor(data$thal)
summary(data)



#Data Visualization

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#Presence and absence of heart disease
ggplot(data, aes(x=data$num, fill=data$num))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name= 'Heart Disease', labels =c("Absence", "Presence"))

#Age analysis
ggplot(data, aes(x=as.factor(age), fill=as.factor(num) )) +
  geom_bar( )

#Sex analysis
ggplot(data, aes(x=as.factor(sex), fill=as.factor(num) )) +
  geom_bar( )

#Cholestrol vs age
p2<-data %>% ggplot(aes(x=age,y=choi,col=sex, size=choi))+geom_point(alpha=0.7)+xlab("Age") + 
  ylab("Cholestoral")+guides(fill = guide_legend(title = "Gender"))+
  theme(plot.margin = margin(0.1,.1,.1,.1, "cm"))
p2

#Box plot- ca vs trestbps:sex
ggplot(data,aes(ca,trestbps))+geom_boxplot(aes(color=sex))+labs(x='number of major vessels (0-3) colored by flourosopy',y='resting blood pressure (in mm Hg on admission to the hospital)')


#Applying the model 

install.packages("randomForest")
install.packages('caTools')
install.packages('caret')
library(randomForest)
require(caTools)
library(caret)

sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  num ~ .,
  data=train
)
rf

pred = predict(rf, newdata=test[-14])

cm = table(test[,14], pred)
cm

result<-confusionMatrix(cm)
result$overall['Accuracy']




