# Libraries used

#install.packages("ggplot2")
#install.packages("csv")
#install.packages('randomForest')
#install.packages("magrittr") 
#install.packages("dplyr") 
#install.packages("ggthemes")
#install.packages("rpart")
#install.packages("rattle")
#install.packages("partykit")
#install.packages("VIM")
#install.packages("effects")
#install.packages("imputeTS")
#install.packages("corrplot")
#install.packages("pROC")
library(pROC)
library(effects)
library(ggplot2)
library(csv)
library(randomForest)
library(magrittr)
library(dplyr)
library(ggthemes)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(partykit)
library(VIM)
library(imputeTS)
library(corrplot)
library(reshape2)

# Setting working directory
setwd("C:/Users/New/Documents/tcd")

# Importing data
Data<-read.csv("projectdata.csv",header=TRUE,sep=",")

# Creating new data set for applying models
df1 <- as.data.frame(Data)
attach(df1)

# Check missing values . Identify the columns which have missing values 
var <- df1[c("X1", "X2", "X3", "X4", "X5", "X6", "X7","Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")]
aggr_plot <- aggr(var, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(var), cex.axis=.9, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Data imputation
df1 <- kNN(df1,variable=c("X1", "X5"),k=7)
df1 <- kNN(df1,variable=c("X6", "X7"),k=6)
df1 <- kNN(df1,variable=c("X2", "X3"),k=6)
df1 <- kNN(df1,variable=c("Y1", "Y5"),k=3)
df1 <- kNN(df1,variable=c("Y6", "Y7"),k=3)
df1 <- kNN(df1,variable=c("Y2", "Y3"),k=3)

colnames(df1)[colSums(is.na(df1)) > 0]

# Imputation Impact
mvc = 0
j = 1
mat <- matrix(ncol=2, nrow=17)
for (i in 1:ncol(Data))
{
  m = sum(is.na(Data[,i]))
  if(m>0){
    mvc = mvc+1
    mat[j,] <- c(colnames(Data[i]),round((mean(Data[,i], na.rm = TRUE)-mean(df1[,i]))*100/mean(df1[,i])))
    j = j + 1
  }
  else{
    mvc
  }
} 

columnVal <- as.numeric(mat[1:14,ncol=2])
columnName <- mat[1:14]
barplot(columnVal,
        ylim = c(-30,30),
        main = "Imputation Effect",
        xlab = "Column Name",
        ylab = "Mean Percentage Change",
        names.arg = columnName,
        col = "red")
     
# Dealing with categorical columns
df1$Response <- as.factor(df1$Response)
df1$Y1 <- as.factor(df1$Y1)
df1$Y2 <- as.factor(df1$Y2)
df1$Y3 <- as.factor(df1$Y3)
df1$Y4 <- as.factor(df1$Y4)
df1$Y5 <- as.factor(df1$Y5)
df1$Y6 <- as.factor(df1$Y6)
df1$Y7 <- as.factor(df1$Y7)

# Removing Outiers
outliers <- function(x) {
  qnt <- quantile(x, probs=.75)
  H <- 5 * IQR(x)
  upper <- (qnt + H)
  return(upper)
}

# column X1 
setX1 <- split(df1$X1,Response)
a = data.frame(Response = "0", X1 = setX1[[1]])
b = data.frame(Response = "1", X1 = setX1[[2]])
plot.data = rbind(a, b)
ggplot(plot.data, aes(x=Response, y=X1, fill=Response)) + 
  geom_boxplot() 
upper <- outliers(df1$X1)
df1 <- df1[df1$X1 < upper,]

# column X2 
setX2 <- split(df1$X2,Response)
a = data.frame(Response = "0", X2 = setX2[[1]])
b = data.frame(Response = "1", X2 = setX2[[2]])
plot.data = rbind(a, b)
ggplot(plot.data, aes(x=Response, y=X2, fill=Response)) + 
  geom_boxplot() 
upper <- outliers(df1$X2)
df1 <- df1[df1$X2 < upper,]

# column X7
setX7 <- split(df1$X7,Response)
a = data.frame(Response = "0", X7 = setX7[[1]])
b = data.frame(Response = "1", X7 = setX7[[2]])
plot.data = rbind(a, b)
ggplot(plot.data, aes(x=Response, y=X7, fill=Response)) + 
  geom_boxplot() 
upper <- outliers(df1$X7)
df1 <- df1[df1$X7 < upper,]

df1 %>% 
  group_by(df1$Response) %>%
  summarise(number = n())

# Spliting data into training and testing sets based on column Group
counts <- table(df1$Response, df1$Group)
barplot(counts,
        ylim = c(0,200),
        main="Data division based on Group",
        ylab = "Count",
        xlab= c("Group 0(Test Set)"," Group 1(Train Set)"), col=c("red","darkblue"),
        legend = c("Response=0","Response=1"))

setdf1 <- split(df1,Group)

train <- setdf1[[2]]
test <- setdf1[[1]]

train %>% 
  group_by(train$Response) %>%
  summarise(number = n())

test %>% 
  group_by(test$Response) %>%
  summarise(number = n())

# Correlation between features
trainCor <- train[,4:17]
trainCor[] <- lapply(trainCor, function(x) as.numeric(as.character(x)))
M<-cor(trainCor)
corrplot(M, method="color")
corrplot(M, method="number")

# Column visualization

# Highest correlation columns 
trainCor <- train[c("Response", "X1", "X2","X3", "X5")]
cor <- melt(trainCor, "Response")
ggplot(cor, aes(value, Response, color = variable)) +
  geom_point(size=10, shape=23)

# Important columns 
trainImp <- train[c("Response", "X1", "X5", "X6", "X2")]
imp <- melt(trainImp, "Response")
ggplot(imp, aes(value, Response, color = variable)) +
  geom_point(size=10, shape=23)

# Random forest
set.seed(100)
modelRandom <- randomForest(Response ~ X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7 ,
                              data = train, importance = T)
print(modelRandom)

predictionRandom <- predict(modelRandom, newdata = test)
confusionMatrix(predictionRandom,test$Response)

importance    <- importance(modelRandom)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Creating a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Using ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Decision tree
set.seed(200)
modelDecision <- rpart(Response ~ X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7 ,
                       data = train, method="class")
print(modelDecision$cptable)
opt <- which.min(modelDecision$cptable[,"xerror"])
cp <- modelDecision$cptable[opt,"CP"]
fitPrune <- prune(modelDecision,cp=cp)

# Predicting for the test dataset
predictionDecision <- predict(fitPrune,test,type="class")
confusionMatrix(predictionDecision,test$Response)

# Logistic regression
set.seed(300)
modelLogistic <- glm(Response ~ X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7 , family=binomial(link='logit'),data=train)

# Predicting for the test dataset
predictionLogistic<-plogis(predict(modelLogistic,test))
y_pred = as.factor(ifelse(predictionLogistic > 0.5, 1, 0))
confusionMatrix(y_pred,test$Response)

# ---------------------------Random forest------------------
set.seed(400)
modelRandomOpt <- randomForest(Response ~ X1+X5+X6+X2 ,
                            data = train, importance = T)

print(modelRandomOpt)
predictionRandomOpt <- predict(modelRandomOpt, newdata = test)
confusionMatrix(predictionRandomOpt,test$Response)

Random_Forest <- modelRandomOpt
plot(Random_Forest)

# ----------------------Decision tree---------------------
set.seed(500)
modelDecisionOpt <- rpart(Response ~ X1+X5+X6+X2 ,
                              data = train, method="class")
fancyRpartPlot(modelDecisionOpt)
plot(as.party(modelDecisionOpt))

# Pruning 
optOpt <- which.min(modelDecisionOpt$cptable[,"xerror"])
cpOpt <- modelDecisionOpt$cptable[optOpt,"CP"]
fitPruneOpt <- prune(modelDecisionOpt,cp=cpOpt)

# Predicting for the test dataset
predictionDecisionOpt <- predict(fitPruneOpt,test,type="class")
confusionMatrix(predictionDecisionOpt,test$Response)

plot(as.party(fitPruneOpt))

# -------------------Logistic regression--------------------
set.seed(600)
modelLogisticOpt <- glm(Response ~ X1+X5+X6+X2 , family=binomial(link='logit'),data=train)

# Predicting for the test dataset
predictionLogisticOpt<-plogis(predict(modelLogisticOpt,test))

y_predOpt = as.factor(ifelse(predictionLogisticOpt > 0.5, 1, 0))
confusionMatrix(y_predOpt,test$Response)

# Roc graphs for logistic regression 
rocLogOpt <- roc(train$Response, as.vector(fitted.values(modelLogisticOpt)))
rocLog <- roc(train$Response, as.vector(fitted.values(modelLogistic)))

plot(rocLogOpt)
plot(rocLog, add=TRUE, col='red', main = "Receiver Operating Characteristic Curve")