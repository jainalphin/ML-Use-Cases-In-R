library(caret)

df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Hot lead prediction/hot-lead-prediction-dataset.csv")

View(df)
dim(df)
sapply(df,class)

#check na value
sum(is.na(df))
colSums(is.na(df))

# dt -----------model----------
library(rpart)
tree_model = rpart(Opportunity_Result~.,df)
tree_model

pre= predict(tree_model,type='class')
res= table(df$Opportunity_Result,pre)

# accuraccy
confusionMatrix(pre,factor(df$Opportunity_Result))

# prediction on test data
library(readxl)
test = read_excel("E:/DATA SCIENCE- Fingertips/Assigenment/R/Hot lead prediction/predict-value_hot-lead.xlsx")
View(test)
colnames(test)[which(names(test)=='Competitor Type')] <- "Competitor_Type"
test_pre = predict(tree_model,test,type = 'class')
test_pre


#-------------------Ranodm Forest---------------
df2=df
df2$Region = as.numeric(factor(df2$Region))
df2$Route_To_Market = as.numeric(factor(df2$Route_To_Market))
df2$Competitor_Type = as.numeric(factor(df2$Competitor_Type))


df2$Opportunity_Result = as.integer(factor(df2$Opportunity_Result))-1
library(caTools)
set.seed(123)
split= sample.split(df$Opportunity_Result,SplitRatio = 0.75)
training_set = subset(df2,split=TRUE)
test_data =subset(df2,split=FALSE)


library(randomForest)
rf= randomForest(x=training_set[-1],y=factor(training_set$Opportunity_Result),ntree = 100)
y_pred = predict(rf,newdata = test_data[-1])
cm = confusionMatrix(y_pred,factor(test_data$Opportunity_Result))
cm