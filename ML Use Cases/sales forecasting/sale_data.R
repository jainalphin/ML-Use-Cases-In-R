library(dplyr)
library(caTools)
df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/sales forecasting/sales_data_forecast.csv")

View(df)

#Find the impact of Baseprice on Salesprice
CR = cor(df$BasePrice,df$SalesPrice)
CR

#Accuracy of the model.
model = lm(SalesPrice~BasePrice,df)
summary(model)

nrow(df)

sample= sample.split(df,SplitRatio = 0.7)
train = subset(df,sample==TRUE)
test =subset(df,sample==FALSE)
colnames(test)

model1 =lm(SalesPrice~BasePrice,train)
summary(model1)
plot(train$SalesPrice,train$BasePrice)
abline(model1,col="red")

X_test =subset(test,select = -SalesPrice)
y_pred = predict(model1,X_test)

r2_score = function(a,b){
  return(cor(a,b)^2)
}
r2_score(test$SalesPrice,y_pred)

