library(dplyr)

df= read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Crime Classification/multiclassification-predicting-crime-dataset.csv")
View(df)

#---------------------random forest--------------------
library(randomForest)
library(caret)
trainset = createDataPartition(y=df$Crime.Type,p=0.80,list = FALSE)
length(trainset)
train.data = df[trainset,]
test.data = df[-trainset,]
dim(train.data)

forest_model = randomForest(factor(Crime.Type)~.,train.data)
forest_model
pre = predict(forest_model,newdata = test.data[,-1])
pre
cm =confusionMatrix(pre,factor(test.data$Crime.Type))


# 2 Get the classification ratio for all the different class available in crime type.
library(stringr)
for(i in 1:6){
  class_name =word(rownames(cm$byClass)[i],start = 2,sep = ':')
  cat("accuracy of",class_name,"is", cm$byClass[i,'Balanced Accuracy'],"\n")
}


#3 accuracy
cm$overall[1]


#-------------------xgboost--------------------
df3=df

crime_type = df3$Crime.Type

#data cleaning
label = as.integer(factor(df3$Crime.Type))-1
df3$Crime.Type =NULL
View(df3)

df3$Neighborhood = as.numeric(factor(df3$Neighborhood))

#traintest split
n = nrow(df3)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(df3[train.index,])
train.label = label[train.index]
test.data = as.matrix(df3[-train.index,])
test.label = label[-train.index]

# Optional------- Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

#train model
num_class = length(levels(factor(crime_type)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=1000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test)
)
xgb.fit

#predict model
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)

colnames(xgb.pred) =levels(factor(crime_type))
View(xgb.pred)
#accuracy
#row wise apply this function
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(factor(crime_type))[test.label+1]
View(xgb.pred)


result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
confusionMatrix(factor(xgb.pred$prediction),factor(xgb.pred$label))





