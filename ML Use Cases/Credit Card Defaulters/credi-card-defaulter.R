library(dplyr)

df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Credit Card Defaulters/credit_card-defaulters_dataset.csv")
View(df)

#1.Replace missing value with mean.
colSums(is.na(df))

for(i in 1:ncol(df)){
  df[is.na(df[,i]),i]=mean(df[,i],na.rm = TRUE)
}

colSums(is.na(df))


# 2. Drop default status column. (Last one)
new_df = subset(df, select = -c(Default.Status))

 # 3 Select top 2000 rows 
head(new_df ,2000)
new_df[1:2000,]

#4 Select this column LIMIT_BAL, AGE, BILL_AMT1, PAY_AMT1
subset(df,select = c(LIMIT_BAL, AGE, BILL_AMT1, PAY_AMT1))

#5. Select those data only who has , BILL_AMT1 more then 50000.
View(new_df %>% filter(new_df$BILL_AMT1 > 50000))

#6. Check the correlation on BILL_AMT1 vs PAY_AMT1.
CR = cor(new_df$BILL_AMT1,new_df$PAY_AMT1)
CR

#7. Rename the column BILL_AMT1 as Bill_amount and PAY_AMT1 as pay_amount.
rename(df ,Bill_amount=BILL_AMT1,pay_amount=PAY_AMT1)

#predict
library(caret)
sum(is.na(df))
dim(df)
library(randomForest)
sapply(df, class)
df$Default.Status = factor(df$Default.Status)

trainingset = createDataPartition(df$Default.Status,p=0.7,list = FALSE)
train.data = df[trainingset,]
test.data = df[-trainingset,]

rf =randomForest(Default.Status~.,train.data)
rf
pre = predict(rf,test.data[,-16])
confusionMatrix(pre,test.data$Default.Status)

#-----------------------svm--------------
library(e1071)
svm = svm(Default.Status~.,train.data)
svm
pre = predict(svm,test.data[,-16])
confusionMatrix(pre,test.data$Default.Status)
