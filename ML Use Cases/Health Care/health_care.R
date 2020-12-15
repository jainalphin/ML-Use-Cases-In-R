library(dplyr)
library(caret)
df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Health Care/healthcare_patient_data.csv")

View(df)
dim(df)

#1 Filter the data by age (select data where age is between 40-65)
View(df %>% filter( AGE> 45 & AGE < 65))

# 2. Drop the unwanted columns (Feature Importance)
library(FSelector)
View(df)
feature_list = gain.ratio(DIABETES~.,df)
feature_list = feature_list %>% arrange(-attr_importance)
View(feature_list)

feature_list = subset(feature_list,attr_importance >= 0.01)
dim(feature_list)

# 3. Handle the missing values.
mean(is.na(df))
#colSums(is.na(df))
for(i in 1:ncol(df)){
  df[is.na(df[,i]),i]=mean(df[,i],na.rm = TRUE)
}

sum(is.na(df))

#4. Prepare the classification model for predict the diabetes class 1 or 0. 
#( 1 means Diabetes positive and 0 means Diabetes Negative result.)
df$DIABETES <- as.integer(df$DIABETES)
head(df$DIABETES)
f = as.formula(paste("DIABETES~",paste(rownames(feature_list),collapse = '+')))
f

library(e1071)

trainingset = createDataPartition(df$DIABETES,p=0.80,list = FALSE)
train.data = df[trainingset,]
test.data = df[-trainingset,]
model = svm(f,train.data,type='C-classification', kernel = 'linear')
model

pre= predict(model,test.data[,rownames(feature_list)])
pre

#4 Check the accuracy of model and also prepare confusion matrix for the out 1 and 0.
length(pre)

res= table(test.data$DIABETES,pre)
res

confusionMatrix(pre,factor(test.data$DIABETES))




