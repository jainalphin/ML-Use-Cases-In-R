install.packages('caTools')
df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/malaria/malaria_project_r_datacleaning-and-ml.csv")
View(df)

library(dplyr)
#1.Filter the Gujarat, Rajasthan and Maharashtra in state.
View(df %>% filter(df$State==c('Gujarat','Rajasthan','Maharashtra')))

#2 Select the data where mortality is high and very high
View(df %>%select(Mortality_rate) %>% filter(df$rate == c('High','Very High')))

#3 Handle the missing values in mortality rate column .
any(is.na(df))
mean(is.na(df))
colSums(is.na(df))

df[is.na(df$Mortality_rate),"Mortality_rate"]=mean(df$Mortality_rate,na.rm=TRUE)
colSums(is.na(df))

#Rename the mortality column to mortality_category
df %>% rename(mortality_category=Mortality_rate)
df


#Classification on mortality column
df= read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/R/malaria//malaria_project_r_datacleaning-and-ml.csv")
View(df)
dim(df)

library(caTools)

dim(df)

#check missing value
sum(is.na(df))

#remove na
df =na.omit(df)

sample=sample.split(df,SplitRatio = 0.8)
train=subset(df,sample==TRUE)
test=subset(df,sample==FALSE)

model = lm(Mortality_rate~.,train)

X_test = subset(test,select = -Mortality_rate)
y_test = test$Mortality_rate
y_pred = predict(model,X_test)

rsq <- function (x, y) cor(x, y) ^ 2
rsq(y_test,y_pred)
summary(model)



