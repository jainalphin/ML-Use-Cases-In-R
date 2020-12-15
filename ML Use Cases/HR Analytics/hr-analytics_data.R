library(readxl)
df=read_excel("E:/DATA SCIENCE- Fingertips/Assigenment/Github/HR Analytics/hr-analytics_data.xlsx")
View(df)

#1. Select- Age, Gender, MaritalStatus
subset(df,select=c('Age','Gender','MaritalStatus'))

#2 Select-Age to JobSatisfaction and BusinessTravel
View(subset(df,select = (c(Age:JobSatisfaction,BusinessTravel))))

#3 Exclude- EducationField, JobSatisfaction, OverTime
View(subset(df,select = -c(EducationField,JobSatisfaction)))
colnames(subset(df,select = -c(EducationField,JobSatisfaction)))

#4 Exclude-2,4,6 Column
View(subset(df,select = -c(2,4,6)))

#5 Select First 20 Observations
View(df[1:5,])
head(df,5)

#6 Select subset where Gender=Female, Age>40, MonthlyIncome>7000
View(subset(df, df$Gender== 'Female' & df$Age >40 & df$MonthlyIncome > 7000))

#7 Select JobSatisfaction, MonthlyIncome, OverTime, DailyRate where MonthlyIncome>7000 & <10000
View(subset(df, select = c(JobSatisfaction,MonthlyIncome,OverTime,DailyRate),
                           (df$MonthlyIncome > 7000 & df$MonthlyIncome < 10000)))


