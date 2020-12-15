library(dplyr)
library(readxl)
df= read_excel("E:/DATA SCIENCE- Fingertips/Assigenment/Github/rainfall-analysis/rainfall-analysis.xlsx")
View(df)

sum(is.na(df))

#1 Which State has highest annual rainfall?
colnames(df)
df[order(-df$annual),1][1,]

#2 2. Find out the relation between state and annual rainfall?
anova =aov(annual~state_ut_name,df)
anova(anova)

#3. Which month has highest total rainfall?
names(which.max(colSums(df[,3:14])))

#4
#4-----------------------------------lm-----------------------------------------------
colnames(df)
f = as.formula(paste("district~","state_ut_name"))
f
View(df)
df2=df

df2$district =as.numeric(factor(df2$district))

model = lm(district~state_ut_name,df2)
summary(model)
res = data.frame(summary(model)$coefficients[,"Pr(>|t|)"])
res= res%>% arrange(-summary.model..coefficients....Pr...t....)
item_type = rownames(res)[1]
item_type



#Which state has lowest rainfall in May?
df[order(df$may),1][1,]


#which month observed highest rainfall in assam?
unique(df$state_ut_name)
df2 =df %>% filter(df$state_ut_name == "assam") %>% select(3:14) 
df2
names(sort(colMeans(df2),decreasing = TRUE)[1])



