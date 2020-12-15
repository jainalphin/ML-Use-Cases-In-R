library(dplyr)
library(readxl)
df= read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/sucide/suicide_statistics_data-set.csv")
View(df)
df2=df
sum(is.na(df))

get_mode = function(a){
  uniq = unique(a)
  return(uniq[which.max(tabulate(match(a,uniq)))])
}

for(i in 1:ncol(df2)){
  if(is.numeric(df[,i]) |is.integer(df[,i]) ){
    df2[is.na(df[,i]),i]=mean(df2[,i],na.rm = TRUE)
  }else{
    df2[is.na(df[,i]),i]=get_mode(df2[,i])
  }
}

sum(is.na(df2))

#1. Which field effects most on Suicides incidence?


library(FSelector)
DF1= cfs(suicides_no~., df2)
f <- as.simple.formula(DF1, "suicides_no")
print(f)


#--------------------2nd method-------------------
f = as.formula(paste("suicides_no~",paste(colnames(df[,1:5]),collapse = "+")))
f

model1 = lm(f,df2)
print(model1)
summary(model1)
summary(model1)$coefficients[,"Pr(>|t|)"]

res = data.frame(summary(model1)$coefficients[,"Pr(>|t|)"])
View(res) 
colnames(res)
res =res %>% rename(p_value=summary.model1..coefficients....Pr...t....)

res = res %>% arrange(-p_value)
View(res)

result_val = rownames(res[1])[1]
result_val


#2  Which age group having high incidence of suicides

result = df2 %>% group_by(age) %>% summarise(sum = sum(suicides_no)) %>% arrange(-sum)
result

#------------------------------ans3----------------------------------
model2 = lm(suicides_no~age,df)
summary(model2)
res = data.frame(summary(model2)$coefficients[,'Pr(>|t|)'])
colnames(res)
res = res %>% arrange(-summary.model2..coefficients....Pr...t....)
result_value2= rownames(res)[1]
result_value2

# 3. Is there any gender affection on suicide rate?
ttest = t.test(suicides_no~sex,df)
ttest
p_value = ttest$p.value
p_value

# 4 Which state having low suicide rate
table(df$country,df$population)
df$suicide_rate = df$suicides_no/df$population
View(df)
df[with(df,order("suicide_rate")),][1]
df[order(df$suicide_rate),1][1]

total = sum(df2[,"population"])
total_state = df2 %>% select(country,population) %>% group_by(country) %>% summarise(sum = sum(population))
total_state$suicide_rate = total_state$sum / total
total_state = total_state %>% arrange(suicide_rate)
total_state[1,1]
