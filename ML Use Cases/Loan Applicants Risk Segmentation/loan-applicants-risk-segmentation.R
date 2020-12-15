df = read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Loan Applicants Risk Segmentation/loan-applicants-risk-segmentation-dataset.csv")
View(df)

#1. Make three group of the product base on sales like High Risk, Medium Risk and Low Risk.
library(cluster)

model = kmeans(df,3)
model
model$cluster
colnames(df)

#2. Make a graph to identify this groups
library(factoextra)
fviz_cluster(model,data =df[,-8], palette = c("#2E9FDF", "#00AFBB", "#E7B800"))

aggregate(df, by=list(cluster=model$cluster), mean)
df$cluster[model$cluster == 2]='High Risk'
df$cluster[model$cluster == 3]='Medium Risk'
df$cluster[model$cluster == 1]='Low Risk'
View(df)

# 3.How many applicants belong to High, Medium & Low Risk Group?
df2 = cbind(df,cluster=model$cluster)
View(df2)

table(df$cluster)

