df= read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/Github/Sales Clustering/product-sales_data_clustering.csv")
View(df)

#1.Make three group of the product base on sales like High sales, Medium sales and Low sales.
library(cluster)
sapply(df, class)

kmeans_model = kmeans(df,centers =3)
#clusplot(df,kmeans_model$cluster,label =3)
aggregate(df,by = list(cluster=kmeans_model$cluster),mean)

df$cluster[kmeans_model$cluster==1]="High Sales"
df$cluster[kmeans_model$cluster==2]="Medium Sales"
df$cluster[kmeans_model$cluster==3]="Low Sales"

#How many products belong to High Sales Product Groups
library(ggpubr)
library(factoextra)
res.km <- kmeans(scale(df), 3, nstart = 25)

fviz_cluster(res.km, data = df[, -5],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

