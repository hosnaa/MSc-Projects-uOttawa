################################# Read Data and Take subset ##################################
library(ggplot2)
library (cluster)
# Read the Data
df_framin <- read.csv("D:/DEBI_Outtawa/Semester 1/DS/Ass3/framingham.csv",header=TRUE);
str(df_framin)

# Subset 
df_framin_field <- df_framin[c(1,2,16)]

# Standardize Age
df_framin_field$age_std <- scale(x =df_framin_field$age)
df_framin_field$age <- NULL

# convert from integer to categorical
df_framin_field$TenYearCHD <- as.factor(df_framin_field$TenYearCHD)
str(df_framin_field)

ggplot(df_framin_field, aes(male, age_std, color = TenYearCHD)) + geom_point()

###################################### Q (A) Kmeans; K = 4 #####################################
# Kmeans clustering
Cluster_kmean <- kmeans(df_framin_field, 4, nstart = 500)

#Tabulate the cross distribution
table(Cluster_kmean$cluster, df_framin_field$TenYearCHD)

Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
ggplot(df_framin_field, aes(age_std, male, color = TenYearCHD)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('blue', 'black','red','green'))

################################## Q (B) Elbow and Kmeans ########################################

# Elbow Curve
wss <- (nrow(df_framin_field)-1)*sum(apply(df_framin_field,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(df_framin_field,centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# We can observe 7 clusters are the best
Cluster_kmean2 <- kmeans(df_framin_field, 7, nstart = 1000)

#Tabulate the cross distribution
table(Cluster_kmean2$cluster, df_framin_field$TenYearCHD)

ggplot(df_framin_field, aes(age_std, male, color = TenYearCHD)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean2$cluster) + 
  scale_color_manual(values = c('blue', 'black','red','green','yellow','white','cyan'))

################################## Q (C) Silhouette Score #####################################
################# For each cluster from the 7 clusters ################################
# Distance metric, here it's Eculidean
# https://stackoverflow.com/questions/33999224/silhouette-plot-in-r
dis = dist(df_framin_field)^2
str(df_framin_field)
sil = silhouette(Cluster_kmean2$cluster, dis) # 
windows() 
plot(sil)

################ For different number of clusters ##############################
silhouette_score <- function(k){
  km <- kmeans(df_framin_field, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df_framin_field))
  mean(ss[, 3])}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
