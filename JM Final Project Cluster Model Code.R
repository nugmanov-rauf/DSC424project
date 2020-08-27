

#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions / Hierarchical Clustering
library(ade4) #PCA Visualizations
library(cluster) #Basic Clustering Algorithms
library(fpc) #DBSCAN
library(dbscan) #DBSCAN
library(mclust) #Model Clustering
library(kernlab) #Spectral Clustering


#Set Working Directory
setwd("C:/Users/jmajor/Desktop/Tribune Publishing/Personal/DePaul/DSC 424 - Advanced Data Analysis/Final Project/Dataset")

#Read in Dataset
CPS_ReportCard_Sample <- read.csv(file="CPS_ReportCard_Sample.csv", header=TRUE, sep=",")



# Need to revisit
#dataSet$`Instruction Score`[which(is.na(dataSet$`Instruction Score`))] = mean(dataSet$`Instruction Score`,na.rm = TRUE)
#dataSet$`Average Student Attendance`[which(is.na(dataSet$`Average Student Attendance`))] = mean(dataSet$`Average Student Attendance`,na.rm = TRUE)
#dataSet$`College Enrollment (number of students) `[which(is.na(dataSet$`College Enrollment (number of students) `))] = mean(dataSet$`College Enrollment (number of students) `,na.rm = TRUE)




# Choose which fields to cluster
CPS_Subset = CPS_ReportCard_Sample[,c(2,13,22,23,54)]

# Transfer into dataframe
df = data.frame(CPS_Subset)

# Add School Name in Rows
rownames(df)<-df[,1]

# Remove School Name in Data
df = df[,c(2,3,4,5)]

# Remove NAs
df <- na.omit(df)

#For All Variables
sum(is.na(df))


df_scale <- scale(df) 


#K-Means Clustering

#Determining Optimal Number of Clusters
fviz_nbclust(df_scale, kmeans)


#Run K-Means Cluster Analysis

set.seed(123)
df_k2 <- kmeans(df_scale, centers=5, iter.max=100, nstart = 25)
df_k2


# Cluster size
df_k2$size

# Cluster means
df_k2$centers

# Visualize
library("factoextra")
fviz_cluster(df_k2, data = df_scale,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())



# May not need this...
my.data.matrix <- df_scale


my.k.choices <- 2:5
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)




#Alternative Way of Running K-Means Clustering

library("factoextra")
# K-means clustering
df_kmeans2 <- eclust(df_scale, "kmeans", k = 5,
                            nstart = 25, graph = FALSE)

df_kmeans2

# Visualize the silhouette of clusters
fviz_silhouette(df_kmeans2)




#K-Mediods Clustering

#Run PAM Algorithm
library(cluster)
# K-medoids directly on the (standardized) data matrix:
df_kmed_2 <- pam(df_scale, k=5, diss=F)
df_kmed_2


# Visualize
fviz_cluster(df_kmed_2, data = df_scale,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())


df_kmed_2$clustering  # printing the "clustering vector"

df_kmed_2$silinfo$avg.width  #printing the average silhouette width



df_med_k3_clust <- lapply(1:3, function(nc) row.names(df)[df_kmed_2$clustering==nc])  
df_med_k3_clust 


df_clara <- clara(df_scale, 5, samples = 50, pamLike = TRUE)

# Print components of clara 
print(df_clara)

# Visualize
fviz_cluster(df_clara, data = df_scale,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())











