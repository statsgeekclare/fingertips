
# Load Packages -----------------------------------------------------------


library(ggplot2)  
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(mclust)
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
library(pvclust)
library(fpc)




# Load Data ---------------------------------------------------------------

#Read in csv
df <- read.csv('Input/Optimising Data/UTLA2.csv')

# Currently included: 
  #age
  #ethnicity
  #Economic status
  #Disability
  #highest level of qualification
  #Population density

#remove NAs
df <- na.omit(df)

#Remove AreaCode from df
df_geography <-df[ -c(2) ]

#Set index as area names and remove geography column
rownames(df_geography) <- df_geography$geography
df_geography$geography <- NULL


# Normalise ---------------------------------------------------------------

#Normalise and scale dataframes
z <- df[,-c(2,1)] #omit the geography names and codes
means <- apply(z,2,mean) #calculate means of variables
sds <- apply(z,2,sd) #calculate standard deviation of variables
nor <- scale(z,center=means,scale=sds) # scale using means and std

#Using just "scale" can also be used as shown here with the variables dataframe
df_variables <- as.data.frame(scale(df_geography)) # standardise variables







# Explore the Data --------------------------------------------------------

dim(df_variables)
str(df_variables)

names(df_variables)

head(df_variables)


pairs(df[3:12])

#Computes a distance matrix between the rows of a data matrix. 
#Compared to the standard dist() function, it supports correlation-based distance measures 
#including "pearson", "kendall" and "spearman" methods.
distance <- get_dist(nor)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) #Visualizes a distance matrix

#get summaries of the means of each variable
var_means <- df_geography %>% summarise_each(funs(mean))

# PCA Analysis ------------------------------------------------------------


#PCA

install.packages("corrr")
library('corrr')

install.packages("ggcorrplot")
library(ggcorrplot)

corr_matrix <- cor(nor)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]

#Visualise PCA
install.packages("FactoMineR")
library("FactoMineR")
fviz_screeplot(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



## FOR PRESENTATION - SMALLER FEATURES

df_presentation <- df_variables %>%
  select(Lessthan18yr,age65to84yr,nonwhite,withdisability)

pairs(df_presentation)

df_presentation2 <- df_variables %>%
  select(Lessthan18yr)

distance_pres <- get_dist(df_presentation2)
fviz_dist(distance_pres, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) #Visualizes a distance matrix


# Find Optimum Clusters ---------------------------------------------------

#Computes a distance matrix between the rows of a data matrix. 
#Compared to the standard dist() function, it supports correlation-based distance measures 
#including "pearson", "kendall" and "spearman" methods.
distance <- get_dist(nor)

#METHOD1
      wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
      for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
      plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
           main = "Elbow Method 1")

#METHOD2            
    # function to compute total within-cluster sum of square 
    wss <- function(k) {
      kmeans(df_variables, k, nstart = 10 )$tot.withinss
      }
    # Compute and plot wss for k = 1 to k = 20
    k.values <- 1:20
    # extract wss for 2-15 clusters
    wss_values <- map_dbl(k.values, wss)
    plot(k.values, wss_values,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of clusters K",
         ylab="Total within-clusters sum of squares",
         main = "Elbow Method 2")

#METHOD 3

    # Determine number of clusters
    wss <- (nrow(df_variables)-1)*sum(apply(df_variables,2,var))
    for (i in 2:20) wss[i] <- sum(kmeans(df_variables,
                                         centers=i)$withinss)
    plot(1:20, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares",main = "Elbow Method 3")

#METHOD 4
    
    set.seed(123)
    # Dertermining and Visualizing the Optimal Number of Clusters
    fviz_nbclust(df_variables, kmeans, method = "wss")


# Silhouette --------------------------------------------------------------

mydata.hclust = hclust(distance)
plot(silhouette(cutree(mydata.hclust,5), distance))

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df_variables, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_variables))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:20
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")




fviz_nbclust(df_variables, kmeans, method = "silhouette")

s2<-kmeans(df_variables,2,iter.max=100,nstart=50,algorithm="Lloyd")
sp2<-plot(silhouette(s2$cluster,dist(df_variables,"euclidean")))

s3<-kmeans(df_variables,3,iter.max=100,nstart=50,algorithm="Lloyd")
sp3<-plot(silhouette(s3$cluster,dist(df_variables,"euclidean")))

s4<-kmeans(df_variables,4,iter.max=100,nstart=50,algorithm="Lloyd")
sp4<-plot(silhouette(s4$cluster,dist(df_variables,"euclidean")))

s5<-kmeans(df_variables,5,iter.max=100,nstart=50,algorithm="Lloyd")
sp5<-plot(silhouette(s5$cluster,dist(df_variables,"euclidean")))

s6<-kmeans(df_variables,6,iter.max=100,nstart=50,algorithm="Lloyd")
sp6<-plot(silhouette(s6$cluster,dist(df_variables,"euclidean")))

s7<-kmeans(df_variables,7,iter.max=100,nstart=50,algorithm="Lloyd")
sp7<-plot(silhouette(s7$cluster,dist(df_variables,"euclidean")))

s8<-kmeans(df_variables,8,iter.max=100,nstart=50,algorithm="Lloyd")
sp8<-plot(silhouette(s8$cluster,dist(df_variables,"euclidean")))

s9<-kmeans(df_variables,9,iter.max=100,nstart=50,algorithm="Lloyd")
sp9<-plot(silhouette(s9$cluster,dist(df_variables,"euclidean")))

s10<-kmeans(df_variables,10,iter.max=100,nstart=50,algorithm="Lloyd")
sp10<-plot(silhouette(s10$cluster,dist(df_variables,"euclidean")))

s15<-kmeans(df_variables,15,iter.max=100,nstart=50,algorithm="Lloyd")
sp10<-plot(silhouette(s10$cluster,dist(df_variables,"euclidean")))

s20<-kmeans(df_variables,20,iter.max=100,nstart=50,algorithm="Lloyd")
sp10<-plot(silhouette(s10$cluster,dist(df_variables,"euclidean")))



# Gap Statistic -----------------------------------------------------------

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df_variables, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)


# Elbow Method -------------------------------------------------------------


set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(df_variables,k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


# 6. Visualizing and Interpreting Results

# Hierarchical ------------------------------------------------------------


mydata.hclust = hclust(distance)
plot(mydata.hclust,hang=-1, labels=df$geography,main='Hierarchical Clustering',las = 2,cex = 0.6)

member = cutree(mydata.hclust,3)
table(member)

# Open a PDF device with a larger size
pdf("my_hierarchical_clustering_plot.pdf", width = 12, height = 8)

# Hierarchical clustering plot with rotated x-axis labels, increased margin, and smaller text size
mydata.hclust = hclust(distance)
par(mar = c(5, 4, 2, 2))  # Set margin to c(bottom, left, top, right)
plot(mydata.hclust, hang = -1, labels = df$geography, main = 'Hierarchical Clustering', las = 2, cex = 0.8)

# Close the PDF device
dev.off()

# Hierarchical Clustering
d <- dist(df,
          method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")

plot(fit) # display dendogram
groups <- cutree(fit, k=15) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=15, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
#fit <-
#  pvclust(df, method.hclust="ward",
#          method.dist="euclidean")
#plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
#pvrect(fit, alpha=.95)






# K means -----------------------------------------------------------------

set.seed(123)
kc<-kmeans(nor,3)
kc

k2 <- kmeans(df_variables, centers = 2, nstart = 25)
k2

k3 <- kmeans(df_variables, centers = 3, nstart = 25)
k4 <- kmeans(df_variables, centers = 4, nstart = 25)
k5 <- kmeans(df_variables, centers = 5, nstart = 25)
k6 <- kmeans(df_variables, centers = 6, nstart = 25)
k7 <- kmeans(df_variables, centers = 7, nstart = 25)
k8 <- kmeans(df_variables, centers = 8, nstart = 25)
k9 <- kmeans(df_variables, centers = 9, nstart = 25)
k10 <- kmeans(df_variables, centers = 10, nstart = 25)
k11 <- kmeans(df_variables, centers = 11, nstart = 25)
k12 <- kmeans(df_variables, centers = 12, nstart = 25)
k13 <- kmeans(df_variables, centers = 13, nstart = 25)
k14 <- kmeans(df_variables, centers = 14, nstart = 25)
k15 <- kmeans(df_variables, centers = 15, nstart = 25)
k16 <- kmeans(df_variables, centers = 16, nstart = 25)
k17 <- kmeans(df_variables, centers = 17, nstart = 25)
k18 <- kmeans(df_variables, centers = 18, nstart = 25)
k19 <- kmeans(df_variables, centers = 19, nstart = 25)
k20 <- kmeans(df_variables, centers = 20, nstart = 25)
k21 <- kmeans(df_variables, centers = 21, nstart = 25)




# Model Based Clustering
fit <- Mclust(df_variables)
plot(fit) # plot results

summary(fit) # display the best model



# Visualise Clusters ------------------------------------------------------

ot<-nor
datadistshortset<-dist(ot,method = "euclidean")
hc1 <- hclust(datadistshortset, method = "complete" )
pamvshortset <- pam(datadistshortset,10, diss = FALSE)
clusplot(pamvshortset, shade = FALSE,col.clus="blue",col.p="red",span=FALSE,main="Cluster Mapping",cex=1.2)

pamvshortset <- pam(datadistshortset,2, diss = FALSE)
clusplot(pamvshortset, shade = FALSE,labels=2,col.clus="blue",col.p="red",span=FALSE,main="Cluster Mapping",cex=1.2)

pamvshortset <- pam(datadistshortset,6, diss = FALSE)
clusplot(pamvshortset, shade = FALSE,labels=2,col.clus="blue",col.p="red",span=FALSE,main="Cluster Mapping",cex=1.2)



# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_variables) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_variables) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_variables) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_variables) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph

# CHECK THESE!

#        clusplot(df_variables, fit$cluster, color=TRUE, shade=TRUE,
#                 labels=2, lines=0)

        # Centroid Plot against 1st 2 discriminant functions
#        plotcluster(mydata, fit$cluster)
        
        # comparing 2 cluster solutions
#        cluster.stats(d, fit1$cluster, fit2$cluster)


# plots to compare
p5 <- fviz_cluster(k6, geom = "point", data = df_variables) + ggtitle("K-means clustering, k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = df_variables) + ggtitle("k = 7")
p7 <- fviz_cluster(k8, geom = "point",  data = df_variables) + ggtitle("k = 8")
p8 <- fviz_cluster(k9, geom = "point",  data = df_variables) + ggtitle("k = 9")
grid.arrange(p5, p6, p7, p8, nrow = 2)
p9 <- fviz_cluster(k10, geom = "point", data = df_variables) + ggtitle("k = 10")
p10 <- fviz_cluster(k11, geom = "point",  data = df_variables) + ggtitle("k = 11")
p11 <- fviz_cluster(k12, geom = "point",  data = df_variables) + ggtitle("k = 12")
p12 <- fviz_cluster(k13, geom = "point",  data = df_variables) + ggtitle("k = 13")
grid.arrange(p9, p10, p11, p12, nrow = 2)
p13 <- fviz_cluster(k14, geom = "point", data = df_variables) + ggtitle("k = 14")
p14 <- fviz_cluster(k15, geom = "point",  data = df_variables) + ggtitle("K-means clustering, k = 15")
p15 <- fviz_cluster(k16, geom = "point",  data = df_variables) + ggtitle("k = 16")
p16 <- fviz_cluster(k17, geom = "point",  data = df_variables) + ggtitle("k = 17")
grid.arrange(p13, p14, p15, p16, nrow = 2)
p17 <- fviz_cluster(k18, geom = "point", data = df_variables) + ggtitle("k = 18")
p18 <- fviz_cluster(k19, geom = "point",  data = df_variables) + ggtitle("k = 19")
p19 <- fviz_cluster(k20, geom = "point",  data = df_variables) + ggtitle("K-means clustering, k = 20")
p20 <- fviz_cluster(k21, geom = "point",  data = df_variables) + ggtitle("k = 21")
grid.arrange(p17, p18, p19, p20, nrow = 2)


p19
p14
p5

# Analysis ----------------------------------------------------------------

pcclust=prcomp(df_variables,scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k20$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))




# Add clusters to data ----------------------------------------------------

df3 <- df %>%
  mutate(cluster = k20$cluster) %>%
  group_by(cluster) %>%
#  summarise_all("mean") %>%
  select(geography.code,cluster) %>%
  rename(AreaCode = geography.code)






# Append clusters to data and create rank 

#Load in data

df4 <- read.csv('Input/Data.csv')

#Merge data with clusters

df5 = merge(x = df4, y = df3, by = "AreaCode", all.x = TRUE)

# Rank by Topic_number,IndicatorID,cluster,Timeperiod


df6 <- df5 %>%
  group_by(Topic_number,IndicatorID,cluster,Timeperiod) %>%
  mutate(rank = rank(Value)) %>%
  mutate(total_count = n())



#NEED TO ADD SOMETHING ABOUT CHECKING FOR INDICATORS AND RANKINGS

#Add column for rank in cluster

df7 <- df6 %>%
  mutate(cluster_rank = paste0(rank,"/",total_count))


#check

df8 <- df7 %>% 
  filter(Topic_number == 1) %>%
  filter(IndicatorID == 20601) %>%
  filter(AreaTypeID == 502) %>%
  filter(Timeperiod == "2021/22") %>%
  filter(cluster == 1) %>%
  select(AreaCode,AreaName,IndicatorName,Value,cluster,cluster_rank)


df9 <- df7 %>% 
  filter(Topic_number == 1) %>%
  filter(IndicatorID == 20601) %>%
  filter(AreaTypeID == 502) %>%
  filter(Timeperiod == "2021/22") %>%
  filter(cluster == 5) %>%
  select(AreaCode,AreaName,IndicatorName,Value,cluster,cluster_rank)

df10 <- rbind(df8,df9)



write.csv(df10, "Test.csv", row.names=FALSE)





