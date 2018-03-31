####################################################################################
### Library                                                                      ###
####################################################################################

library(caret)
library(pROC)
library(RSNNS)
library(leaps)
library(data.table)
library(tree)
library(reshape2)
library(e1071)
library(AppliedPredictiveModeling)
library(pscl)
library(boot)
library(kohonen)
library(dbscan)

## load data
setwd("C:/Users/Lenovo/Desktop/Acads/Projects/AAP/Random subset")
full = read.csv("Final_subset.csv",nrows =10000,  header=T)


### missing Values
sum(is.na(full))
missing <- data.frame(sapply(full, function(x) sum(is.na(x))))
names(missing) <- c("NumMissing")
missing = subset(missing, missing$NumMissing != 0)
missing

full = full[, !names(full) %in% c("LIFECYCLE_PRE_PEAK_POST","LIFECYCLE_PRE_PEAK_POST")]
str(full)

### combine sku and store #
full$sku_store = paste(full$SKU_NUMBER,full$STORE_NUMBER,sep="_")

##Make a new dataframe with the desired columns
df<-full[,c("SKU_NUMBER","STORE_NUMBER","PCT_LIFECYCLE_REMAINING","FAILURE_SALES_PY")]


####Clustering on failure rates and lifecycle####


## Dropping rows with NA
df_omit<-na.omit(df)

##Removing ID columns and taking a log transformation of failure sales as it has a skewed distribution
df_val<-df_omit[,c("PCT_LIFECYCLE_REMAINING","FAILURE_SALES_PY")]
df_val$FAILURE_SALES_PY_LOG<-log(df_val$FAILURE_SALES_PY+1)
df_val$FAILURE_SALES_PY<-NULL

##standardizing the values
preProcValues <- preProcess(df_val, method = c("range", "YeoJohnson"))
df_std <- predict(preProcValues, df_val)

plot(df_std$PCT_LIFECYCLE_REMAINING,df_std$FAILURE_SALES_PY_LOG)

#K-means clustering
cost_df <- data.frame() #accumulator for cost results

for(k in 3:10){
  #allow up to 50 iterations to obtain convergence, and do 20 random starts
  
  kmeans_df <- kmeans(x=df_std, centers=k, nstart=20, iter.max=50)
  
  #Combine cluster number and cost together, write to df
  cost_df <- rbind(cost_df, cbind(k, kmeans_df$tot.withinss))
}

names(cost_df) <- c("cluster", "tr_cost")
cost_df

# create an elbow plot
par(mfrow=c(1,1))
cost_df[,2] <- cost_df[,2]/1000
plot(x=cost_df$cluster, y=cost_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=cost_df$cluster, y=cost_df$te_cost, col="green")

##Kmeans with appropriate k
kmeans_df <- kmeans(x=df_std, centers=6, nstart=20, iter.max=50)
clust<-as.data.frame(kmeans_df$cluster)

##Creating final dataframe with all columns and clusters number
Final_df<-df_omit[,c("SKU_NUMBER","STORE_NUMBER")]
Final_df<-cbind(Final_df,df_std)
Final_df<-cbind(Final_df,clust)
colnames(Final_df)[colnames(Final_df)=="kmeans_df$cluster"] <- "Kmeans_Cluster"

##Kohonen Networks
df_koh<-as.matrix(df_std)
som_aap <- som(df_koh,
               grid = somgrid(3, 2),
               rlen = 100,
               alpha = c(0.3, 0.00),
               radius = 2)
plot(som_aap, type="changes")
plot(som_aap, type="codes")
plot(som_aap, type="count")
clust_koh<-as.data.frame(som_aap$unit.classif)

##Adding Kohonen Clusters to Final Dataframe
Final_df<-cbind(Final_df,clust_koh)
colnames(Final_df)[colnames(Final_df)=="som_aap$unit.classif"] <- "Kohonen_Cluster"

##HDBSCAN Sclustering
hd_cl<-hdbscan(df_koh, minPts = 40)
plot(df_koh, col=hd_cl$cluster+1, pch=20)
clust_hd<-as.data.frame(hd_cl$cluster)
Final_df<-cbind(Final_df,clust_hd)
colnames(Final_df)[colnames(Final_df)=="hd_cl$cluster"] <- "HDBSCAN_Cluster"

##Creating key to join the cluster dataframe with the main file
Final_df$sku_store = paste(Final_df$SKU_NUMBER,Final_df$STORE_NUMBER,sep="_")
Final_df1 = Final_df[, c("sku_store","Kmeans_Cluster","HDBSCAN_Cluster")]

### right join on the new cluster data
demo = merge(x=full, y=Final_df1, by = "sku_store", all.y = TRUE)
demo = demo[, !names(demo) %in% c("SKU_NUMBER","STORE_NUMBER")]
