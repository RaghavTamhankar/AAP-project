
####################################################################################
### Feature Engineering                                                          ###
####################################################################################

str(demo$QTY_SOLD_CY)
demo$Sold = ifelse(!demo$QTY_SOLD_CY == 0.0, 1, 0)
table(demo$Sold)

### remove non-relevant features, CY data is unknown
str(demo)

drops <- c("sku_store","SS_SALES_CY","CONV_RATE","LOST_QTY_CY","AVG_UNIT_CY", "LIFECYCLE_CY","FAILURE_SALES_CY","NEW_MPOG_ID",
           "AVG_LOST_CY","AVG_TOTAL_CY","LOOKUP_COUNT_CY","LOOKUP_COUNT_PY","TOTAL_VIO_CY","UNADJ_TOTAL_VIO_CY","SKU_EXISTENCE_CY")
demo <- demo[,!(names(demo) %in% drops)]

## Removing columns which were used to form clusters
demo$FAILURE_SALES_PY<-NULL
demo$PCT_LIFECYCLE_REMAINING<-NULL

### missing Values
sum(is.na(demo))
missing <- data.frame(sapply(demo, function(x) sum(is.na(x))))
names(missing) <- c("NumMissing")
missing = subset(missing, missing$NumMissing != 0)
missing

demo = demo[!(rowSums(is.na(demo))),]

demo$QTY_SOLD_PPY = as.numeric(as.character(demo$QTY_SOLD_PPY))
demo$QTY_SOLD_PY = as.numeric(as.character(demo$QTY_SOLD_PY))
demo$SS_SALES_PY = as.numeric(as.character(demo$SS_SALES_PY))
demo$LOST_QTY_PY = as.numeric(as.character(demo$LOST_QTY_PY))
demo$AVG_UNIT_PY = as.numeric(as.character(demo$AVG_UNIT_PY))
demo$AVG_LOST_PY = as.numeric(as.character(demo$AVG_LOST_PY))
demo$AVG_TOTAL_PY = as.numeric(as.character(demo$AVG_TOTAL_PY))
demo$SOLD_SINCE_MAXI = as.numeric(as.character(demo$SOLD_SINCE_MAXI))
demo$LIFECYCLE_PY = as.numeric(as.character(demo$LIFECYCLE_PY))
demo$QTY_SOLD_CY = as.numeric(as.character(demo$QTY_SOLD_CY))
demo$Kmeans_Cluster = as.factor(demo$Kmeans_Cluster)
demo$HDBSCAN_Cluster = as.factor(demo$HDBSCAN_Cluster)

sum(is.na(demo))
demo[is.na(demo)] <- 0

### 
str(demo)
demo_cla = demo[,!names(demo) %in% c("QTY_SOLD_CY","Kmeans_Cluster","HDBSCAN_Cluster","Sold")]

# pre-process data by using yeo-johnson, then z-scale transform (ignore target # variable)
pp_hpc <- preProcess(demo_cla, method = c("center", "scale", "YeoJohnson"))
pp_hpc

transformed <- predict(pp_hpc, newdata = demo_cla)
head(transformed)
str(transformed)


##MAking 3 different datasets with different clusters
demo_base = cbind(transformed,demo$Sold)
demo_kmeans = cbind(transformed,demo$Kmeans_Cluster,demo$Sold)
demo_HDBSCAN = cbind(transformed,demo$HDBSCAN_Cluster,demo$Sold)

##Renaming columns
colnames(demo_base)[colnames(demo_base)=="demo$Sold"] <- "Sold"
colnames(demo_kmeans)[colnames(demo_kmeans)=="demo$Sold"] <- "Sold"
colnames(demo_HDBSCAN)[colnames(demo_HDBSCAN)=="demo$Sold"] <- "Sold"
colnames(demo_kmeans)[colnames(demo_kmeans)=="demo$Kmeans_Cluster"] <- "Kmean_Cluster"
colnames(demo_HDBSCAN)[colnames(demo_HDBSCAN)=="demo$HDBSCAN_Cluster"] <- "HDBSCAN_Cluster"

#Converting target variable as factor
demo_HDBSCAN$Sold<-as.factor(demo_HDBSCAN$Sold)
demo_base$Sold<-as.factor(demo_base$Sold)
demo_kmeans$Sold<-as.factor(demo_kmeans$Sold)
