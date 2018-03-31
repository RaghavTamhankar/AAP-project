
####################################################################################
### Data partition                                                               ###
####################################################################################
### create dummy predicted variable
set.seed(1234)

levels(demo_base$Sold) <- make.names(levels(factor(demo_base$Sold)))
demo_base$Sold = relevel(demo_base$Sold,"X1")
table(demo_base$Sold)

trainIndex_base <- createDataPartition(demo_base$Sold 
                                  , p = 0.80    
                                  , times = 1   
                                  , list = F)
training_base <- demo_base[trainIndex_base,]
testing_base <- demo_base[-trainIndex_base,]

trainIndex_kmeans <- createDataPartition(demo_kmeans$Sold 
                                       , p = 0.80    
                                       , times = 1   
                                       , list = F)
training_kmeans <- demo_base[trainIndex_kmeans,]
testing_kmeans <- demo_base[-trainIndex_kmeans,]

trainIndex_HDBSCAN <- createDataPartition(demo_HDBSCAN$Sold 
                                       , p = 0.80    
                                       , times = 1   
                                       , list = F)
training_HDBSCAN <- demo_base[trainIndex_HDBSCAN,]
testing_HDBSCAN <- demo_base[-trainIndex_HDBSCAN,]
