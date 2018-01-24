## preProcess and clean code here

demo = read.csv("aap.csv", sep=",",nrows=10000, header=T)

drops <- c("SS_SALES_CY","CONV_RATE","LOST_QTY_CY","AVG_UNIT_CY", "LIFECYCLE_CY","FAILURE_SALES_CY","NEW_MPOG_ID",
           "AVG_LOST_CY","AVG_TOTAL_CY","LOOKUP_COUNT_CY","TOTAL_VIO_CY","UNADJ_TOTAL_VIO_CY","SKU_EXISTENCE_CY","LOOKUP_COUNT_PY")
demo <- demo[,!(names(demo) %in% drops)]

full1<-full %>% drop_na()



## final output file with rows utilized for kmeans and kahonen
