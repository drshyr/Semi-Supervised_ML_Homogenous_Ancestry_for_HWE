#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model-Based Clustering based on Parametrized Finite Gaussian Mixture Models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For individuals who are classified as "Other" from Random Forest, used Model-Based Clustering to find groups

##data: Individuals classified as "Other" from Random Forest.
###data - rows are individuals, columns are PCs from TRACE
##ID: Sample IDs
library('mclust')

fit <- Mclust(data)
plot(fit) # plot results
summary(fit) # display the best model 
data_classified<-data.frame(Sample=ID,data,Cluster=fit$classification)
write.table(data_classified,"data_clusters.txt",row.names=F,col.names=T,quote=F)

###After obtaining the clusters, decide cluster classification, i.e., super population classification, by comparing Self-reported and Predicted Ethnicity Prediction 
###Combine RF + Cluster classification for final results