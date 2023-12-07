#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random Forest for Predicting Populations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##training_data: N X P matrix
####N is the number of training samples, i.e., 1000 Genome
####P - first column contains the labels, columns 2:P are the PCs from TRACE

###data (test set): N2 X P2 matrix
####N2 is the samples of interest 
####P2 - first column are the sample ids, columns 2:P2 are the PCs from TRACE

pop_forest = function(training_data, data, ntree=100, seed=42, pcs=1:10) {
  # training_data: data.frame with columns 'known_pop' and 'pc1', ..., 'pcn'
  # data: data.frame with columns 'pc1', ..., 'pcn'
  sample.id <- as.numeric(row.names(data))
  set.seed(seed)
  require(randomForest)
  require(tidyr)
  require(dplyr)
  form = formula(paste('as.factor(known_pop) ~', paste0('pc', pcs, collapse = ' + ')))
  forest = randomForest(form,
                        data = training_data,
                        importance = T,
                        ntree = ntree)
  #print(forest)
  fit_data = data.frame(predict(forest, data, type='prob'), combined_sample = data$combined_sample, stringsAsFactors = F)
  out <- fit_data %>%
    gather(predicted_pop, probability, -combined_sample) %>%
    group_by(combined_sample) %>%
    slice(which.max(probability))
  out <- as.data.frame(out)
  out1 <- cbind(sample.id, out)
  return(out1)
}

###Classify individuals who have a classification probability of less than 0.9 as "Other"