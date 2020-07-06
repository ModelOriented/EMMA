source_list <- list.files('datasets/', recursive = TRUE, pattern = 'code.R', full.names = FALSE)

dataset_list <- list()

for(i in seq_along(source_list)){
  setwd(paste0('datasets/', dirname(source_list)[i]))
  source(paste0('datasets/', source_list[i]))
  dataset_list[[length(dataset_list)+1]] <- list(data = dataset, target_name = target_column)
 }
names(dataset_list) <- dirname(source_list)

saveRDS(dataset_list, 'dataset_raw.Rds')


### train/test split
train_percent_of_data <- 0.8
dataset_train_test_list <- list()
for(i in seq_along(dataset_list)){
  
  set.seed(123)
  no_instances <- nrow(dataset_list[[i]]$data)
  
  train_index <- sort(sample(1:no_instances, floor(train_percent_of_data * no_instances)))
  
  write.csv(x = data.frame(dataset = names(dataset_list)[i], index = train_index), 
            file = paste0('./train_index/train_index_', names(dataset_list)[i], '.csv'))
  
  dataset_train_test_list[[length(dataset_train_test_list)+1]] <- list(data_train  = dataset_list[[i]]$data[train_index,],
                                                                        data_test  = dataset_list[[i]]$data[-train_index,],
                                                                        target_name = dataset_list[[i]]$target_name)
  

}


names(dataset_train_test_list) <- names(dataset_list)


saveRDS(dataset_train_test_list, 'dataset_train_test_list.Rds')


### properties of datasets
library(jsonlite)
library(xtable)
library(dplyr)
library(tidyr)

json_list <- list.files('datasets/', recursive = TRUE, pattern = 'dataset.json', full.names = TRUE)

json_list_file <- lapply(json_list, function(x) fromJSON(paste(readLines(x)), flatten=TRUE))
json_list_file <- json_list_file[-10]


dataset_list <- readRDS('dataset_raw.Rds')

prc_missing_data <- sapply(dataset_list, function(x) mean(is.na(x$data)))
prc_missing_data <- prc_missing_data[-10]   




summary_datasets_table <- data.table::rbindlist(lapply(json_list_file,
       function(x)  data.frame(x[c('id', 'data_name', 'number_of_instances', 'number_of_numeric_features', 'number_of_numeric_features_with_missings',
                                   'number_of_categorical_features', 'number_of_categorical_features_with_missings')]))) 

summary_datasets_table$prc_missings <- prc_missing_data

summary_datasets_table <- summary_datasets_table %>% 
  data.frame() %>% 
  mutate(data_name = paste0(data_name, ' (', id, ')')) %>% 
  select(-id) %>% 
  select(data_name, number_of_instances, prc_missings, 
         number_of_numeric_features, number_of_numeric_features_with_missings, 
         number_of_categorical_features, number_of_categorical_features_with_missings) %>% 
  mutate(prc_missings= paste0(round(prc_missings*100, 1), '%')) %>% 
  rename(`dataset(ID)`= data_name, `no instances` = number_of_instances, `no cont vars`=number_of_numeric_features,
         `no cont vars missing`=number_of_numeric_features_with_missings,
         `no cat vars`=number_of_categorical_features, 
         `no cat vars missing`=number_of_categorical_features_with_missings)

xtable(summary_datasets_table)
 
