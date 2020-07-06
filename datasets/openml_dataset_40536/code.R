# libraries
library(dplyr)
library(OpenML)
source('create_summary_json.R')

# config
set.seed(1)
source <- 'openml'

# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 40536L
data_name <- list_all_openml_dataset[list_all_openml_dataset[, 'data.id']==openml_id, 'name']

dataset_openml <- getOMLDataSet(data.id=openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features

# preprocessing
## cleaning types of columns
dataset <- dataset_raw
dataset$d_age[is.na(dataset$age) | is.na(dataset$age_o)] <- NA

## create json
summary_final_json <- CreateSummary(data=dataset, target_column=target_column, id=openml_id, data_name=data_name, source='openml', added_by='PlentyCoups')
write(summary_final_json, 'dataset.json')
