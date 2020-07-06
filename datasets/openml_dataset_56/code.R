# libraries
library(OpenML)
source('create_summary_json.R')

# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 56L
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.
# all NA'a are alreay marked, all columns are somewhat balanced and all columns are factors
dataset <- dataset_raw


## create json only once
#summ <- CreateSummary(data = dataset, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'hazdula')
#write(summ, 'dataset.json')
