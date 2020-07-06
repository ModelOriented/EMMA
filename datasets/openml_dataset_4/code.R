# libraries
library(OpenML)

source('create_summary_json.R')


# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 4L
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## usunięcie kolumny id

dataset <- dataset_raw


## create json
file<-CreateSummary(data = dataset, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'MaroonBlue')

# podobnie jak source, bez ścieżki nie działa
write(file, 'dataset.json')
