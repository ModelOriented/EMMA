# libraries
library(OpenML)
library(dplyr)
source('create_summary_json.R')

# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 41278L
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.

## usunięcie kolumny z datą (typu character) - last_online.
dataset_raw <- dataset_raw[,-11]

## usunięcie wszystkich rekordów opisujących studentów, dzięki czemu target (job) będzie mieć tylko dwa poziomy
dataset_raw <- dataset_raw%>%filter(job!="student")
dataset <- dataset_raw


## create json

file <- CreateSummary(data = dataset, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'MaroonBlue')
write(file, 'dataset.json')
