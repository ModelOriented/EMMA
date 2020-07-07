# libraries
library(OpenML)
library(dplyr)
library(tidyverse)
source('create_summary_json.R')

# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 38
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.
dataset <- dataset_raw %>% 
  # drop 'TBG' column: whole is empty, 'TBG_measured' column: has one same value
  select(-TBG, -TBG_measured) %>%
  # change unrealistic age values to NAs
  mutate(age=ifelse(age>123, NA, age))
  
## create json
file <- CreateSummary(data = dataset, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'okcze and p-przybylek')
write(file, 'dataset.json')
