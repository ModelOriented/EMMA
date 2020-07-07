# libraries
library(dplyr)
library(OpenML)
source('create_summary_json.R')


# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 188L
data_name <- list_all_openml_dataset[list_all_openml_dataset[, 'data.id']==openml_id, 'name']

dataset_openml <- getOMLDataSet(data.id=openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.
dataset <- dataset_raw %>%
  # transform Latitude from degrees and minutes to degrees with fractions
  mutate(Latitude=-as.numeric(substr(Latitude, 1, 2))-as.numeric(substr(Latitude, 5, 6))/60) %>%
  # cast Sp to unordered factor
  mutate(Sp=factor(Sp, levels=unique(Sp), ordered=F)) %>%
  # change unrealistic Latitude values to NAs
  mutate(Latitude=ifelse(Latitude<(-60), NA, Latitude)) %>%
  # change unrealistics DBH values to NAs
  mutate(DBH=ifelse(DBH>100, NA, DBH)) %>%
  # transform problem to binary classification
  mutate(Utility=factor(ifelse(Utility %in% c('best', 'good'), 1, 0), levels=c(1, 0), ordered=F)) %>%
  # drop some columns
  select(-Abbrev, -Rep, -Locality, -Map_Ref)


## create json
summary_final_json <- CreateSummary(data=dataset, target_column=target_column, id=openml_id, data_name=data_name, source='openml', added_by='PlentyCoups')
write(summary_final_json, 'dataset.json')
