# libraries

source('./create_summary_json.R')

# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 	23381L
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.
colnames(dataset_raw)[1:12] <- c('Style', 'Price', 'Rating', 'Size', 'Season', 'NeckLine',
                                   'SleeveLength', 'waiseline', 'Material', 'FabricType',
                                   'Decoration', 'Pattern')
# To lower 
zmienne <- c('Style', 'Price', 'Size', 'Season', 'NeckLine',
             'SleeveLength', 'waiseline', 'Material', 'FabricType',
             'Decoration', 'Pattern')
for (i in zmienne){
  dataset_raw[,i] <- tolower(dataset_raw[,i])
}

dataset_raw$Season <- ifelse(dataset_raw$Season=='automn','autumn',dataset_raw$Season)

d <- ifelse(dataset_raw$Rating==0,TRUE,FALSE)
dataset_raw$Rating[d] <- NA

for (i in zmienne){
  dataset_raw[,i] <- as.factor(dataset_raw[,i])
}

dataset <- dataset_raw
## create json
summary_1<- CreateSummary(data = dataset_raw, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'jjanborowka')
write(summary_1,'./dataset.json')


