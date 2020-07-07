# libraries
library(OpenML)
source('create_summary_json.R')

# config
set.seed(1)
source <- 'openml'


# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- 1590L
data_name <- list_all_openml_dataset[list_all_openml_dataset[,'data.id'] == openml_id,'name']

dataset_openml <- getOMLDataSet(data.id = openml_id)
dataset_raw <- dataset_openml$data
target_column <- dataset_openml$target.features


# preprocessing
## cleaning types of columns, removing columns etc.
dataset_raw <- dataset_raw[, -c(3, 5)]
# fnlwgt nic istotnego nie mówi, a education.num jest jednoznaczny z education
dataset_raw[,'age'] <- as.integer(dataset_raw[,'age'])
# wiek jest liczbą całkowitą
czynnik <- sapply(dataset_raw, class)=="factor"
for (i in 1:13) if (czynnik[i]){
  dataset_raw[,i] <- tolower(as.character(dataset_raw[,i]))
  if (i==12){
    dataset_raw[!is.na(dataset_raw$native.country) & dataset_raw$native.country=='hong',
                'native.country'] <- 'hong-kong'
    dataset_raw[!is.na(dataset_raw$native.country) & dataset_raw$native.country=='holand-netherlands',
                'native.country'] <- 'netherlands'
    dataset_raw[!is.na(dataset_raw$native.country) & dataset_raw$native.country=='trinadad&tobago',
                'native.country'] <- 'trinidad&tobago'
  }
  dataset_raw[,i] <- factor(dataset_raw[,i],
                            levels=unique(dataset_raw[,i]),
                            ordered=F)
}
# konwersja czynników do małych liter i poprawa niektórych nazw
dataset_raw[,'relationship'] <- as.character(dataset_raw[,'relationship'])
dataset_raw[dataset_raw$relationship %in% c("husband", "wife"),'relationship'] <- "married"
dataset_raw[,'relationship'] <- factor(dataset_raw[,'relationship'],
                                       levels=unique(dataset_raw[,'relationship']),
                                       ordered=F)
# "husband" i "wife" można sprowadzić do jednej kategorii "married", ponieważ mamy już gdzie indziej określoną płeć;
# zdarzały się oczywiście bardzo nieliczne przypadki "husband"/"female" (raz) i "wife"/"male" (trzy razy),
# ale można je interpretować np. jako posiadanie męża zamiast bycia mężem
dataset <- dataset_raw

## create json
plik <- CreateSummary(data = dataset, target_column = target_column, id = openml_id, data_name = data_name, source = 'openml', added_by = 'Z-Xbeova')
write(plik, 'dataset.json')
