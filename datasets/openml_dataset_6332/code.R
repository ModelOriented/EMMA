# libraries
library(OpenML)
source('./create_summary_json.R')

set.seed(123)
source <- 'openml'

# download data
list_all_openml_dataset <- listOMLDataSets()

openml_id <- list_all_openml_dataset[list_all_openml_dataset$name == 'cylinder-bands', 'data.id']
dataset_openml <- getOMLDataSet(data.id = openml_id)
data <- dataset_openml$data
target_column <- dataset_openml$target.features

# preprocessing
## encode missing data with the appropriate symbol
data[data == "?"] <- NA 

## getting rid of irrelevant columns
# data <- data[, -c(2,6,8,9,12,23)] 
data <- data[, -c(1,2,4,6,8,9,12,23)]

## Label-encoding
df <- data

label_cols <- c('customer', 'paper_type', 'ink_type', 'solvent_type', 'press_type', 'cylinder_size', 'paper_mill_location', 'grain_screened', 'proof_on_ctd_ink', 'type_on_cylinder')
for (col in label_cols){
  df[, col] <- as.numeric(df[, col])
  df[which(is.na(data[, col]), arr.ind = TRUE), col] <- NA
  df[, col] <- as.factor(df[,col])
}

## Types conversion
#df$job_number <- as.factor(df$job_number)
to_numeric <- c('ink_temperature', 'roughness', 'varnish_pct', 'solvent_pct', 'ink_pct', 'wax', 'hardener', 'anode_space_ratio')
to_integer <- c('blade_pressure', 'proof_cut', 'viscosity', 'ESA_Voltage', 'roller_durometer', 'current_density', 'chrome_content', 'humifity', 'press_speed')

for (col in to_numeric){
  df[, col] <- as.numeric(df[, col])
}

for (col in to_integer) {
  df[, col] <- as.integer(df[, col])
}

dataset <- df

## create json
file <- CreateSummary(data = df, target_column = target_column, id = openml_id, data_name = 'cylinder-bands', source = 'openml', added_by = 'ejowik')
#write(file, 'dataset.json')