#Pre-processing data sets and saving information about them
library(OpenML)
library(readr)
library(dplyr)
library(janitor)
library(tidyselect)

source("./datasets_store/create_new_summary.r")
source("./datasets_store/preprocess.R")

#Reading list of selected data sets
datasets <- read_csv("./datasets_store/datasets_selection/selected_datasets.csv")

data_types_troubles <- c()
miss_in_target <- c()

i <- 1
for(id in datasets$ID[155]){
  
  df_oml <- getOMLDataSet(id)
  df_desc <- df_oml$desc
  df <- df_oml$data
  
  if(id==940 | id==565){
    #Spelling mistake in OML description
    df_desc$ignore.attribute <- c('date', 'Q.E') 
  }
  if(id==41704){
    #Spelling mistake in OML description
    df_desc$ignore.attribute <- "instance_id" 
  }
  
  
  ### Preprocessing ###
  cleaned_df <- preprocess(df, df_desc, df_oml)
  df <- cleaned_df$df
  
  if(cleaned_df$data_types_troubles){
    write.table( id,  
                 file="./datasets_store/information_base/data_types_troubles.csv", 
                 append = T, 
                 sep=',', 
                 row.names=F, 
                 col.names=F )
  }
  if(cleaned_df$miss_in_target){
    write.table( id,  
                 file="./datasets_store/information_base/miss_in_target_troubles.csv", 
                 append = T, 
                 sep=',', 
                 row.names=F, 
                 col.names=F )
  }
  
  ### Collecting information step ###
  summary <- create_summary(df_oml, df_desc, df)
  json_summary <- summary$summary_json
  missings_pattern <- summary$missings_pattern
  
  write(json_summary, paste("./datasets_store/information_base/dataset_", id, ".json", sep = ""))
  write.csv(missings_pattern, paste("./datasets_store/patterns_base/dataset_", id, ".csv", sep = ""))
  
  #Cleaning cache
  i <- i+1
  if(i==20){
    clearOMLCache()
    i <- 1
  }
}

#Troubles detection (founded missings in target variable, no problems with data types)
detected_miss_in_target <- read_csv("datasets_store/information_base/miss_in_target_troubles.csv")
#Removing from selected data sets
selected_datasets <- read_csv("./datasets_store/datasets_selection/selected_datasets.csv")
selected_datasets <- selected_datasets[!selected_datasets$ID %in% detected_miss_in_target$X1, ]
#write.csv(selected_datasets, "./datasets_store/datasets_selection/selected_datasets.csv")

#example_json <- fromJSON("./datasets_store/information_base/dataset_25.json")
#example_pattern <- read_csv("./datasets_store/patterns_base/dataset_25.csv")

