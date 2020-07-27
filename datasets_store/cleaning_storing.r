#Pre-processing data sets and saving information about them
library(OpenML)
library(readr)
library(dplyr)
library(janitor)
library(tidyselect)

source("./datasets_store/create_new_summary.r")

#Reading list of selected data sets
datasets <- read_csv("./datasets_store/datasets_selection/selected_datasets.csv")

check_type <- function(x){
  is.numeric(x) | is.factor(x)
}

data_types_troubles <- c()
miss_in_target <- c()

#Temporary test on 20 datasets
for(id in datasets$ID[1:20]){
  
  df_oml <- getOMLDataSet(id)
  df_desc <- df_oml$desc
  df <- df_oml$data
  
  ### Preprocessing step ###
  
  #Columns to ignore according to tags
  to_ignore <- df_desc$ignore.attribute
  if(!is.na(to_ignore)){
    df <- select(df, -all_of(to_ignore))  
  }
  
  #Removing row ID column according to tags
  row_id <- df_desc$row.id.attribute
  if(!is.na(row_id)){
    df <- select(df, -all_of(row_id))  
  }
  
  #Data types
  data_types_check <- sapply(df, check_type)
  
  if(any(!data_types_check)){
    wrong_columns <- colnames(df)[!data_types_check]
    
    #Trying possible conversions
    for(col in wrong_columns){
      
      if(is.logical(df[,col]) | is.character(df[,col])){
        df[,col] <- as.factor(df[,col])
      }else{
        warning(paste("Data type trouble ID: ", as.character(id)))
        data_types_troubles <- c(data_types_troubles, id)
      }
    }
  }
  
  #Removing possible duplicate rows
  df <- df[!duplicated(df), ]
  
  #Removing constant columns
  df <- remove_constant(df)  
  
  #Removing empty columns
  df <- remove_empty(df, "cols")
  
  #All to lowercase
  df <- mutate_if(df, is.factor, function(x) factor(tolower(x)))
  
  #Missing values in target variable
  target <- df_oml$target.features
  if(any(is.na(df[, target]))){
    miss_in_target <- c(miss_in_target, id)
    warning(paste("Missing values in target variable ID: ", as.character(id)))
  }
  
  #Categorical columns with high fraction of unique values
  #?
  
  ### Collecting information step ###
  json_summary <- create_summary(df_oml, df_desc, df)
  write(json_summary, paste("./datasets_store/information_base/dataset_", id, ".json", sep = ""))
}

#example_json <- fromJSON("./datasets_store/information_base/dataset_25.json")

