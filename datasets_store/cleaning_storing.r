#Pre-processing data sets and saving information about them
library(OpenML)
library(readr)
library(dplyr)
library(janitor)
library(tidyselect)

#Reading list of selected data sets
datasets <- read_csv("./datasets_store/datasets_selection/selected_datasets.csv")

check_type <- function(x){
  is.numeric(x) | is.factor(x)
}

data_types_troubles <- c()

for(id in datasets$ID){
  
  df_oml <- getOMLDataSet(id)
  df_desc <- df_oml$desc
  df <- df_oml$data
  
  ### Pre-processing step ###
  
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
  
  #ID columns detected manually
  
  #Missing values in target variable
  
  #Categorical columns with high fraction of unique values
  
  #### Collecting information step ###
}

  
#temp
df <- t
x <- c(NA,NA)
y <- c(2,3)
z <- c("c", "a")
t <- data.frame(x,y,z, stringsAsFactors = T)

