#Selecting data sets with missings, and binary classification or regression tasks

library(OpenML)
library(dplyr)
library(readr)

#Reading IDs of all data sets which have missing values
df_ids <- read.csv("./datasets_store/datasets_selection/all_datasets_with_missings_id.csv")
colnames(df_ids) <- c("No", "ID")

#Data sets with error
err <- c(41147)
df_ids <- df_ids[!df_ids$ID %in% err,]

#Collecting basic information
n <- nrow(df_ids)
ID <- df_ids$ID
name <- rep(NA, n)
version <- rep(NA, n)
task_type <- rep(NA, n)

datasets <- data.frame(ID, name, version, task_type)

i <- 1
for(id in df_ids$ID){
  
  try({
    
    df_oml <- getOMLDataSet(id)
    df <- df_oml$data
    
    target <- df[,df_oml$target.features]
    
    if(is.numeric(target)){
      task <- "regression"
    }else if(is.factor(target)){
      
      if(length(unique(target))==2){
        task <- "binary"
      }else{
        task <- "multi-class"
      }
      
    }else{
      task <- "other"
    }
    
    vec <- c(id, df_oml$desc$name, df_oml$desc$version, task)
    datasets[datasets$ID==id, ] <- vec
    
  })
  
  #Cleaning cache
  i <- i+1
  if(i==20){
    clearOMLCache()
    i <- 1
  }
}

#Selecting data sets
selected_datasets <- datasets[datasets$task_type %in% c("regression", "binary"),]

#Checking duplicates from versioning
duplicate <- group_by(selected_datasets, name, task_type)%>%count()%>%filter(n>1)
duplicate_id <- selected_datasets$ID[selected_datasets$name %in% duplicate$name]

#Leaving only the oldest version of data set (versions follow the order of uploads according to OML)
selected_versions <- group_by(selected_datasets[selected_datasets$name %in% duplicate$name,], name, task_type)%>%
  slice(which.min(version))

#Update selected data sets
to_remove <- duplicate_id[!duplicate_id %in% selected_versions$ID]
selected_datasets <- selected_datasets[!selected_datasets$ID %in% to_remove, ]
#Finally 155 data sets left

#write_csv(selected_datasets, "./datasets_store/datasets_selection/selected_datasets.csv")
