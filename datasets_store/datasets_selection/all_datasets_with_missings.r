#Searching for all data sets from OpenML which have missing values

library(OpenML)

all_datasets <- listOMLDataSets()

#Leaving data sets with missings or without information about missings
datasets <- all_datasets[all_datasets$number.of.missing.values>0 | is.na(all_datasets$number.of.missing.values), ]

#Check data sets without information about missings
df_to_check <- datasets$data.id[is.na(datasets$number.of.missing.values)]
checked_with_missings <- rep(F, length(df_to_check))

i <- 0
for(id in df_to_check){
  i <- i+1
  
  try({
    
    df <- getOMLDataSet(id)
    #Check number of missings
    missings_number <- sum(is.na(df$data))
    
    if(missings_number>0){
      checked_with_missings[i] <- T
    }
  })
}

#Checked data sets with missings
df_checked <- df_to_check[checked_with_missings==T]

#Save IDs of all data sets with missings
datasets_miss_id <- c(datasets$data.id[!is.na(datasets$number.of.missing.values)], df_checked)

#write.csv(datasets_miss_id, "./datasets_store/datasets_selection/all_datasets_with_missings_id.csv")

