#MCAR test

source("datasets_store/preprocess.R")
#library(devtools)
#install_url("https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/BaylorEdPsych_0.5.tar.gz")
#install_url("https://cran.r-project.org/src/contrib/Archive/mvnmle/mvnmle_0.1-11.1.tar.gz")

library(mvnmle)
library(BaylorEdPsych)
library(OpenML)

info_df <- read.csv(file = "datasets_store/patterns_analysis/readed_json_info.csv")
info_df <- info_df[info_df$task == 'binary',]

sample_id <- read.csv("datasets_store/datasets_test_sample.csv")$id
sample <- info_df[info_df$id %in% sample_id, -1]


result <- data.frame("id" = sample_id, "p_val" = rep(NA, length(sample_id)))

for (id in sample_id) {
  df_oml <- getOMLDataSet(id)
  
  if(id==940 | id==565){
    #Spelling mistake in OML description
    df_oml$desc$ignore.attribute <- c('date', 'Q.E') 
  }
  if(id==41704){
    #Spelling mistake in OML description
    df_oml$desc$ignore.attribute <- "instance_id" 
  }
  
  df <- preprocess(df_oml)$df
  try({
  p_val <- LittleMCAR(df)$p.value
  result[result$id == id, ] <- c(id, p_val)
  })
}

#write.csv(result, file = "datasets_store/patterns_analysis/mcar_test_in_sample.csv", row.names = FALSE)
