library(jsonlite)

#selected <- read.csv(file = "datasets_store/datasets_selection/selected_datasets.csv")
#selected <- selected[selected$task_type == "binary", "ID"]
selected <- read.csv("datasets_store/datasets_test_sample.csv")$id

summary <- data.frame("id" = c(), "var_per" = c(), "row_per" = c(), "miss_per" = c(), 
                      "num_per" = c(), "cat_per" = c(), "miss_inst_per" = c())


pattern_summary <- function(row){
  pat <- row[-c(1, length(row))]
  var_per <- round(100*sum(pat==0)/length(pat), digits = 2)
  row_per <- round(100*row[1]/no_instances, digits = 2)
  miss_per <- round(100*row[length(row)]/no_miss, digits = 2)
  miss_inst_per <- round(100*row[1]/no_miss_inst, digits = 2)
  
  var_types_in_pattern <- var_types[pat==0]
  num_per <- round(100*sum(var_types_in_pattern=="numeric")/length(var_types_in_pattern), digits = 1)
  cat_per <- round(100*sum(var_types_in_pattern=="factor")/length(var_types_in_pattern), digits = 1)
  
  vec <- c(id, var_per, row_per, miss_per, num_per, cat_per, miss_inst_per)
  
  return(vec)
}

for (id in selected) {
  pattern <- list.files(path = "datasets_store/patterns_base", pattern = paste("_", id, ".csv", sep = ""), full.names = TRUE)
  json <- list.files(path = "datasets_store/information_base", pattern = paste("_", id, ".json", sep = ""), full.names = TRUE)
  
  frame <- read.csv(pattern)[,-1]
  frame <- frame[-nrow(frame),]
  json <- fromJSON(json)
  
  var_types <- json$variables_types
  no_instances <- json$no_of_instances
  no_miss <- json$no_of_missings
  no_miss_inst <- json$no_of_instances_with_missings
  
  to_pattern_analysis <- frame[frame$missings_count>0, ]
  
  single_summary <- as.data.frame(t(apply(to_pattern_analysis, 1, pattern_summary)))
  colnames(single_summary) <- c("id", "var_per", "row_per", "miss_per", "num_per", "cat_per", "miss_inst_per")
  summary <- rbind(summary, single_summary)
}

#write.csv(summary, file = "datasets_store/patterns_analysis/summary_of_patterns_in_sample.csv", row.names = FALSE)
