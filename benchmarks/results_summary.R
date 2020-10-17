library(dplyr)

sum_up_results <- function(input_file, output_dir, result_id){
  
  path <- input_file
  p1 <- read.csv(path, row.names = NULL)
  p1 <- p1[, c("iteration", "classif.acc", "classif.auc", "classif.fbeta", "task" ,"imputer", "model")]
  df_grep <- p1
  
  #Add approach info col
  if(any(grepl("_A", df_grep$imputer))){
    df_grep <- mutate(p1, "approach" = ifelse(grepl("_A", imputer), "A", "B"))
  }else{
    df_grep <- mutate(p1, "approach" = ifelse(grepl("_B", imputer), "B", "A"))
  }
  df_grep$imputer <- gsub("_A", "", df_grep$imputer)
  df_grep$imputer <- gsub("_B", "", df_grep$imputer)
  
  ###Aggregate to CV mean
  agr <- group_by(df_grep, imputer, model, approach) %>%
    summarise("mean_acc" = mean(classif.acc, na.rm = TRUE), 
              "mean_auc" = mean(classif.auc, na.rm = TRUE), 
              "mean_f1" = mean(classif.fbeta, na.rm = TRUE))
  
  write.csv(agr, file = paste(output_dir, "avg_", result_id, ".csv", sep = ""), row.names = FALSE)
  
  ###Mean on each task
  agr <- group_by(df_grep, imputer, approach, task)%>%
    summarise("mean_acc" = mean(classif.acc, na.rm = TRUE), 
              "mean_auc" = mean(classif.auc, na.rm = TRUE), 
              "mean_f1" = mean(classif.fbeta, na.rm = TRUE))
  
  write.csv(agr, file = paste(output_dir, "avg_on_tasks_", result_id, ".csv", sep = ""), row.names = FALSE)
  
  agr <- group_by(df_grep, imputer, model, task, approach)%>%
    summarise("avg" = mean(classif.acc))%>%
    filter(model=="classif.glmnet")%>%
    group_by(imputer, approach)%>%
    summarise("positive" = n())%>%
    arrange(imputer)
  
  write.csv(agr, file = paste(output_dir, "successful_imp_", result_id, ".csv", sep = ""), row.names = FALSE)
  
}

# path <- "benchmarks/raw_results/result_simple.csv"
# p1 <- read.csv(path, row.names = NULL)
# p1 <- p1[, c("iteration", "classif.acc", "classif.auc", "classif.fbeta", "task" ,"imputer", "model")]


#p2 <- p2[, c("iteration", "classif.acc","task" ,"imputer")]

#Write basic CSV
#write.csv(p1, "benchmarks/trial_4/simple.csv", row.names = FALSE)


# ###Aggregate to CV mean
# agr <- group_by(p1, imputer, model) %>%
#   summarise("mean_acc" = mean(classif.acc))
# fin <- reshape2::dcast(data = agr, imputer ~ model)
# 
# ###Mean on each task
# agr <- group_by(p1, task, imputer)%>%
#   summarise("mean_acc" = mean(classif.acc))
# fin <- reshape2::dcast(data = agr, imputer ~ task)
# 
# ###Aggregate to number of successful imputations
# agr <- group_by(p1, imputer, model, task)%>%
#   summarise("avg" = mean(classif.acc))%>%
#   summarise("positive" = n())
# fin <- reshape2::dcast(data = agr, imputer ~ model)
# 
# #Add approach info col
# df_grep <- mutate(p1, "approach" = ifelse(grepl("_B", imputer), "B", "A"))
# #df_grep <- mutate(p1, "approach" = ifelse(grepl("_A", imputer), "A", "B"))
# df_grep$imputer <- gsub("_A", "", df_grep$imputer)
# df_grep$imputer <- gsub("_B", "", df_grep$imputer)
# 
# ###Aggregate to CV mean
# agr <- group_by(df_grep, imputer, model, approach) %>%
#   summarise("mean_acc" = mean(classif.acc, na.rm = TRUE), 
#             "mean_auc" = mean(classif.auc, na.rm = TRUE), 
#             "mean_f1" = mean(classif.fbeta, na.rm = TRUE))
# #fin <- reshape2::dcast(data = agr, imputer ~ model)
# 
# ###Mean on each task
# agr <- group_by(df_grep, imputer, approach, task)%>%
#   summarise("mean_acc" = mean(classif.acc, na.rm = TRUE), 
#             "mean_auc" = mean(classif.auc, na.rm = TRUE), 
#             "mean_f1" = mean(classif.fbeta, na.rm = TRUE))
# 
# ###Aggregate to number of successful imputations
# agr <- group_by(df_grep, imputer, model, task, approach)%>%
#   summarise("avg" = mean(classif.acc))%>%
#   group_by(imputer, model, approach)%>%
#   summarise("positive" = n())%>%
#   arrange(imputer)
# 
# temp <- group_by(agr, imputer, model)%>%
#   summarise("total_splits" = 5*max(positive))
# 
# ###A/B splits won
# agr <- group_by(df_grep, task, imputer, model, iteration)%>%
#   mutate("best_score" = approach[which.max(classif.acc)])%>%
#   group_by(imputer, task, model)%>%
#   summarise("better_A" = 5*sum(best_score=="A")/n())%>%
#   group_by(imputer, model)%>%
#   summarise("better_A_splits" = sum(better_A))
# 
# agr <- inner_join(agr, temp, by = c("imputer", "model"))
#   
