library(dplyr)
library(data.tree)
library(colourvalues)

#Input data, same as in combined_report
# data <- read.csv("datasets_store/patterns_analysis/readed_json_info.csv")[, -1]
# selected_df <- read.csv("datasets_store/datasets_selection/selected_datasets.csv")
# data <- data[data$id %in% selected_df$ID, ]
# data <- data[!is.na(data$name), ]
# patterns <- read.csv(file = "datasets_store/patterns_analysis/summary_of_patterns_in_all.csv")
# patterns <- patterns[patterns$id %in% data$id, ]


#Data frame for decision tree
df <- data
# % of missings
df$per_missings <- round(100*df$no_missings/(df$no_instances*df$no_features), digits = 1)
# % of instances with missings
df$per_instances_missings <- round(100*df$no_instances_w_miss/df$no_instances, digits = 1)
# % of unique patterns in number of instances 
df$patterns_fraction <- round(100*df$no_of_patterns/df$no_instances, digits = 1)
# average % of variables in patterns by dataset
avg_per_var_pattern <- group_by(patterns, id)%>%
  summarise("avg_per_var_pattern" = mean(var_per))
df <- merge(df, avg_per_var_pattern, by = "id")
# average % of instances in patterns by dataset
avg_per_inst_pattern <- group_by(patterns, id)%>%
  summarise("avg_per_inst_pattern" = mean(row_per))
df <- merge(df, avg_per_inst_pattern, by = "id")

df <- df[, c("id", "per_missings", "per_instances_missings", "patterns_fraction", "avg_per_var_pattern", "avg_per_inst_pattern")]


decision_tree <- function(df, decisions){
  #Function returning data frame with leafs ids and tree object for plotting
  #-df: data frame created above, contains columns with key statistics used in reports 
  #-decisions: list with thresholds for each variable, 
  # order of variables in list determines order of tree splits,
  # multi-valued thresholds are possible when passed in vector
  # names of list elements must be the same as columns in df
  
  decision_variables <- names(decisions)
  
  df <- df[, c("id", decision_variables)]
  
  for (var in decision_variables) {
    
    breaks <- c(-Inf, decisions[[var]], 100)
    labels <- c(paste("<", decisions[[var]], "%", sep = ""), paste(">", max(decisions[[var]]), "%", sep = ""))
    
    df[, var] <- cut(df[, var], breaks = breaks, labels = labels)
  }
  
  df <- group_by_at(df, decision_variables)%>%
    mutate("group_id" = cur_group_id())
  df <- group_by(df, group_id)%>%
    mutate("group_size" = n())
  
  #Building tree object
  sum_up <- group_by_at(df, decision_variables)%>%
    summarise("leaf_size" = n())

  sum_up[, -ncol(sum_up)] <- Map(paste, names(sum_up)[-ncol(sum_up)], " \n ", sum_up[, -ncol(sum_up)], sep = "")
  
  tree <- sum_up
  tree$pathString <- apply(tree[, c(decision_variables, "leaf_size")], 1, paste, collapse = "/")
  tree$pathString <- paste("datasets", tree$pathString, sep="/")
  tree <- as.Node(tree)
  
  cols <- colourvalues::color_values(sum_up$leaf_size, alpha = 175)
  mapply(function(node, col) {SetNodeStyle(node, fillcolor = col)}, node = tree$leaves, col = cols)
  SetNodeStyle(tree, style = "filled, rounded", shape = "box", fillcolor = "lightcyan", fontcolor = "black")
  
  return(list("df" = df, "tree" = tree))
}



#Example of use
#Decision variables to choose:
# - "per_missings", "per_instances_missings", "patterns_fraction", "avg_per_var_pattern", "avg_per_inst_pattern"

# decisions <- list(
#   "per_missings" = 10,
#   "per_instances_missings" = c(15, 90),
#   "patterns_fraction" = 10
# )
# 
# result <- decision_tree(df, decisions)
# result_df <- result$df
# #Plotting tree
# tree <- result$tree
# plot(tree)

