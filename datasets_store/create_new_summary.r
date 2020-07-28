library(jsonlite)
library(dplyr)
library(mice)

create_summary <- function(df_oml, df_desc, df){
  
  #Types of features
  categorical <- colnames(df)[sapply(df, is.factor)]
  numeric <- colnames(df)[!sapply(df, is.factor)]
  
  #Missing values
  shadow_data <- is.na(df)
  number_of_missings_in_features <- colSums(shadow_data)
  number_of_missings_in_instances <- rowSums(shadow_data)
  
  #Columns with missings
  missings_df <- data.frame("column" = colnames(df), 
                            "no_of_missings" = number_of_missings_in_features,
                            "percentage_of_missings" = round(100*number_of_missings_in_features/nrow(df), 1))%>%
    filter(no_of_missings>0)
  
  categorical_missings <- missings_df[missings_df$column %in% categorical, ]
  numeric_missings <- missings_df[missings_df$column %in% numeric, ]
  
  #Missings pattern
  mice_pattern_matrix <- md.pattern(df, plot=F)
  missings_pattern <- data.frame(pattern_count = as.integer(rownames(mice_pattern_matrix)), mice_pattern_matrix)
  colnames(missings_pattern)[ncol(missings_pattern)] <- "missings_count"
  
  summary <- list(#Basic information
                  'id' = df_desc$id,
                  'name' = df_desc$name,
                  'version' = df_desc$version,
                  'target' = df_oml$target.features,
                  'no_of_instances' = nrow(df),
                  'no_of_features' = ncol(df),
                  'categorical_features' = categorical,
                  'numeric_features' = numeric,
                  #Missing values information
                  'categorical_missings' = categorical_missings,
                  'numeric_missings' = numeric_missings,
                  'no_of_missings' = sum(shadow_data),
                  'no_of_instances_with_missings' = sum(number_of_missings_in_instances>0),
                  'no_of_features_with_missings' = nrow(missings_df),
                  'no_of_categorical_with_missings' = nrow(categorical_missings),
                  'no_of_numeric_with_missings' = nrow(numeric_missings),
                  'missings_pattern' = missings_pattern
                  )

  summary_json <- toJSON(summary, pretty = F, auto_unbox = T)
  return(summary_json)
}