library(jsonlite)

CreateSummary <- function(data, target_column, id, data_name, source = 'openml', added_by = NA){
  # data - data frame
  # target_column - name of column with target class
  # id - dataset id in OpenML 
  # data_name - dataset name in OpenML
  # source - by default "openml"
  # added_by - GitHub username
  
  shadow_data <- is.na(data)
  
  type_of_features <- sapply(data, class)
  number_of_missings_in_features <- colSums(shadow_data)
  number_of_missings_in_instances <- rowSums(shadow_data)
  
  if(any(!type_of_features %in% c('numeric', 'integer', 'factor'))){
    warning('Uncompatibile type of columns')
  }
  
  summary_final <- list('id' = id,
                        'added_by' = added_by,
                        'data_name' = data_name,
                        'source' = source,
                        'target' = target_column,
                        'number_of_instances' = nrow(data),
                        'number_of_features' = ncol(data),
                        'number_of_numeric_features' = sum(type_of_features %in% c('numeric', 'integer')),
                        'number_of_categorical_features' = sum(type_of_features  == 'factor'),
                        'number_of_missings' = sum(shadow_data),
                        'number_of_instances_with_missings' = sum(number_of_missings_in_instances > 0),
                        'number_of_features_with_missings' = sum(number_of_missings_in_features > 0),
                        'number_of_numeric_features_with_missings' = sum(type_of_features %in% c('numeric', 'integer') & number_of_missings_in_features > 0),
                        'numeric_features_with_missings' = names(type_of_features)[(type_of_features %in% c('numeric', 'integer') & number_of_missings_in_features > 0)],
                        'number_of_categorical_features_with_missings' = sum(type_of_features %in% c('factor') & number_of_missings_in_features > 0),
                        'categorical_features_with_missings' = names(type_of_features)[(type_of_features %in% c('factor') & number_of_missings_in_features > 0)])
  
  summary_final_json <- jsonlite::toJSON(summary_final, pretty = TRUE, auto_unbox = TRUE, .na = "null")
  
  return(summary_final_json)
  
}


