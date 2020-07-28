#' Creating formula
#'
#' @description This function create formula to use in with function form mice package.
#' Also inform if its necessary to use gml instead of lm (no numeric values in dataset).
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param coll_miss character vector. Names of columns with NA.
#' @param coll_no_miss character vector. Names of columns without NA.
#' @param coll_type character vector. Vector containing column type names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @usage formula_creating(df,coll_miss,coll_no_miss,coll_type,percent_of_missing)
#' @return List with formula object[1] and information if its no numeric value in dataset[2].


formula_creating <- function(df,coll_miss,coll_no_miss,coll_type,percent_of_missing){


  # Flags if no numeric value in df
  no_numeric <-  T

  #If df contains numeric values
  if  ('numeric' %in% coll_type | 'intiger' %in% coll_type){
    no_numeric <- F
    numeric_columns <- colnames(df)[ifelse('numeric' == coll_type | 'intiger' == coll_type,T,F)]

    #If some numeric columns don't contain missing data
    numeric_no_missing <- intersect(numeric_columns,colnames(df)[percent_of_missing==0])
    if (length(numeric_no_missing)>0){
      predicted_value <- numeric_no_missing[1]
      if (sum(percent_of_missing>0)>=3){
        columns_missing  <-  as.data.frame(cbind(percent_of_missing,colnames(df)))
        columns_missing <- columns_missing[order(as.numeric(as.character(columns_missing$percent_of_missing)),decreasing = TRUE),]
        predicting_values <- columns_missing$V2[1:3]
      }
      else{ predicting_values <- coll_miss}

    }

    else{
      columns_missing_type <- as.data.frame(cbind(percent_of_missing,colnames(df),coll_type))
      columns_missing_type_n_i <- columns_missing_type[columns_missing_type$coll_type=='numeric' | columns_missing_type$coll_type == 'initger',]
      if (length(row.names(columns_missing_type_n_i))>=1) {
        predicted_value <- columns_missing_type_n_i[order(columns_missing$percent_of_missing),][1]}
      else {no_numeric <-  T }
      if (length(row.names(columns_missing_type[-1,]))>=3){
        predicting_values <-  columns_missing_type[order(as.numeric(as.character(columns_missing_type$percent_of_missing)),decreasing = T),V2][1:3]
      }
      else{predicting_value <-setdiff(predicted_value,coll_miss)}
    }


  }
  # If df don't contains numeric values
  if (no_numeric){
    predicted_value <- coll_no_miss[1]
    if (sum(percent_of_missing>0)>=3){
      columns_missing  <-  as.data.frame(cbind(percent_of_missing,colnames(df)))
      columns_missing <- columns_missing[order(as.numeric(as.character(columns_missing$percent_of_missing)),decreasing = TRUE),]
      predicting_values <- columns_missing$V2[1:3]
    }
    else{ predicting_values <- coll_miss}
  }



  return(list(as.formula(paste(as.character(predicted_value),paste(as.character(predicting_values),collapse = '+'),sep='~')),no_numeric))

}
