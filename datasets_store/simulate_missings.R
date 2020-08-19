library(missMethods)
library(dplyr)

simulate_missings <- function(df, 
                              per_missings,
                              per_instances_missings = NULL,
                              per_variables_missings = NULL, 
                              variables_with_missings = NULL){
  
  #Function to generate missing observation in datasets according to 
  #schemes from decisions trees obtained in the exploratory analysis
  
  if(missing(per_missings)){
    warning("Please define percentage of missings")
    return(NULL)
  }
  
  if(!is.null(per_variables_missings) & !is.null(variables_with_missings)){
    warning("You can only set one of the parameters at a time: \n 
            per_variables_missings or variables_with_missings.")
    return(NULL)
  }
  
  per_missings <- per_missings/100
  
    #Usage 1: only "per_missings" set
    if(is.null(per_instances_missings) & is.null(per_variables_missings) & is.null(variables_with_missings)){
      
      df <- delete_MCAR(df, p = per_missings, p_overall = TRUE)
    
    #Usage 2: only "per_missings" and "per_instances_missings" set  
    }else if(!is.null(per_instances_missings) & is.null(per_variables_missings & is.null(variables_with_missings))){
      
      per_instances_missings <- per_instances_missings/100
      
      no_rows_missing <- floor(per_instances_missings*nrow(df))
      no_missings <- floor(per_missings*nrow(df)*ncol(df))
      no_missings_by_row <- ceiling(no_missings/no_rows_missing)
      
      if((no_missings>(no_rows_missing*ncol(df))) | (no_missings<no_rows_missing)){
        
        warning("Incorrect dimensions of missings")
      
      }else{
      
        cols_sample <- replicate(n = no_rows_missing, sample(size = no_missings_by_row, c(1:ncol(df))))
        cols_sample <- as.vector(cols_sample)
        rows_sample <- sample(size = no_rows_missing, nrow(df))
        rows_sample <- rep(rows_sample, each = no_missings_by_row)
        
        index <- matrix(ncol = 2, c(rows_sample, cols_sample))
        
        over <- length(rows_sample)-no_missings
        
        if(over>0){
          
          to_remove <- sample(size = over, no_rows_missing)
          to_remove <- (to_remove-1)*no_missings_by_row+1
          index <- index[-to_remove, ]  
          
        }
        
        df[index] <- NA
        
      }
    #Usage 3: "per_missings", and "per_variables_missings" or "variables_with_missings" set  
    }else if(is.null(per_instances_missings) & !is.null(per_variables_missings)){
      per_variables_missings <- per_variables_missings/100
      
      if(is.null(variables_with_missings)){
        
        no_cols_missings <- ceiling(per_variables_missings*ncol(df))
        cols_missings <- sample(size = no_cols_missings, c(1:ncol(df)))
        
      }else{
        
        no_cols_missings <- length(variables_with_missings)
        cols_missings <- variables_with_missings
        
      }
      
      no_missings <- floor(per_missings*nrow(df)*ncol(df))
      
      if((no_missings>(no_cols_missings*nrow(df))) | (no_missings<no_cols_missings)){
        
        warning("Incorrect dimensions of missings")
        
      }else{
        
        percentage <- no_missings/(no_cols_missings*nrow(df))
        temp <- as.data.frame(df[, cols_missings])
        temp <- delete_MCAR(temp, p = percentage, p_overall = TRUE)
        df[, cols_missings] <- temp
        
      }
    #Usage 4: all parameters set  
    }else{
      
      if(is.null(variables_with_missings)){
        
        per_variables_missings <- per_variables_missings/100
        no_cols_missings <- ceiling(per_variables_missings*ncol(df))
        cols_missings <- sample(size = no_cols_missings, c(1:ncol(df)))
        
      }else{
        
        no_cols_missings <- length(variables_with_missings)
        cols_missings <- variables_with_missings
        
      }
      
      per_instances_missings <- per_instances_missings/100
      
      no_missings <- floor(per_missings*nrow(df)*ncol(df))
      no_rows_missing <- floor(per_instances_missings*nrow(df))
    
      if((no_missings>(no_rows_missing*no_cols_missings)) | (no_missings<max(no_cols_missings, no_rows_missing))){
        
        warning("Incorrect dimensions of missings")
        
      }else{
        
        rows_sample <- sample(size = no_rows_missing, nrow(df))
        
        if(no_rows_missing>no_cols_missings){
          
          length_diff <- no_rows_missing-no_cols_missings
          cols_sample <- c(sample(cols_missings), sample(size = length_diff, cols_missings, replace = TRUE))
          index <- matrix(ncol = 2, c(rows_sample, cols_sample))
          
          all_index <- matrix(ncol = 2, c(rep(rows_sample, each = no_cols_missings), rep(cols_missings, times = no_rows_missing)))
          
          index_diff <- anti_join(as.data.frame(all_index), as.data.frame(index))
          over_sample <- index_diff[sample(size = (no_missings-no_rows_missing), nrow(index_diff)), ]
          index <- rbind(index, as.matrix(over_sample))
          
          df[index] <- NA
          
        }else{
          
          length_diff <- no_cols_missings-no_rows_missing
          rows <- c(rows_sample, sample(size = length_diff, rows_sample, replace = TRUE))
          index <- matrix(ncol = 2, c(rows, cols_missings))
          
          all_index <- matrix(ncol = 2, c(rep(rows_sample, each = no_cols_missings), rep(cols_missings, times = no_rows_missing)))
          
          index_diff <- anti_join(as.data.frame(all_index), as.data.frame(index), )
          over_sample <- index_diff[sample(size = (no_missings-no_cols_missings), nrow(index_diff)), ]
          index <- rbind(index, as.matrix(over_sample))
          
          df[index] <- NA
          
        }
      }
    }
  return(df)
}

data_test <- data.frame("x" = seq(1, 100), "y" = seq(101, 200), "z" = seq(101, 200), "w" = seq(101, 200))
df <- data_test
df <- simulate_missings(data_test, per_missings = 1, per_variables_missings = 100, per_instances_missings = 2)

#Missing values
shadow_data <- is.na(df)
number_of_missings_in_features <- colSums(shadow_data)
number_of_missings_in_instances <- rowSums(shadow_data)
#'no_of_missings'
sum(shadow_data)/(nrow(df)*ncol(df))
#'no_of_instances_with_missings'
sum(number_of_missings_in_instances>0)/nrow(df)
#'no_of_features_with_missings'
sum(number_of_missings_in_features>0)/ncol(df)
