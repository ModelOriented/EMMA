library(data.table)

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
    warning("You can only set one of the following parameters: \n 
            per_variables_missings or variables_with_missings.")
    return(NULL)
  }
  
  per_missings <- per_missings/100
  no_missings <- floor(per_missings*nrow(df)*ncol(df))
    
    if(is.null(per_instances_missings) & (is.null(per_variables_missings) & is.null(variables_with_missings))){
      #Usage 1: only "per_missings" set
      
      index <- as.matrix(expand.grid(1:nrow(df), 1:ncol(df)))
      index <- index[sample(size = no_missings, nrow(index)), ]
      
      df[index] <- NA
    
    }else if(!is.null(per_instances_missings) & is.null(per_variables_missings) & is.null(variables_with_missings)){
      #Usage 2: only "per_missings" and "per_instances_missings" set
      #Implementation:
      # 1. To ensure missings in all sampled instances, calculating ceiling of number of missing values per row
      # 2. Sample indexes of missings, because of usage of ceiling generating more missings than declared
      # 3. Remove randomly redundant missings from some rows 
      
      per_instances_missings <- per_instances_missings/100
      
      no_rows_missing <- floor(per_instances_missings*nrow(df))
      no_missings_by_row <- ceiling(no_missings/no_rows_missing)
      
      if((no_missings>(no_rows_missing*ncol(df))) | (no_missings<no_rows_missing)){
        
        warning("Incorrect dimensions of missings")
      
      }else{
        
        #Generating column indexes of missings for each row
        cols_sample <- replicate(n = no_rows_missing, sample(size = no_missings_by_row, ncol(df)))
        cols_sample <- as.vector(cols_sample)
        #Generate row indexes of missings
        rows_sample <- sample(size = no_rows_missing, nrow(df))
        rows_sample <- rep(rows_sample, each = no_missings_by_row)
        
        index <- matrix(ncol = 2, c(rows_sample, cols_sample))
        
        #Check number of redundant missings 
        over <- length(rows_sample)-no_missings
        if(over>0){
          to_remove <- sample(size = over, no_rows_missing)
          to_remove <- (to_remove-1)*no_missings_by_row+1
          index <- index[-to_remove, ]  
        }
        
        df[index] <- NA
        
      }
    }else if(is.null(per_instances_missings)){
      #Usage 3: "per_missings", and "per_variables_missings" or "variables_with_missings" set  
      #Implementation similar to usage 2
      
      per_variables_missings <- per_variables_missings/100
      
      if(is.null(variables_with_missings)){
        no_cols_missings <- ceiling(per_variables_missings*ncol(df))
        cols_missings <- sample(size = no_cols_missings, ncol(df))
      }else{
        no_cols_missings <- length(variables_with_missings)
        cols_missings <- variables_with_missings
      }
      
      if((no_missings>(no_cols_missings*nrow(df))) | (no_missings<no_cols_missings)){
        
        warning("Incorrect dimensions of missings")
        
      }else{
        
        no_missings_by_col <- ceiling(no_missings/no_cols_missings)
        
        #Generating rows indexes of missings for each column
        rows_sample <- replicate(n = no_cols_missings, sample(size = no_missings_by_col, nrow(df)))
        rows_sample <- as.vector(rows_sample)
        #Generate column indexes of missings
        cols_sample <- rep(cols_missings, each = no_missings_by_col)
        
        index <- matrix(ncol = 2, c(rows_sample, cols_sample))
        
        #Check number of redundant missings 
        over <- length(cols_sample)-no_missings
        if(over>0){
          to_remove <- sample(size = over, no_cols_missings)
          to_remove <- (to_remove-1)*no_missings_by_col+1
          index <- index[-to_remove, ]  
        }
        
        df[index] <- NA
        
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
          
          index_diff <- fsetdiff(as.data.table(all_index), as.data.table(index))
          over_sample <- index_diff[sample(size = (no_missings-no_rows_missing), nrow(index_diff)), ]
          index <- rbind(index, as.matrix(over_sample))
          
          df[index] <- NA
          
        }else{
          
          length_diff <- no_cols_missings-no_rows_missing
          rows <- c(rows_sample, sample(size = length_diff, rows_sample, replace = TRUE))
          index <- matrix(ncol = 2, c(rows, cols_missings))
          
          all_index <- matrix(ncol = 2, c(rep(rows_sample, each = no_cols_missings), rep(cols_missings, times = no_rows_missing)))
          
          index_diff <- fsetdiff(as.data.table(all_index), as.data.table(index))
          over_sample <- index_diff[sample(size = (no_missings-no_cols_missings), nrow(index_diff)), ]
          index <- rbind(index, as.matrix(over_sample))
          
          df[index] <- NA
          
        }
      }
    }
  return(df)
}

#Tests
#' library(OpenML)
#' datasets <- listOMLDataSets()
#' datasets <- datasets%>%
#'   filter(number.of.instances > 100, number.of.missing.values == 0, number.of.instances<1000, data.id<1000)
#' datasets <- datasets[sample(size = 10, nrow(datasets)), "data.id"]
#' 
#' param1 <- list("per_missings" = 1,
#'                "per_instances_missings" = NULL,
#'                "per_variables_missings" = NULL, 
#'                "variables_with_missings" = NULL)
#' param2 <- list("per_missings" = 25,
#'                "per_instances_missings" = NULL,
#'                "per_variables_missings" = NULL, 
#'                "variables_with_missings" = NULL)
#' param3 <- list("per_missings" = 5,
#'                "per_instances_missings" = 40,
#'                "per_variables_missings" = NULL, 
#'                "variables_with_missings" = NULL)
#' param4 <- list("per_missings" = 10,
#'                "per_instances_missings" = 25,
#'                "per_variables_missings" = NULL, 
#'                "variables_with_missings" = NULL)
#' param5 <- list("per_missings" = 1,
#'                "per_instances_missings" = 5,
#'                "per_variables_missings" = 80, 
#'                "variables_with_missings" = NULL)
#' param6 <- list("per_missings" = 5,
#'                "per_instances_missings" = 10,
#'                "per_variables_missings" = 50, 
#'                "variables_with_missings" = NULL)
#' param7 <- list("per_missings" = 5,
#'                "per_instances_missings" = 20,
#'                "per_variables_missings" = NULL, 
#'                "variables_with_missings" = c(1,2,3))
#' 
#' r <- c()
#' par <- param7
#' for (id in datasets) {
#'   
#'   try({
#'   
#'   df_dow <- getOMLDataSet(id)$data
#'   df <- simulate_missings(df = df_dow, 
#'                           per_missings = par$per_missings,
#'                           per_instances_missings = par$per_instances_missings,
#'                           per_variables_missings = par$per_variables_missings,
#'                           variables_with_missings = par$variables_with_missings)
#'   
#'   #Missing values
#'   shadow_data <- is.na(df)
#'   number_of_missings_in_features <- colSums(shadow_data)
#'   number_of_missings_in_instances <- rowSums(shadow_data)
#'   #'no_of_missings'
#'   v1 <- sum(shadow_data)/(nrow(df)*ncol(df))
#'   #'no_of_instances_with_missings'
#'   v2 <- sum(number_of_missings_in_instances>0)/nrow(df)
#'   #'no_of_features_with_missings'
#'   v3 <- sum(number_of_missings_in_features>0)/ncol(df)
#'   
#'   vec <- c(v1, v2, v3)
#'   r <- cbind(r, vec)
#'   
#'   })
#' }


#' data_test <- data.frame("x" = seq(1, 100), "y" = seq(101, 200), "z" = seq(101, 200), "w" = seq(101, 200))
# df <- data_test
# df <- simulate_missings(df_dow, per_missings = 1, per_variables_missings = NULL, per_instances_missings = NULL)
#' 
#' #Missing values
#' shadow_data <- is.na(df)
#' number_of_missings_in_features <- colSums(shadow_data)
#' number_of_missings_in_instances <- rowSums(shadow_data)
#' #'no_of_missings'
#' sum(shadow_data)/(nrow(df)*ncol(df))
#' #'no_of_instances_with_missings'
#' sum(number_of_missings_in_instances>0)/nrow(df)
#' #'no_of_features_with_missings'
#' sum(number_of_missings_in_features>0)/ncol(df)

