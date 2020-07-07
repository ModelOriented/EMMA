library(mice)
library(missForest)
library(VIM)
library(missMDA)
library(softImpute)
library(Amelia)


### remove rows
impute_remove_rows <- function(data){
  return(data[rowSums(is.na(data))==0,])
}


### remove columns
impute_remove_columns <- function(data){
  data[,colSums(is.na(data))==0]
}

### random

random_replace_in_vector <- function(x){
  x[is.na(x)] <- sample(unique(na.omit(x)), sum(is.na(x)), replace = TRUE)
  return(x)
}

impute_random <- function(data){
  data.frame(lapply(data,  random_replace_in_vector))
}
  

### mean median

imputation_mode_median <- function(df){
  # browser()
  Mode <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  for (i in 1L:length(df)){
    if (sum(is.na(df[,i])) > 0){
      if (mode(df[,i]) == 'character' | is.factor(df[,i])){
        to_imp <- Mode(df[,i])
        df[,i][is.na(df[,i])] <- to_imp
      }
      else{
        to_imp <- median(df[,i], na.rm = TRUE) 
        df[,i][is.na(df[,i])] <- to_imp
      }
    }
  }
  
  return(df)
}


### VIM

imputation_fun_vim <- function(df){
  no_columns <- length(df)
  imputed <- kNN(df)
  imputed <- imputed[,1:no_columns]
  return(imputed)
}

### VIM hotdeck

imputation_fun_vim_hotdeck <- function(df){
  no_columns <- length(df)
  imputed <- hotdeck(df)
  imputed <- imputed[,1:no_columns]
  return(imputed)
}

## missForest

imputation_fun_missForest <- function(df){
  return(missForest(df)$ximp)
}


### softimpute
imputation_softimpute <- function(data){
  # browser()
  type_of_data <- sapply(data, class)
  factor_columns <- colnames(data)[type_of_data=='factor']
  cat_data <- data[,factor_columns]
  cat_data_imputed <- imputation_mode_median(cat_data)
  
  numeric_colnames <- setdiff(colnames(data), factor_columns)
  
  if(length(numeric_colnames)>0){
  numeric_data <- as.matrix(data[, numeric_colnames])
 
  imputer <- softImpute(numeric_data)
 

  
  numeric_data_imputed <- complete(numeric_data, imputer)
  
  
  all_data_imputed <- cbind(cat_data_imputed, numeric_data_imputed)[,colnames(data)]
  }
  else{
    all_data_imputed <- cat_data_imputed[,colnames(data)]
  }
  all_data_imputed
}



### mice 


imputation_fun_mice <- function(df){
  init <- mice(df, maxit=0, remove.collinear = FALSE, remove.constant = FALSE) 
  meth <- init$method
  predM <- init$predictorMatrix
  imputed <- mice(df, method=meth, predictorMatrix=predM, m=5, nnet.MaxNWts = 5000, remove.collinear = FALSE, remove.constant = FALSE)
  completed <- mice::complete(imputed)
  return(completed)
}


imputation_fun_mice_pmm <- function(df){
  init <- mice(df, maxit=0,  remove.collinear = FALSE, remove.constant = FALSE) 
  meth <- init$method
  predM <- init$predictorMatrix
  imputed <- mice(df, method=rep('pmm', length(meth)), predictorMatrix=predM, m=5, remove.collinear = FALSE, remove.constant = FALSE)
  completed <- mice::complete(imputed)
  return(completed)
}

imputation_fun_mice_cart <- function(df){
  init <- mice(df, maxit=0,  remove.collinear = FALSE, remove.constant = FALSE) 
  meth <- init$method
  predM <- init$predictorMatrix
  imputed <- mice(df, method=rep('cart', length(meth)), predictorMatrix=predM, m=5,remove.collinear = FALSE, remove.constant = FALSE)
  completed <- mice::complete(imputed)
  return(completed)
}


### missMDA
impute_missMDA <- function(data, ncp = NA){
  if(is.na(ncp)){

    ncp <- estim_ncpFAMD(data,
                         ncp.min=0, ncp.max = 3,
                         method.cv = 'Kfold',
                         nbsim=10)
  }


  imputeFAMD(data, ncp=ncp, 
             method="Regularized",
             maxiter = 10)$completeObs
}


### Amelia

imputation_fun_amelia <- function(data){
   # browser()
  no_unique <- sapply(data, function(x) length(unique(x)))
  constant_column <- ifelse(any(no_unique==1), which(no_unique==1), NA)
  
  type_of_data <- sapply(data, class)
  
  
  factor_columns <- setdiff(names(type_of_data[type_of_data=='factor']), names(type_of_data)[constant_column])
  
  if(!is.na(constant_column)){
    data_to_amelia <- data[,-constant_column]
  }else{
    data_to_amelia <- data
  }
  
  p <- amelia(data_to_amelia , noms = factor_columns, m=1)$imputations$imp1
  
  if(!is.na(constant_column)){
    cbind(p, data[,constant_column])[, colnames(data)]
  }else{
    p[, colnames(data)]
  }
  
 
}




### apply imputation function to list of dataset

get_imputed_data <- function(data,   imputed_function){
  
  #browser()
  train <- data$data_train[, -c(which(colnames(data$data_train) ==data$target_name))]
  test <- data$data_test[, -c(which(colnames(data$data_test) ==data$target_name))]
  
  expr_time <- system.time({
    imputed_data <- lapply(list(train, test), imputed_function)
    
  })
  names(imputed_data) <- c('data_train', 'data_test')
  imputed_data$data_train <- cbind(imputed_data$data_train, data$data_train[, data$target_name] )
  imputed_data$data_test <- cbind(imputed_data$data_test, data$data_test[, data$target_name] )
  
  colnames(imputed_data$data_train)[ncol(imputed_data$data_train)] <- data$target_name
  colnames(imputed_data$data_test)[ncol(imputed_data$data_test)] <- data$target_name
  
  return(list(imputed_data = imputed_data, 
              target_name = data$target_name,
              time = expr_time))
  
  
}


