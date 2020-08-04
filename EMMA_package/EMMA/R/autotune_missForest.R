#' Perform imputation using missForest form missForest package.
#'
#' @description Function use missForest package for data imputation. Function use OBBerror (more in missForest documentation) to perform grid search.
#' Best parameters to imputation are chosen form given sets. Imputation use parallel calculation by default and use existing parallel backend if it's possible.
#' If not function starts new parallel backend. !!!! Function doesn't turn off parallel backend by default after imputation. !!!!
#'
#'
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param cores integer.  Number of threads used by parallel calculations. By default approximately half of available CPU cores.
#' @param ntree_set integer vector. Vector contains numbers of tree for grid search.
#' @param mtry_set integer vector. Vector contains numbers of variables randomly sampled at each split.
#' @param parallel logical. If TRUE parallel calculation is using.
#' @param optimize optimize inside function
#' @param ntree ntree from missForest function
#' @param mtry mtry form missforest function
#' @param verbose If FALSE funtion didn't print on console.
#' @param maxiter maxiter form missForest function.
#' @param maxnodes maxnodes from missForest function.
#' @import missForest
#' @import doParallel
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @return Return data.frame with imputed values.
autotune_missForest <-function(df,percent_of_missing,cores=NULL,ntree_set =c(100,200,500,1000),mtry_set=NULL,parallel=TRUE,col_0_1=FALSE,
                               optimize=TRUE,ntree=100,mtry=NULL,verbose=FALSE,maxiter=20,maxnodes=NULL){

  # Checking if parallel backed is runing and starting it if not
  if (parallel){
    veribles = ncol(df)
    if (ceiling(detectCores()/2)>=veribles){cores <- (veribles-2)}
    registerDoParallel(cores = cores)
  }

  # Prepering mtry_set if not given
  if (is.null(mtry_set)){
    mtry_set <- 1:4
    mtry_set[1] <- floor(sqrt(ncol(df)))
    if (mtry_set[1]>1){
      mtry_set[2] <- ceiling(mtry_set[1]/2)
      vector <- (mtry_set[1]:ncol(df))
      mtry_set[3] <- floor(length(vector)/3)
      mtry_set[4] <- floor(2*length(vector)/3)
    }
    else{
      vector <- (mtry_set[1]:ncol(df))
      mtry_set[2] <- flor(length(vector)/4)
      mtry_set[3] <- floor(2*length(vector)/4)
      mtry_set[4] <- floor(3*length(vector)/4)
    }

  }
  if (optimize){
  # If parallel=TRUE
  parallelize <- 'no'
  if (parallel){
  parallelize <-  'variables'}


  # Grid search using mean OBBerror
  best_params <-  c(-11,-11)
  best_OBB <- 10
  for (i in ntree_set)
  {
    for (j in mtry_set){
      skip_to_next <- FALSE

      tryCatch({
        iteration <-  mean(missForest(df,maxiter = maxiter,ntree = i,mtry = j,parallelize=parallelize,maxnodes = maxnodes,verbose = verbose)$OOBerror)
        if (iteration<best_OBB){
          best_OBB <- iteration
          best_params[1] <- i
          best_params[2] <- j
        }



      }, error = function(e) { skip_to_next <<- TRUE})

      if(skip_to_next) { next }
    }
  }

  #fianl imputation
  final <- missForest(df,maxiter = maxiter,maxnodes = maxnodes,ntree = best_params[1],mtry = best_params[2],parallelize=parallelize,verbose = verbose)$ximp
  }
  if (!optimize){
    if (is.null(mtry)){
    final <- missForest(df,maxiter = maxiter,ntree = ntree,maxnodes = maxnodes,mtry = floor(sqrt(ncol(df))),parallelize = parallelize,verbose = verbose)$ximp}
    else{ final <- missForest(df,maxiter = maxiter,ntree = ntree,maxnodes = maxnodes,mtry = mtry,parallelize = parallelize,verbose = verbose)$ximp}
  }
  #adding 0_1_cols
  if (col_0_1){
    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)
  }

  # turn off paralllel
  if (parallel){
    registerDoSEQ()
  }
  return(final)
}





