#' Perform imputation using MFA algorithm.
#'
#' @description Function use MFA (Multiple Factor Analysis) to impute missing data. Groups are created using original column order and taking as much variable to one group as possible.
#' This method doesn't allow to Multiple Imputation. Function doesn't use any optimization teaching for ncp param.
#'
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param col_type character vector. Vector containing column type names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param random.seed random seed.
#' @param ncp Number of dimensions used by algorithm. Default 2.
#' @param coeff.ridge Value use in Regularized method.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @param maxiter maximal number of iteration in algortihm.
#' @param method method used in imputation algoritm.
#' @param threshold threshold for convergence.
#'
#' @import missMDA
#' @details MFA requires to select group type but numeric types can only be set as 'c' - centered and 's' - scale to unit variance.
#' It's impossible to provide these conditions so numeric type is always set as 's'.
#' @return Return one data.frame with imputed values.



missMDA_MFA <- function(df,col_type,percent_of_missing,random.seed=123,ncp =2 ,col_0_1=F,maxiter=1000,
                        coeff.ridge=1,threshold=1e-6,method='Regularized',out_file=NULL){

  if(!is.null(out_file)){
    write('MFA',file = out_file,append = T)
  }

  if (sum(is.na(df))==0){return(df)}

  #Creating gropus
  col_type_simpler <-  ifelse(col_type=='factor','n','s')


  groups <- c(99999999)
  type <- c('remove')
  counter <- 1
  for (i in 1:(length(col_type_simpler)-1)){
    flag <- col_type_simpler[i+1]


    if (flag ==col_type_simpler[i]){
      counter <- counter +1
      if (i +1 == length(col_type_simpler)){groups <- c(groups,counter)
      type <- c(type,col_type_simpler[i+1])}
    }
    if (flag !=col_type_simpler[i]){
      groups <- c(groups,counter)
      type <- c(type,col_type_simpler[i])
      counter <- 1
      if (i +1 == length(col_type_simpler)){groups <- c(groups,counter)
      type <- c(type,col_type_simpler[i+1])}
    }}
  groups <- groups[-1]
  type <- type[-1]
  if (length(groups )==1 ){

    groups <- c(floor(groups/2),groups-floor(groups/2))
    type <- rep(type[1],2)
  }
# Imputation
  no_ok <- FALSE

  tryCatch({

    final <-  missMDA::imputeMFA(df,group = groups,type = type,ncp = ncp,method = method,threshold = threshold,maxiter = maxiter,coeff.ridge = coeff.ridge)$completeObs
    },error = function(e){no_ok <<- TRUE}
    )
  tryCatch({
  if (no_ok | !exists('final')){final <-  missMDA::imputeMFA(df,group = groups,type = type,ncp = 1,method = method,threshold = threshold,maxiter = maxiter,coeff.ridge = coeff.ridge)$completeObs}
  if (!is.null(out_file)){
    write('  OK',file = out_file,append = T)

  }
  },error=function(e){
    if(!is.null(out_file)){
      write(as.character(e),file = out_file,append = T)
    }
    stop(e)
  })

    # adding 0_1 columns

if (col_0_1){
  columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
  colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
  final <- cbind(final,columns_with_missing)
}
return(final)
}


