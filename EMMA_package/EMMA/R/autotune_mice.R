#' Automatical tuning of parameters and imputation using mice package.
#'
#' @description Function impute missing data using mice functions. First perform random search using linear models (generalized linear models if only
#' categorical values are available). Using glm its problematic. Function allows user to skip optimization in that case but it can lead to errors.
#' Function optimize prediction matrix and method. Other mice parameters like number of sets(m) or max number of iterations(maxit) should be set
#' as hight as possible for best results(higher values are required more time to perform imputation). If u chose to use one inputted dataset m is not important.
#'
#'
#'
#' @param df data frame for imputation.
#' @param m number of sets produced by mice.
#' @param maxit maximum number of iteration for mice.
#' @param col_miss name of columns with missing values.
#' @param col_no_miss character vector. Names of columns without NA.
#' @param col_type character vector. Vector containing column type names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param low_corr double betwen 0,1 default 0 lower boundry of correlation set.
#' @param up_corr double between 0,1 default 1 upper boundary of correlation set. Both of these parameters work the same for a fraction of features.
#' @param methods_random set of methods to chose. Default 'pmm'.
#' @param iter number of iteration for randomSearch.
#' @param random.seed random seed.
#' @param optimize_no_numeric if user wont to optimize when no numeric values exist in df. Default False.
#' @param correlation If True correlation is using if Fales fraction of features. Default True.
#' @param return_one One or many imputed sets will be returned. Default True.
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#'
#'
#'
#' @return Return imputed datasets or mids object containing multi imputation datasets.
autotune_mice <- function(df,m=5,maxit=5,col_miss,col_no_miss,col_type,percent_of_missing,low_corr=0,up_corr=1,methods_random=c('pmm'),iter,random.seed=123,optimize_no_numeric = F,correlation=T,return_one=T,col_0_1 = F ){



  formula_cre <- formula_creating(df,col_miss,col_no_miss,col_type,percent_of_missing)
  formula <- formula_cre[1]
  no_numeric <- as.logical(formula_cre[2])

  # If user chose to optimise no numeric dataset
  if (!optimize_no_numeric){
    params <- random_param_mice_search(df=df,low_corr = low_corr,up_corr = up_corr,methods_random = methods_random,formula = formula,no_numeric = no_numeric,random.seed = random.seed,iter=iter,correlation = correlation)
    #If user chose to use correlation
    if (correlation){
      imp_final <- mice(df,m=m,maxit = maxit,method = as.character(params[2]),pred=quickpred(df, mincor=as.numeric(params[1]),method = 'spearman'),seed = random.seed)
    }
    if (!correlation){
      imp_final <- mice(df,m=m,maxit = maxit,method = as.character(params[2]),pred=quickpred(df, minpuc = as.numeric(params[1]),method = 'spearman'),seed = random.seed)
    }
  }


  if (optimize_no_numeric){
    if (correlation){
      imp_final <- mice(df,m=m,maxit = maxit,method = 'pmm',pred=quickpred(df, mincor=0.5,method = 'spearman'),seed = random.seed)
    }
    if (!correlation){
      imp_final <- mice(df,m=m,maxit = maxit,method = 'pmm',pred=quickpred(df, minpuc = 0.5,method = 'spearman'),seed = random.seed)
    }
  }
  # If user chose to return one dataset
  if (return_one){
    imputed_dataset <- complete(imp_final)
    # If user chose to return 0,1 columns
    if (col_0_1 ){
      where_imputed <- as.data.frame(imp_final$where)[,imp_final$nmis>0]
      colnames(where_imputed) <- paste(colnames(where_imputed),'where',sep = '_')
      imputed_dataset <- cbind(imputed_dataset,where_imputed*1)
    }

    return(imputed_dataset)
  }
  if(!return_one){
    return(imp_final)
  }




}
