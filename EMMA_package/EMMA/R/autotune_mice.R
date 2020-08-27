#' Creating a formula for use in mice imputation evaluation.
#'
#' @description Function is used in \code{\link{autotune_mice}} but can be use sepraetly.
#' @details Function create a formula as follows. It creates one of the formulas its next possible formula impossible possible formula is created: \cr 1. Numeric no missing ~ 3 numeric with most missing \cr 2. Numeric no missing ~ all available numeric with missing \cr 3. Numeric with less missing ~ 3 numeric with most missing \cr 4. Numeric with less missing ~ all available numeric with missing \cr 5. No numeric no missing ~ 3 most missing no numeric \cr 6. No numeric no missing ~ all available no numeric with missing \cr 7. No numeric with less missing ~ 3 no numeric with most missing \cr 8. No numeric with less missing ~ all available no numeric with missing.
#' \cr For example, if its impossible to create formula 1 and 2 formula 3 will be created but if it's possible to create formula 1 and 5 formula 1 will be created.
#' @param df data.frame. Data frame to impute missing values with column names.
#' @param col_miss character vector. Names of columns with NA.
#' @param col_no_miss character vector. Names of columns without NA.
#' @param col_type character vector. A vector containing column type names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @import mice
#' @usage formula_creating(df,coll_miss,coll_no_miss,coll_type,percent_of_missing)
#' @return List with formula object[1] and information if its no numeric value in dataset[2].

formula_creating <- function(df,col_miss,col_no_miss,col_type,percent_of_missing){


  # Flags if no numeric value in df
  no_numeric <-  T

  #If df contains numeric values
  if  ('numeric' %in% col_type | 'intiger' %in% col_type){
    no_numeric <- F
    numeric_columns <- colnames(df)[ifelse('numeric' == col_type | 'intiger' == col_type,T,F)]

    #If some numeric columns don't contain missing data
    numeric_no_missing <- intersect(numeric_columns,colnames(df)[percent_of_missing==0])
    if (length(numeric_no_missing)>0){
      predicted_value <- numeric_no_missing[1]
      if (sum(percent_of_missing>0)>=3){
        columns_missing  <-  as.data.frame(cbind(percent_of_missing,colnames(df)))
        columns_missing <- columns_missing[order(as.numeric(as.character(columns_missing$percent_of_missing)),decreasing = TRUE),]
        predicting_values <- columns_missing$V2[1:3]
      }
      else{ predicting_values <- col_miss}

    }

    else{
      columns_missing_type <- as.data.frame(cbind(percent_of_missing,colnames(df),col_type))
      columns_missing_type_n_i <- columns_missing_type[columns_missing_type$col_type=='numeric' | columns_missing_type$col_type == 'initger',]
      if (length(row.names(columns_missing_type_n_i))>=1) {
        predicted_value <- columns_missing_type_n_i[order(columns_missing_type$percent_of_missing),'V2'][1]}
      else {no_numeric <-  T }
      if (length(row.names(columns_missing_type[-1,]))>=3){
        predicting_values <-  columns_missing_type[order(as.numeric(as.character(columns_missing_type$percent_of_missing)),decreasing = T),'V2'][1:3]
      }
      else{predicting_values <-setdiff(col_miss,as.character(predicted_value))}
    }


  }
  # If df don't contains numeric values
  if (no_numeric){
    predicted_value <- col_no_miss[1]
    if (sum(percent_of_missing>0)>=3){
      columns_missing  <-  as.data.frame(cbind(percent_of_missing,colnames(df)))
      columns_missing <- columns_missing[order(as.numeric(as.character(columns_missing$percent_of_missing)),decreasing = TRUE),]
      predicting_values <- columns_missing$V2[1:3]
    }
    else{ predicting_values <- col_miss}
  }



  return(list(as.formula(paste(as.character(predicted_value),paste(as.character(predicting_values),collapse = '+'),sep='~')),no_numeric))

}



#' Performing randomSearch for selecting the best method and correlation or fraction of features used to create a prediction matrix.
#'
#' @description This function perform random search and return values corresponding to best mean MIF (missing information fraction). Function is mainly used in \code{\link{autotune_mice}} but can be use separately.
#' @details Function use Random Search Technik to found the best param for mice imputation. To evaluate the next iteration logistic regression or linear regression (depending on available features) are used. Model is build using a formula from \code{\link{forumla_creating}} function. As metric MIF (missing information fraction) is used. Params combination with lowest (best) MIF is chosen. Even if a correlation is set at False correlation it's still used to select the best features. That main problem with
#' calculating correlation between categorical columns is still important.
#' @param low_corr double between 0,1 default 0 lower boundry of correlation set.
#' @param up_corr double between 0,1 default 1 upper boundary of correlation set. Both of these parameters work the same for a fraction of features.
#' @param methods_random set of methods to chose. Default 'pmm'.
#' @param df data frame to input.
#' @param formule first product of formula_creating() funtion. For example formula_creating(...)[1]
#' @param no_numeric second product of formula_creating() function.
#' @param iter number of iteration for randomSearch.
#' @param random.seed radnom seed.
#' @param correlation If True correlation is using if Fales fraction of features. Default True.
#' @import mice
#'
#' @return List with best correlation (or fraction ) at first place, best method at second, and results of every iteration at 3.

random_param_mice_search <- function(low_corr=0,up_corr=1,methods_random = c('pmm'),df,formula,no_numeric,iter,random.seed=123,correlation=T){

  set.seed(random.seed)
  corr <- runif(iter,0,1)
  if (!is.null(methods_random)){
  met <- sample(methods_random,iter,replace = T)
}
  if(is.null(methods_random)){
    met <- NULL
  }
  # Performing random search and saving result
  result <- rep(1,iter)

  for (i in 1:iter){
    skip_to_next <- F

    tryCatch(
      {
        if (correlation){
          inputation <- mice::mice(df,method = met[i],pred=mice::quickpred(df, mincor=corr[i],method = 'spearman'),seed = random.seed)}
        if (!correlation){
          inputation <- mice::mice(df,method = met[i],pred=mice::quickpred(df, minpuc=corr[i],method = 'spearman'),seed = random.seed)
        }

        if (as.logical(no_numeric[1])){
          fit <- with(inputation,glm(as.formula(as.character(formula)),family = binomial))
        }
        if (!as.logical(no_numeric[1])){

          fit <- with(inputation,expr = lm((as.formula(as.character(formula)))))
        }
        result[i] <- mean(mice::tidy(mice::pool(fit))$fmi)

      }, error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }

  }

  # Returning result
  return(list(corr[which.min(result)],met[which.min(result)],result))

}



#' Automatical tuning of parameters and imputation using mice package.
#'
#' @description Function impute missing data using mice functions. First perform  random search using linear models (generalized linear models if only
#' categorical values are available). Using glm its problematic. Function allows users to skip optimization in that case but it can lead to errors.
#' Function optimize prediction matrix and method. Other mice parameters like number of sets(m) or max number of iterations(maxit) should be set
#' as hight as possible for best results(higher values are required more time to perform imputation). If u chose to use one inputted dataset m is not important. More information can be found in \code{\link{random_param_mice_search}} and \code{\link{formula_creating}} and \code{\link[mice]{mice}}.
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
#' @param methods_random set of methods to chose. Default 'pmm'. If seted on NULL defoult method is used : By default, the method uses pmm, predictive mean matching (numeric data) logreg, logistic regression imputation (binary data, factor with 2 levels) polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) polr, proportional odds model for (ordered, > 2 levels).
#' @param iter number of iteration for randomSearch.
#' @param random.seed random seed.
#' @param optimize if user wont to optimize.
#' @param correlation If True correlation is using if Fales fraction of features. Default True.
#' @param return_one One or many imputed sets will be returned. Default True.
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param set_cor Correlation or fraction of featurs using if optimize= False
#' @param set_method Method used if optimize=False. If NULL default method is used (more in methods_random section ).
#' @param verbose If FALSE function didn't print on console.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @import mice
#' @importFrom mice complete
#' @return Return imputed datasets or mids object containing multi imputation datasets.
autotune_mice <- function(df,m=5,maxit=5,col_miss,col_no_miss,col_type,set_cor=0.5,set_method='pmm',percent_of_missing,low_corr=0,up_corr=1,methods_random=c('pmm'),iter,random.seed=123,optimize = T,correlation=T,return_one=T,col_0_1 = F ,verbose=FALSE,out_file=NULL){

  if(sum(is.na(df))==0){return(df)}

  formula_cre <- formula_creating(df,col_miss,col_no_miss,col_type,percent_of_missing)
  formula <- formula_cre[1]
  no_numeric <- as.logical(formula_cre[2])

  # If user chose to optimise no numeric dataset
  tryCatch({
  if (optimize){
    if(!is.null(out_file)){
      write('MICE',file = out_file,append = T)
    }
    params <- random_param_mice_search(df=df,low_corr = low_corr,up_corr = up_corr,methods_random = methods_random,formula = formula,no_numeric = no_numeric,random.seed = random.seed,iter=iter,correlation = correlation)
    #If user chose to use correlation
    if (!is.null(out_file)){

    write('correlation    method',file = out_file,append = T)
    write(c(params[[1]],params[[2]]),file = out_file,append = T)

    }

    if (correlation){

      imp_final <- mice::mice(df,printFlag = verbose,m=m,maxit = maxit,method = (params[[2]]),pred=mice::quickpred(df, mincor=(params[[1]]),method = 'spearman'),seed = random.seed)

      }
    if (!correlation){
      imp_final <- mice::mice(df,printFlag = verbose,m=m,maxit = maxit,method = (params[[2]]),pred=mice::quickpred(df, minpuc = (params[[1]]),method = 'spearman'),seed = random.seed)
    }



    }


  if (!optimize){

    if (correlation){
      imp_final <- mice::mice(df,printFlag = verbose,m=m,maxit = maxit,method = set_method,pred=mice::quickpred(df, mincor=set_cor,method = 'spearman'),seed = random.seed)
    }
    if (!correlation){
      imp_final <- mice::mice(df,printFlag = verbose,m=m,maxit = maxit,method = set_method,pred=mice::quickpred(df, minpuc = set_cor,method = 'spearman'),seed = random.seed)
    }
  }
  if(!is.null(out_file)){
    write('OK',file = out_file,append = T)
  }
  },error=function(e){
    if(!is.null(out_file)){
      write(as.character(e),file = out_file,append = T)
    }
    stop(e)
  })
  # If user chose to return one dataset
  if (return_one){
    imputed_dataset <- mice::complete(imp_final)
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



