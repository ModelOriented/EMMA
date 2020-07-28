#' Performing randomSearch for selecting best method and correlation or fraction of features used to create prediction matrix.
#'
#' @description This function perform random search and return values corresponding to best mean IMF (missing information fraction).
#'
#' @param low_corr double between 0,1 default 0 lower boundry of correlation set.
#' @param up_corr double between 0,1 default 1 upper boundary of correlation set. Both of these parameters work the same for a fraction of features.
#' @param methods_random set of methods to chose. Default 'pmm'.
#' @param df data frame to input.
#' @param formule first product of formula_creating() funtion. For example formula_creating(...)[1]
#' @param no_numeric second product of formula_creating() function.
#' @param iter number of iteration for randomSearch.
#' @param random.seed radnom seed.
#' @param correlation If True correlation is using if Fales fraction of features. Default True.
#'
#' @detalist Even if correlation is set at False correlation its still use to select best features. That mean problem with
#' calculating correlation between categorical columns is still important.
#'
#' @return List with best correlation (or fraction ) at first place, best method at second, and results of every iteration at 3.

random_param_mice_search <- function(low_corr=0,up_corr=1,methods_random = c('pmm'),df,formula,no_numeric,iter,random.seed=123,correlation=T){

  set.seed(random.seed)
  corr <- runif(iter,0,1)
  met <- sample(methods_random,iter,replace = T)


  # Performing random search and saving result
  result <- rep(1,iter)

  for (i in 1:iter){
    skip_to_next <- F

    tryCatch(
      {
        if (correlation){
          inputation <- mice(df,method = met[i],pred=quickpred(df, mincor=corr[i],method = 'spearman'),seed = random.seed)}
        if (!correlation){
          inputation <- mice(df,method = met[i],pred=quickpred(df, minpuc=corr[i],method = 'spearman'),seed = random.seed)
        }

        if (as.logical(no_numeric[1])){
          fit <- with(inputation,glm(as.formula(as.character(formula)),family = binomial))
        }
        if (!as.logical(no_numeric[1])){

          fit <- with(inputation,expr = lm((as.formula(as.character(formula)))))
        }
        result[i] <- mean(tidy(pool(fit))$fmi)

      }, error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }

  }

  # Returning result
  return(list(corr[which.min(result)],met[which.min(result)],result))

}
