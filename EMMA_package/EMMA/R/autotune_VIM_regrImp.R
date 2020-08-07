#' Perform imputation using VIM packeg and irmi function #### UNFINISHED
#'
#' @description Function use IRMI (Iterative robust model-based imputation ) to impute missing data.
#'
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param eps threshold for convergency
#' @param maxit maximum number of iterations
#' @param step  stepwise model selection is applied when the parameter is set to TRUE
#' @param robust if TRUE, robust regression methods will be applied (it's impossible to set step=TRUE and robust=TRUE at the same time)
#' @param init.method Method for initialization of missing values (kNN or median)
#' @param force if TRUE, the algorithm tries to find a solution in any case, possible by using different robust methods automatically. (should be set FALSE for simulation)
#' @import VIM
#'
#' @return Return one data.frame with imputed values.

autotune_VIM_regrImp <- function(df,col_type,percent_of_missing,col_0_1=F,robust=F,mod_cat=F,verbose=T){


  # Cheking if imputation can be perform
  if(sum(percent_of_missing==0)==0){
    stop('No values with no missing values')
  }
  if (sum(is.na(df))==0){return(df)}
  DONT_WORK <- FALSE

  tryCatch({
  #Trying Full formula
  form_str <- paste(paste(colnames(df)[percent_of_missing>0],collapse = '+'),paste(colnames(df)[percent_of_missing==0],collapse = '+'),sep = '~')
  final <- regressionImp(formula = as.formula(form_str),data = df,robust = robust,mod_cat = mod_cat,imp_var = F)
  },error=function(e){
    DONT_WORK <<- TRUE

  })
  if (DONT_WORK){
    tryCatch({
      #Trying Full formula
      form_str <- paste(paste(colnames(df)[percent_of_missing>0],collapse = '+'),paste(colnames(df)[percent_of_missing==0&col_type=='numeric'],collapse = '+'),sep = '~')
      final <- regressionImp(formula = as.formula(form_str),data = df,robust = robust,mod_cat = mod_cat,imp_var = F)
      DONT_WORK <<- FALSE
    },error=function(e){
      DONT_WORK <<- TRUE

    })
  }

  if (DONT_WORK){
    tryCatch({
      #Trying Full formula
      form_str <- paste(paste(colnames(df)[percent_of_missing>0],collapse = '+'),paste(colnames(df)[percent_of_missing==0&col_type=='numeric'][1],collapse = '+'),sep = '~')
      final <- regressionImp(formula = as.formula(form_str),data = df,robust = robust,mod_cat = mod_cat,imp_var = F)
      DONT_WORK <<- FALSE
    },error=function(e){
      DONT_WORK <<- TRUE

    })
  }

  if(col_0_1){

    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)

  }

  return(final)



}



# test <- data
#
# col_type <- 1:ncol(test)
#
# for (i in col_type){
#   col_type[i] <- class(test[,i])
#
# }
#
# percent_of_missing <- 1:ncol(test)
#
# for (i in percent_of_missing){
#   percent_of_missing[i] <- (sum(is.na(test[,i]))/length(test[,1]))*100
# }
#
# wynik_test <- autotune_VIM_regrImp(test,col_type = col_type,percent_of_missing)
# sum(is.na(wynik_test))

