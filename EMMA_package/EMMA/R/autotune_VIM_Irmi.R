#' Perform imputation using VIM package and irmi function
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
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @import VIM
#'
#' @return Return one data.frame with imputed values.
autotune_VIM_Irmi <- function(df,percent_of_missing,eps=5,maxit=100,step=FALSE,robust=FALSE,init.method='kNN',force=FALSE,col_0_1=FALSE,out_file=NULL){

  if(!is.null(out_file)){
    write('VIM_IRMI',file = out_file,append = T)
  }

  tryCatch({

    final <- irmi(df,eps=eps,maxit = maxit,step = step,robust = robust,init.method = init.method,force = force,imp_var = F)
    if(!is.null(out_file)){
      write('  OK ',file = out_file,append = T)
    }
  },error = function(e){
    if(!is.null((out_file))){
      write(as.character(e),file = out_file,append = T)
    }
    print('IRMI dont work on selcted params runing on defoult')
    final <- irmi(df,imp_var = F)
  })



  if(col_0_1){

    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)

  }

  return(final)

}



