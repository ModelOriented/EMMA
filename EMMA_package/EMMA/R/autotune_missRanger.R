#' Perform imputation using missRenger form missRegnger package.
#'
#' @description Function use missRenger package for data imputation. Function use OBBerror (more in missForest documentation) to perform random search.
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param maxiter maximum number of iteration for missRanger algorithm
#' @param random.seed random seed use in imputation
#' @param mtry sample fraction use by missRanger. This param isn't optimized automatically. If NULL default value from ranger package will be used.
#' @param num.trees number of trees. If optimize == TRUE. Param set seq(10,num.trees,iter) will be used.
#' @param verbose If FALSE function doesn't print on console.
#' @param out_file Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @param pmm.k Number of candidate non-missing values to sample from in the predictive meanmatching step. 0 to avoid this step. If optimize == TRUE param set sample(1:pmm.k,iter) will be used. If pmm.k==0 missRanger == missForest.
#' @param optimize If TRUE inside optimization will be performed.
#' @param iter Number of iteration for a random search.
#'
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#' @import missRanger
#' @return Return data.frame with imputed values.
#' @export




autotune_missRanger <- function(df,percent_of_missing,maxiter=10,random.seed=123,mtry=NULL,num.trees=500,verbose=F,col_0_1=F,out_file=NULL,pmm.k=5,optimize=T,iter=10){

  if(!is.null(out_file)){
    write('missRanger',file = out_file,append = T)
  }
  if(sum(is.na(df))==0){return(df)}
  if(sum(percent_of_missing==100)>0){
    if(!is.null(out_file)){
      write('Feature contains only NA',file = out_file,append = T)
    }
    stop('Feature contains only NA')
  }

  if(!is.null(pmm.k)){
    if(!optimize & pmm.k==0){
      print('missForest will be use')
    }
  }

  tryCatch({

    if (optimize){
      # random param set preper



      num.trees <- floor(seq.int(10,num.trees,iter))
      pmm.k <- sample(1:pmm.k,iter,replace = T)


      # random param search
      best_param <- c(500,5)
      best_oob <- 1
      for (i in 1:iter){
        go_next <- FALSE
        tryCatch({
        out <- capture.output(result <- missRanger::missRanger(df,maxiter = maxiter,seed = random.seed,num.trees=num.trees[i],verbose = 2,pmm.k = pmm.k[i]))

        # Reading oob from last iteration
        curent_oob <- mean(as.numeric(strsplit(out[[length(out)]], split='\t')[[1]][-1]))
        },error=function(e){go_next <<- TRUE})
        if(go_next){next}
        if (curent_oob<=best_oob){
          best_oob <- curent_oob
          best_param <- c(num.trees[i],pmm.k[i])
        }

      }

      num.trees <- best_param[1]
      pmm.k <- best_param[2]

      if(!is.null(out_file)){
        write(best_param,file = out_file,append = T)
      }
    if(!is.null(mtry)){
      final <- missRanger::missRanger(df,maxiter = maxiter,seed = random.seed,num.trees=num.trees,sample.fraction=mtry,verbose = as.numeric(verbose),pmm.k = pmm.k)
    }
   else{final <- missRanger::missRanger(df,maxiter = maxiter,seed = random.seed,num.trees=num.trees,verbose = as.numeric(verbose),pmm.k = pmm.k)}


    }
    if(!optimize){
      if (is.null(mtry)){

        final <- missRanger::missRanger(df,maxiter = maxiter,seed = random.seed,num.trees=num.trees,verbose = as.numeric(verbose),pmm.k = pmm.k)
      }
      else{final <- missRanger::missRanger(df,maxiter = maxiter,seed = random.seed,num.trees=num.trees,verbose = as.numeric(verbose),sample.fraction=mtry/ncol(df),pmm.k = pmm.k)}


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
        if (col_0_1){
          columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
          colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
          final <- cbind(final,columns_with_missing)
        }
  return(final)




}






