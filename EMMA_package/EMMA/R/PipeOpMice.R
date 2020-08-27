#' @title PipeOpMice
#'
#' @name PipeOpMice
#'
#' @description
#' Implements mice methods as mlr3 pipeline more about mice \code{\link{autotune_mice}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_mice"}.
#' \item \code{m} :: \code{integer(1)}\cr
#' number of sets produced by mice, default \code{5}.
#' \item \code{maxit} :: \code{integer(1)}\cr
#' maximum number of iteration for mice, default \code{5}.
#' \item \code{set_corr} :: \code{double(1)}\cr
#' Correlation or fraction of featurs using if optimize= False. When correlation == F  its represent a fraction of features to use in imputation for each variable.  default \code{0.5}.
#' \item \code{set_method} :: \code{character(1)}\cr
#' Method used if optimize=False. If NULL default method is used (more in methods_random section ), default \code{'pmm'}.
#' \item \code{low_corr} :: \code{double(1)}\cr
#' double betwen 0,1 default 0 lower boundry of correlation used in inner optimization (used only when optimize == TRUE), default \code{0}.
#' \item \code{up_corr} :: \code{double(1)}\cr
#' double between 0,1 default 1 upper boundary of correlation set used in inner optimization (used only when optimize == TRUE). Both of these parameters work the same for a fraction of features if correlation == FALSE.,defoult \code{1}.
#' \item \code{methods_random} :: \code{character(1)}\cr
#' set of methods to chose. Default 'pmm'. If seted on NULL defoult method is used : By default, the method uses pmm, predictive mean matching (numeric data) logreg, logistic regression imputation (binary data, factor with 2 levels) polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) polr, proportional odds model for (ordered, > 2 levels), default \code{c('pmm')}
#' \item \code{iter} :: \code{integer(1)}\cr
#' number of iteration for randomSearch, default \code{5}.
#' \item \code{random.seed} :: \code{integer(1)}\cr
#' random seed, default \code{123}.
#' \item \code{optimize} :: \code{logical(1)}\cr
#' If set TRUE function will optimize parametrs of imputation automaticlly. If parametrs will be tune by other methode shoude be set as FALSE, default \code{FALSE}.
#' \item \code{correlation} :: \code{logical(1)}\cr
#' If True correlation is using if Fales fraction of features, default \code{TRUE}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#'  if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, Default \code{FALSE}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export
PipeOpMice <-  R6::R6Class("mice_imputation",lock_objects=FALSE,
                              inherit = PipeOpImpute,
                              public = list(
                                initialize = function(id = "imput_mice", m=5,maxit=5,set_cor=0.5,
                                                      set_method='pmm',low_corr=0,up_corr=1,
                                                      methods_random=c('pmm'),iter=5,random.seed=123,optimize = F,correlation=F,col_0_1=F,out_file=NULL
                                                      ) {
                                  super$initialize(id, whole_task_dependent=TRUE,param_vals = list( m=m,maxit=maxit,set_cor=set_cor,
                                                                          set_method=set_method,low_corr=low_corr,up_corr=up_corr,
                                                                          methods_random=methods_random,iter=iter,random.seed=random.seed,optimize = optimize,correlation=correlation,col_0_1=col_0_1,
                                                                          out_file=out_file),
                                                   param_set= ParamSet$new(list(
                                    'set_cor'=ParamDbl$new('set_cor', lower = 0, upper = 1, special_vals = list(), default = 0.5, tags = 'mice'),
                                    'iter'=ParamInt$new('iter',lower = 1,upper = Inf,default = 5,tags='mice'),
                                    'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 2,tags='mice'),
                                    'maxit'=ParamInt$new('maxit',lower = 5,upper = 100,default = 5,tags='mice'),
                                    'set_method'=ParamFct$new('set_method',levels = c('pmm','midastouch','sample','cart'),default = 'pmm',tags='mice'),
                                    'low_corr'=ParamDbl$new('low_corr',lower = 0,upper = 1,default = 0,tags='mice'),
                                    'up_corr'=ParamDbl$new('up_corr',lower = 0,upper = 1,default = 1,tags='mice'),
                                    'methods_random'=ParamFct$new('methods_random',levels=c('pmm','midastouch','sample','cart'),default = c('pmm'),tag='mice'),
                                    'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='mice'),
                                    'optimize'=ParamLgl$new('optimize',default = F,tags='mice'),
                                    'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='mice'),
                                    'correlation'=ParamLgl$new('correlation',default = F,tags='mice'),
                                    'out_file'=ParamUty$new('out_file',default = NULL,tags = 'mice')

                                ))
                                  )



                                  self$imputed <- FALSE
                                  self$column_counter <- NULL
                                  self$data_imputed <- NULL

                                }),private=list(

                                .train_imputer=function(feature, type, context){
                                  imp_function <- function(data_to_impute){




                                    data_to_impute <- as.data.frame(data_to_impute)
                                    # prepering arguments for function
                                    col_type <- 1:ncol(data_to_impute)
                                    for (i in col_type){
                                      col_type[i] <- class(data_to_impute[,i])
                                    }
                                    percent_of_missing <- 1:ncol(data_to_impute)
                                    for (i in percent_of_missing){
                                      percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                    }
                                    col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                    col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                    data_imputed <- autotune_mice(data_to_impute,col_miss = col_miss,col_no_miss = col_no_miss,col_type = col_type,
                                                                  percent_of_missing = percent_of_missing,m=self$param_set$values$m,iter=self$param_set$values$iter,
                                                                  maxit = self$param_set$values$maxit,
                                                                  low_corr = self$param_set$values$low_corr,up_corr = self$param_set$values$up_corr,
                                                                  set_cor = self$param_set$values$set_cor,set_method = self$param_set$values$set_method,
                                                                  methods_random = self$param_set$values$methods_random,random.seed = self$param_set$values$random.seed,
                                                                  optimize = self$param_set$values$optimize,
                                                                  correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1,verbose = F,
                                                                  out_file =self$param_set$values$out_file,return_one = T
                                    )





                                    return(data_imputed)
                                  }
                                  self$imputed_predict <- TRUE
                                  self$flag <- 'train'
                                  if(!self$imputed){
                                    self$column_counter <- ncol(context)+1
                                    self$imputed <- TRUE
                                    data_to_impute <- cbind(feature,context)

                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed) <- self$state$context_cols

                                  }
                                  if(self$imputed){
                                    self$column_counter <- self$column_counter -1

                                  }
                                  if  (self$column_counter==0){
                                    self$imputed <- FALSE
                                  }
                                  self$train_s <- TRUE
                                  return(NULL)

                                },
                                .impute=function(feature, type, model, context){
                                  imp_function <- function(data_to_impute){


                                    data_to_impute <- as.data.frame(data_to_impute)
                                    # prepering arguments for function
                                    col_type <- 1:ncol(data_to_impute)
                                    for (i in col_type){
                                      col_type[i] <- class(data_to_impute[,i])
                                    }
                                    percent_of_missing <- 1:ncol(data_to_impute)
                                    for (i in percent_of_missing){
                                      percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                    }
                                    col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                    col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                    data_imputed <- autotune_mice(data_to_impute,col_miss = col_miss,col_no_miss = col_no_miss,col_type = col_type,
                                                                  percent_of_missing = percent_of_missing,m=self$param_set$values$m,iter=self$param_set$values$iter,
                                                                  maxit = self$param_set$values$maxit,
                                                                  low_corr = self$param_set$values$low_corr,up_corr = self$param_set$values$up_corr,
                                                                  set_cor = self$param_set$values$set_cor,set_method = self$param_set$values$set_method,
                                                                  methods_random = self$param_set$values$methods_random,random.seed = self$param_set$values$random.seed,
                                                                  optimize = self$param_set$values$optimize,
                                                                  correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1,verbose = F,
                                                                  out_file =self$param_set$values$out_file,return_one = T
                                    )





                                    return(data_imputed)
                                  }

                                  if (self$imputed){
                                      feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                                  }
                                    if((nrow(self$data_imputed)!=nrow(context) | !self$train_s ) & self$flag=='train'){
                                      self$imputed_predict <- FALSE
                                      self$flag <- 'predict'
                                    }

                                      if(!self$imputed_predict){
                                    data_to_impute <- cbind(feature,context)

                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed)[1] <- setdiff(self$state$context_cols,colnames(context))
                                    self$imputed_predict <- TRUE
                                  }


                                  if (self$imputed_predict & self$flag=='predict' ){
                                    feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]

                                  }

                                  if(self$column_counter == 0 & self$flag=='train'){
                                    feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]
                                    self$flag <- 'predict'
                                    self$imputed_predict <- FALSE
                                  }
                                  self$train_s <- FALSE

                                  return(feature)
                                }



                              )
)
mlr_pipeops$add("mice_imputation", PipeOpMice)



