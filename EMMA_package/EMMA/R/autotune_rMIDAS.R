#' @title Multiple Imputation using Denoising Autoencoders
#'
#' @description The function creates an easy interface for rMIDAS package. More information about specific can be found here http://commonmark.org/help.
#'
#' @param data Data frame with missing values
#' @param col_type Columns type in data frame
#' @param path character string, path to python binary if type == "auto", path to virtualenv if type == "virtualenv", or the name of a Conda environment if type=="condaenv". More information aboute Pythone enviroment can be found in Detalis.
#' @param type character string, one of 'auto' (for python binary),'virtualenv', or 'condaenv. More information aboute Pythone enviroment can be found in Detalis.
#' @param training_epchos An integer, indicating the number of forward passes to conduct when running the model.
#' @param layer_structure A vector of integers, The number of nodes in each layer of the network (default = c(256, 256, 256), denoting a three-layer network with 256 nodes per layer). Larger networks can learn more complex data structures but require longer training and are more prone to overfitting.
#' @param seed Random seed
#' @param learn_rate A number, the learning rate γ (default = 0.0001), which controls the size of the weight adjustment in each training epoch. In general, higher values reduce training time at the expense of less accurate results.
#'
#'
#' @details The function requires a Python environment created with package reticulate. The environment has to be created before function call and wouldn't be close by function. An example of Python background can be found in Example sections.
#'
#' @examples
#'  #Creating Python enviroment using conda
#'
#'
#'  reticulate::conda_create('impute')
#'  reticulate::conda_install('impute','scikit-learn')
#'  rMIDAS::set_python_env('impute',type="condaenv")
#'
#'  autotune_rMIDAS(data,col_type,'impute','condaenv')
#'
#'  # Can be change to any difrent  Python enviroment
#'
#'
#' @author 	Thomas Robinson , Ranjit Lall, Alex Stenlake
#'
#' @export
autotune_rMIDAS <- function(data,col_type,path,type,training_epchos=10L,layer_structure = c(256, 256, 256),seed =123,learn_rate=4e-04){
  df <- as.data.frame(data)
  if(sum(is.na(df))==0){return(df)}


  ## Checking enviroment
  if(!rMIDAS::set_python_env(path,type)){
    stop('Problem with enviroment')
  }

  ### Runing imputation

  # binary column
  bin_col <- c('mark')
  iter <- 2
  for (i in colnames(df)[col_type=='factor'] ){
    if(length(levels(df[,i]))==2){
      bin_col[iter] <- i
      iter <- iter +1
    }
  }
 bin_col <- bin_col[-1]

### convert factor

 df[col_type=='factor'] <- lapply(df[col_type=='factor'], as.character)
 ### convert numeric

 df[col_type=='integer'] <- lapply(df[col_type=='integer'], as.numeric)

  data_to_impute <-  rMIDAS::convert(df,
                                     bin_cols = c(bin_col),
                                     cat_cols = c(setdiff(colnames(df)[col_type=='factor'],bin_col)),
                                     minmax_scale = T
                                     )


  trained_data <- rMIDAS::train(data_to_impute,training_epochs = training_epchos,layer_structure = layer_structure,seed = seed ,learn_rate=learn_rate)
  final <- as.data.frame(complete(trained_data,m=1)[[1]])
  ## back to int
  for (i in colnames(final)[col_type == "integer"]) {
    final[, i] <- as.integer(final[, i])
  }
  ## back to factor
  for (i in colnames(data)[(col_type == "factor")]) {
    final[,i] <- as.factor(final[,i])
    if (!setequal(levels(na.omit(data[, i])), levels(final[, i]))) {

      levels(final[, i]) <- c(levels(na.omit(df[, i])))
    }
  }


  return(list('model'=trained_data,'data'=final))
}



