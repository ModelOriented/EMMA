



autotune_Amelia <- function(df,col_type,percent_of_missing,col_0_1=FALSE,parallel=TRUE,polytime=NULL,splinetime=NULL,intercs=FALSE,optimize_empir=TRUE,empir=0,
                            threshold=Inf) {
  # prepering information about categorical column
  categorical_col <-  colnames(df)[ifelse(col_type=='factor',T,F)]

  # seting parallel options
  if(parallel){
    parallel <- 'multicore'
  }
  if(!parallel){
    parallel <- 'no'
  }



}
# col_type <- 1 :34
# for ( i in col_type){
#
#   col_type[i] <- class(data[,i])
# }
# w<- colnames(data[,-1])[ifelse(col_type[-1]=='factor',T,F)]
# tryCatch({
# c <- capture.output(amelia(data[,-1],noms = w,parallel = 'multicore',incheck = T,empri = 0,m = 1))
# },error = function(e){print('dupa')})
#
