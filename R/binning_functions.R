#' @title joethomasRfunctions
#' @description Functions that bin data
#' @export
# binneR - written by Joseph Thomas
#   Purpose: Bins together data into a specific number of bins.
#   Final value is a mean of all datapoints in a specific bin.
binner<-function(data,nBins=1,startingROW=1,endingROW=(NROW(data)),startingCOL=1,endingCOL=1){
  #Defining terms from the inputs
  rowm<-startingROW
  rowM<-endingROW
  colm<-startingCOL
  colM<-endingCOL
  #Counting the number of total values to bin
  total_number_of_values<-length(data[rowm:rowM,colm:colM])
  #Calculate the number of values in each bin
  values_per_bin<-total_number_of_values/nBins
  #Calculate the mean of values in each bin
  start=1
  mean_values=c()
  count=0
  bin_inside=values_per_bin

  for(i in 1:nBins){
    mean_values[i]<-mean(data[start:values_per_bin,colm:colM])
    count=count+1
    start=1+(bin_inside*count)
    values_per_bin=values_per_bin+bin_inside
  }
  return(mean_values)
}
#
#matrixBinneR - written by Joseph Thomas
# Purpose: Can bin multiple columns from the same matrix into the same number of bins at one time.
matrixBinneR<-function(data,nBins=1,startingROW=1,endingROW=(NROW(data)),numberOfColumns=1){
  #binneR function as the starting place
  binner<-function(data,nBins=1,startingROW=1,endingROW=(NROW(data)),startingCOL=1,endingCOL=1){
    #Defining terms from the inputs
    rowm<-startingROW
    rowM<-endingROW
    colm<-startingCOL
    colM<-endingCOL
    #Counting the number of total values to bin
    total_number_of_values<-length(data[rowm:rowM,colm:colM])
    #Calculate the number of values in each bin
    values_per_bin<-total_number_of_values/nBins
    #Calculate the mean of values in each bin
    start=1
    mean_values=c()
    count=0
    bin_inside=values_per_bin

    for(i in 1:nBins){
      mean_values[i]<-mean(data[start:values_per_bin,colm:colM])
      count=count+1
      start=1+(bin_inside*count)
      values_per_bin=values_per_bin+bin_inside
    }

    return(mean_values)
  }
  #Here we apply binneR to multiple columns
  result<-list()
  for(i in 1:numberOfColumns){
    out<-binner(data=data,nBins = nBins,startingROW = 1,startingCOL = i,endingCOL = i)
    coeff<-i
    result[[coeff]]<-out
  }
  output<-matrix(unlist(result),ncol=numberOfColumns,byrow=TRUE)
  return(output)}
#
#
#
