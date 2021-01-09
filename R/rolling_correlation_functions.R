#' @title joethomasRfunctions
#' @description Functions that calculate rolling correlations.
#' @export
# rollingCorrelation - written by Joseph Thomas
#   Purpose: Calculate rolling correlations with these functions.
#     Options for correlation include pearson, spearman, and kendall.
rollingCorrelation<-function(data1,data2,windowWidth=10,method="pearson")
{
  startingPoint<-round((windowWidth/2)+0.1) #Starts the correlations off at the mid point of the window to ensure that there is equal data in all windows. The 0.1 is because the round function in R rounds a value of 0.5 down to 0. The 0.1 adds a negligible value but results in a round up which is needed.
  endingPoint<-(length(data1)-(round(windowWidth/2))) #Ends the correlations at the final set of digits that matches the window width. As.integer rounds down which is necessary for this to work.
  ##Loop time
  begin<-0 #We want to create a value for start at 0 so that we can add to it.
  out<-c()
  for(i in startingPoint:endingPoint)
  {
    begin<-begin+1 #This begins the loop and defines the boundaries of the data.
    firstValue<-begin #Defines the first value in the vector to correlate.
    lastValue<-firstValue+(windowWidth-1) #Defines the final value in the vector. This is simply the first vector plus the window width minus 1.
    #
    #Subgroup the data
    sub1<-data1[firstValue:lastValue]
    sub2<-data2[firstValue:lastValue]
    #
    #Correlate
    cor<-cor.test(x = sub1,y = sub2,method = method)
    #Extract the coeff value
    coeff<-cor$estimate
    #Compile the values
    out[i]<-unlist(coeff)
  }
  return(out)
}
#
#matrixRollingCorrelation
# Purpose: Similar to rollingCorrelation, but applied to a matrix of values.
matrixRollingCorrelation<-function(data,matrix,windowWidth=10,method="pearson",numberOfColumns=1){
  #The goal of this function is to apply a rolling correlation to matrix or ensamble.
  #First we need the rollingCorrelation function
  rollingCorrelation<-function(data1,data2,windowWidth=10,method="pearson")
  {
    startingPoint<-round((windowWidth/2)+0.1)
    endingPoint<-(length(data1)-(round(windowWidth/2)))
    begin<-0
    out<-c()
    for(i in startingPoint:endingPoint)
    {
      begin<-begin+1
      firstValue<-begin
      lastValue<-firstValue+(windowWidth-1)
      sub1<-data1[firstValue:lastValue]
      sub2<-data2[firstValue:lastValue]
      cor<-cor.test(x = sub1,y = sub2,method = method)
      coeff<-cor$estimate
      out[i]<-unlist(coeff)
    }
    return(out)
  }
  #Now that the rollingCorrelation function is loaded, we need to apply it to a matrix of values
  #Binning multiple columns
  result<-list()
  data1<-data
  for(i in 1:numberOfColumns)
  {
    column<-i
    data2<-matrix[,i]
    out<-rollingCorrelation(data1,data2,windowWidth=windowWidth,method=method)
    coeff<-i
    result[[coeff]]<-out
  }
  output<-matrix(unlist(result),ncol=numberOfColumns,byrow=TRUE)
  return(output)
}
