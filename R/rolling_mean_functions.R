#' @title joethomasRfunctions
#' @description Functions that calculate rolling mean values.
#' @export
# rollingMean - written by Joseph Thomas
#   Purpose: Calculated rollingMean values quickly.
#     The primary input is a vector of data. Output is a vector of mean values.
rollingMean<-function(data,windowWidth=10)
{
  startingPoint<-round((windowWidth/2)+0.1) #Starts the correlations off at the mid point of the window to ensure that there is equal data in all windows. The 0.1 is because the round function in R rounds a value of 0.5 down to 0. The 0.1 adds a negligible value but results in a round up which is needed.
  endingPoint<-(length(data)-(round(windowWidth/2))) #Ends the correlations at the final set of digits that matches the window width. As.integer rounds down which is necessary for this to work.
  ##Loop time
  begin<-0 #We want to create a value for start at 0 so that we can add to it.
  out<-c()
  for(i in 1:(endingPoint-startingPoint))
  {
    begin<-begin+1 #This begins the loop and defines the boundaries of the data.
    firstValue<-begin #Defines the first value in the vector to correlate.
    lastValue<-firstValue+(windowWidth-1) #Defines the final value in the vector. This is simply the first vector plus the window width minus 1.
    #
    #Subgroup the data
    sub<-data[firstValue:lastValue]
    #
    #Mean
    Mean<-mean(sub)
    #Compile the values
    out[i]<-Mean
  }
  return(out)}
