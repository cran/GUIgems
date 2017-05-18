timeInStates <- function(mydata, end_time=100){
#mydata - cohort@time.to.state
#res -times in each state for every patient  
#TODO:
  nr <- nrow(mydata)
  nc <- ncol(mydata)
  tmp <- mydata*0
  tmp[is.na(tmp)] <- 0
  tmp <- tmp+end_time+1
  tmp1 <- tmp
  mydata[,nc+1]<- end_time

  #Checking the differences between subsequent stages, with distance 2, 3, etc.
  for (f in c(1:(nc))){
  tmp1[, 1:(nc+1-f)]<- (-1)*(mydata[,1:(nc+1-f)]-mydata[,(f+1):(nc+1)])
  tmp1[is.na(tmp1)]<-end_time+1
  tmp1[tmp1<0]<-end_time +1
  tmp <- pmin((tmp),(tmp1))
  }
  
  res <- tmp
  res[res==(end_time+1)]<-0
  print(dim(res))
  View(res)
  return(res)
}

sumTimeInStates <- function(mydata, end_time = 100){
  colSums(timeInStates(mydata, end_time))
  
}