

initHfNames <-function(n){
n2 <- n*n
hfNames <- array(rep("impossible", n2), dim = c(n,n))

hfNames[col(hfNames)<=row(hfNames)]<-"NULL"


return(hfNames)
}
