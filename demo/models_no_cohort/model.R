#An example of model definition for GUI_gems
#Names of the variables are important, they are being read into the model based on the names.

numStates <- 6
statesNames <- c("CHBV_nEFT", 
                    "CHBV_EFT",  
                    "clearance", 
                    "Cirrhosis", 
                    "HCC",       
                    "death"      
                    ) 
                    

hfNames <- array(rep("impossible", 36), dim = c(6,6)) #creates an array 6x6,
                                                      #all transitions impossible
hfNames[1:5,6] <- rep("Exponential",5)
hfNames[1,2]   <- "Exponential"
hfNames[1:2,3] <- rep("Weibull",2)
hfNames[1:2,4] <- rep("Exponential",2)
hfNames[4,5]   <- "Weibull"

hfNames[col(hfNames)<=row(hfNames)]<-"NULL" #only transmissions to the states 
                                            #with higher numbers are allowed
rownames(hfNames) <- statesNames
colnames(hfNames) <- statesNames

M <- makeM(hfNames)


params <- generateParameterMatrix(M)
params[[1,2]] <- list(rate = 0.7)                # an example parameter for exponential hazard function

params[[1,3]] <- list(shape = 0.3, scale = 1.2)  # an example parameters for Weibull hazard function
params[[1,3]] <- list(shape = 0.2, scale = 1.3)

params[[1,4]] <- list(rate = 0.02)
params[[2,4]] <- list(rate = 0.01)

params[[4,5]] <- list(shape = 0.5, scale = 0.7) 

params[[1,6]] <- list(rate = 0.002)
params[[2,4]] <- list(rate = 0.003)
params[[3,5]] <- list(rate = 0.003)
params[[4,6]] <- list(rate = 0.04)
params[[5,6]] <- list(rate = 0.07)

cohortSize <- 200
