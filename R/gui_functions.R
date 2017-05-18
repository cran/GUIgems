#R CMD Rd2pdf ../Gems2 
#Guigems
#
#Author: Zofia Baranczuk
#User interface for "gems"


baselineFunction_empty <- function(cohortSize){data.frame(rep(NULL, cohortSize))}

# Return parameters for predefined functions
# @description predefinedParameters returnes list of the parameters set to NULL
# for the predefined hazard functions ("Weibull", "Exponential", "impossible") 
# @param
# preFun -- name of a predefined function.
# @return
# list of parameters of preFun
# @examples
# parE <- predefinedParameters("Exponential")
# # parE = list("rate"=NULL)
# @author
# Zofia Baranczuk
predefinedParameters <- function(preFun){
  params <- list("Exponential"=list("rate"=NULL), "Weibull"=list("shape"=NULL, "scale"=NULL), 
                 "multWeibull" = list( "weights"=NULL, "shapes"=NULL, "scales"=NULL),  "impossible"=list())
  return(params[preFun][[1]])
}

# how many rows can we show in the gui
nShow <- function(n){  
 max(n, 12)
}

baselineFunction_empy <- function(cohortSize) {
  baseline = matrix(NA, nrow = cohortSize)
}

# save function as file after editing using fixByString
# @param functionName, fileName
saveFunctionFile <- function(functionName, fileName){
  sink(fileName)
  cat(paste(functionName, " <- ", sep=""))
  print(eval(parse(text=functionName)))
  sink()
  
  tmp <- readLines(fileName)
  file.remove(fileName)
  l_tmp <- length(tmp)-1
  sink(fileName)
  for (f in seq(1,l_tmp,1)){
    cat(tmp[f])
    cat("\n")
  }
  sink()
}


# Reads cohort size given by the user in GUI and saves in mypanel$cohortSize.
# Used only from rpanel level
#
# @param mypanel, cohortSize
#
# @return mypanel
setCohortSize <- function(mypanel, cohortSize=0){
  if (cohortSize == 0)
    cohortSize <- as.numeric(mypanel$cohortSize_input) 
  #mypanel$cohortSize defined in the window already
  mypanel$cohortSize <- cohortSize  
  mypanel  
}


setMaxTime <- function(mypanel, max_time=0){
  if (max_time == 0)
    max_time <- as.numeric(mypanel$max_time) 
  #mypanel$cohortSize defined in the window already
  mypanel$max_time <- max_time  
  mypanel  
}

# Reads number of states given by the user in GUI and saves in mypanel$numStates. 
# Generates matrices for gems with required number of states. 
# Used only from rpanel level.
#
# @param mypanel, num
# @return mypanel
setStatesNum <- function(mypanel, num=0){
  if ((num==0))
    mypanel$numStates <- as.numeric(mypanel$num)
  else 
    mypanel$numStates <- num
  
  n <- mypanel$numStates
  nShow <- max(n, 12)
  mypanel$hfNames <- initHfNames(n)
  if (n!=length(mypanel$statesNames)){
    print("Not appropiate number of states names. Changing to basic states names ")
    mypanel$statesNames <- paste("State", c(1:n), sep=" ")
  }
  mypanel$M <-generateHazardMatrix(mypanel$numStates,mypanel$statesNames)
  dimnames(mypanel$hfNames) <- dimnames((mypanel$M)@list.matrix)
  mypanel$ttt <-matrix(FALSE, nrow=n, ncol=n)
  dimnames(mypanel$ttt) <- dimnames((mypanel$M)@list.matrix) 
  mypanel$updateM <- FALSE
  for (g in seq(2,n,1)){
    for (f in seq(1,g-1)){
      if ("ttt" %in% mypanel$hfNames[f,g]) {
        mypanel$ttt[f,g] <- TRUE
      }
      if (!(mypanel$hfNames[f,g] %in% c("Weibull", "Exponential", "impossible"))) {
        
        fun_exists <- tryCatch(mypanel$M[[f,g]] 
                               <-(eval(parse(text=mypanel$hfNames[f,g]))), 
                               error=function(error){print("NAF")})
        # does hfNames work here??)
        if (!is.function(fun_exists)){
         # print(fun_exists)
          if (fun_exists=="NAF"){
            mypanel$M[[f,g]] <-mypanel$hfNames[f,g]
            print("NAF, but here it is ok1")
            #TODO: name of the function, not yet defined
            mypanel$updateM <- TRUE
          }
     # }
      }
      else {
        print("for r it is a function")
        mypanel$M[[f,g]] <-mypanel$hfNames[f,g]
        print(mypanel$M[[f,g]])
        #Weibull and Exponential are not ttt 
      }
    }
  }
  }
  mypanel$params <-  tryCatch(  #TODO: if does not work with proper mastrix, empty parameters matrix
    generateParameterMatrix(mypanel$M),
    error = function(cond){
      #TODO - check if the problem are not defined hazard functions
      rpanel::rp.messagebox('Could not generate parameters matrix based on the hazard functions matrix
                    . Are the hazard functions defined?')
      return(generateParameterMatrix(generateHazardMatrix(n)))
    }
)
  mypanel$covariance <- tryCatch(
    generateParameterCovarianceMatrix(mypanel$params),
    error = function(cond){
      rpanel::rp.messagebox('Could not generate parameters covariance matrix. Are the hazard functions defined?')
      return(NA)
    }
  )
#baselineFunction_empty <- function(cohortSize){data.frame(rep(NULL, cohortSize))}
# mypanel@baselineFunction <- baselineFunction_empy
#mypanel$baseline <- baselineFunction_empy(mypanel$cohortSize)
drawTable(mypanel)
  mypanel
}

 
# Edits hazard function or parameters. From the GUI level only
# @param mypanel, skip_edit=FALSE - don't edit the hazard functions names, but updates M matrix. 
# @return mypanel
editHf <-function(mypanel, skip_edit=FALSE){
  n <- mypanel$numStates
  if (skip_edit  == FALSE){
    mypanel$hfNames <- edit((mypanel$hfNames))
  }
  print(mypanel$hfNames)
  for (g in seq(2,n,1)){
    for (f in seq(1,g-1,1)){      
      if ((!is.null(mypanel$hfNames[f,g]))&& (!mypanel$hfNames[f,g]=="NULL")) {
        if ("ttt" %in% mypanel$hfNames[f,g]) {
          mypanel$ttt[f,g] <- TRUE
        }
        if (!(mypanel$hfNames[f,g] %in% c("Weibull", "Exponential", "impossible", "multWeibull"))) {
          
          fun_exists <- tryCatch(mypanel$M[[f,g]] 
                                 <-(eval(parse(text=mypanel$hfNames[f,g]))),
                                 error=function(error){print("NAF")})
          if (!is.function(fun_exists)){
            if (fun_exists=="NAF"){
              mypanel$M[[f,g]] <-mypanel$hfNames[f,g]
              print("NAF, but here it is ok3")
              mypanel$updateM <- TRUE
            }
          #}    
        }
        else {
          mypanel$M[[f,g]] <-mypanel$hfNames[f,g]
          #Weibull and Exponential are not ttt 
        }
      }
    }
  }    
  }
  drawTable(mypanel)
  ## TODO: change the loop to updateM(mypanel) 
  ## update only change entries -how??
  ## get rid of the loop!
  mypanel
}

# Edits mypanel$ttt (time.to.transition) table. From the GUI level only. 
# Mypanel$ttt gets changed.
# @param mypanel
# @return mypanel
editTTT <-function(mypanel){
  mypanel$ttt <- edit(mypanel$ttt)
  mypanel
}

# Edits StatesNames. From the GUI level only
# @param mypanel
# @return mypanel
editStatesNames <-function(mypanel){
  statesNames <- edit(as.matrix(mypanel$statesNames))
  mypanel<-loadStatesNames(mypanel, statesNames)
  drawTable(mypanel)
  mypanel
  
}

# updateMypanel. After loading the model cohortSi ze and statesNumber are not updated and 
# it is necessary to click "update mypanel", to update all shown mypanel parameters.
# TODO: find a way to show updated mypanel parameters directly when they get changed 
updateMypanel <-function(mypanel){
  drawTable(mypanel)
  mypanel
  
}

# Loads StatesNames. From the GUI level only
# @param mypanel, statesNames - if NULL, we read states names from a file chosen by the user.
# @return mypanel
loadStatesNames <- function(mypanel, statesNames = NULL){
  if (is.null(statesNames)){ 
    snFile <- tryCatch(file.choose(), error = function(e) "") 
    if (!snFile==""){ 
    source(snFile, local=TRUE) # in form statesNames <- c("birth", "life", "death")
    source(snFile, local=FALSE)
    }
    else
      return(mypanel)
  }
  mypanel$statesNames <- statesNames
  #TODO: chceck if statesNames (from:, to:) and 
  new_M_list <- addStatesNames((mypanel$M)@list.matrix, statesNames)
  mypanel$M@list.matrix <- new_M_list
  dimnames(mypanel$hfNames)<-dimnames((mypanel$M)@list.matrix)   
  new_params <- addStatesNames(mypanel$params@list.matrix, statesNames)
  mypanel$params@list.matrix <- new_params
  
  if (exists("mypanel$covariance")){
    new_covariance <- addStatesNames(mypanel$covariance@list.matrix, statesNames)
    mypanel$covariance@list.matrix <- new_covariance
  }
  
  if (exists("mypanel$cohort")){
    print("colnames for a cohort")
    colnames(mypanel$cohort@time.to.state)<-statesNames
  }  
  
  drawTable(mypanel)
  mypanel  
}


# Prepares ttt matrix based on hfNames. It changes M matrix as well. 
# TODO: check if I need to change M!
# From GUI level only, it is solved in gems differently. 
# TODO: check it. Do I want to use gems function for it. 
# @param mypanel, hfNames
# @return mypanel
makeM_ttt <- function(mypanel, hfNames){
  mypanel$M <-generateHazardMatrix(mypanel$numStates,mypanel$statesNames)
  n <- mypanel$numStates
  mypanel$ttt <-matrix(FALSE, nrow=mypanel$numStates, ncol=mypanel$numStates)
  dimnames(mypanel$ttt) <- dimnames((mypanel$M)@list.matrix) 
  mypanel$updateM <- FALSE
  for (g in seq(2,n,1)){
    for (f in seq(1,g-1)){
      if ("ttt" %in% mypanel$hfNames[f,g]) {
        mypanel$ttt[f,g] <- TRUE
      }
      if (!(mypanel$hfNames[f,g] %in% c("Weibull", "Exponential", "impossible"))) {
        
        fun_exists <- tryCatch(mypanel$M[[f,g]] 
                               <-(eval(parse(text=mypanel$hfNames[f,g]))),
                               error=function(error){print("NAF")})
        if (!is.function(fun_exists)){
          if (fun_exists=="NAF"){
            mypanel$M[[f,g]] <- mypanel$hfNames[f,g]
            print("NAF, but here it is ok2")
            mypanel$updateM <- TRUE
          }    
       # }
      }
      else {
        print("for r it is a function")
        mypanel$M[[f,g]] <-mypanel$hfNames[f,g]
        print(mypanel$M[[f,g]])
        #Weibull and Exponential are not ttt 
      }
    }
  } 
  }
  mypanel 
}


# Makes hfNames based on matrix M. Usefull for GUI only. The names of M functions are not available,
# so it rewrites the whole functions.
# @param mypanel, M
# @return mypanel
# @export
makeHf <- function(M){
  hfNames <-array(rep("NULL",  M@states.number* M@states.number), dim<-c( M@states.number, M@states.number))
  
  for (f in seq(1: M@states.number-1)){
    for (g in seq(f+1: M@states.number)){
      hfNames[f,g] <-toString((M@list.matrix[f,g]))#, f,g,sep="_")
    }  
  }
  print(hfNames)
  hfNames
}


# Saves the model to the files. TODO:
# 1. Log file -- runable??. With all changes to the model etc.
# 2. Model to read by next start_gui
# 3. Model to read by the user (is it possible to have 2.==3. )
# 4. random seed
# 5. cohort if simulated
# 
# @param mypanel. TODO: in gems save cohort works similarly. Check if reusable.
# @return mypanel
#
saveModel <- function(mypanel){
  fileName <- tcltk::tclvalue(tcltk::tkgetSaveFile(defaultextension=".R", filetypes="{{R Data} {.RData}} {{All Files} {*.*}}"))
#initialfile=functionName, defaultextension=".R",
  #title="Save model...", filetypes="{R scritp {.R} {{All Files} {*.*}}"))
  a <- list(mypanel)
  print(fileName)
  if (!fileName==""){ 
  save(a, file = fileName)
  }
mypanel
}


# Loads a model from the file. Given as an argument or GUI-chosen.
# @param mypanel, filename. If filename == NULL, dialog box to choose one.
# @return mypanel
# 
loadModel <- function(mypanel, fileName = NULL){
  # basic elements of the model <-- null to check, if defined. 
  # If not, leave empty for further definitions
  #TODO: check if everything defined, if defined consistently
  # baseline - as a function or as a data frame  
  #read in the file with hazard functions if defined by hfNames
  #check which one is defined
  if (is.null(fileName)){
    print(fileName)
    modelFile <- tcltk::tk_choose.files(default = "", caption = "Select files",
                                 multi = TRUE, filters = NULL, index = 1)
  }  else
    modelFile <- fileName
  print(modelFile)
  #TODO: if not chosen --> file
  
  if(modelFile==""){
    return(mypanel)
  }
  
  hfFunctionsFile <- NULL
  baseline <- NULL
  baselineFunction <- 0
  hfNames <- NULL
  covariance <- 0
  ttt <-0
  cohortSize <- 0
  mypanel$cohort <- NULL
  M<-NULL
  params <-NULL
  statesNames <-NULL
  startRow <- 0
  startColumn <-0
 
  if (tools::file_ext(modelFile)=="RData"){ 
    a<-eval(parse(text=load(modelFile)))
    attributes1 <-c("numStates", "startRow",  "startColumn", "cohortSize", "statesNames", "hfNames",       
                   "M", "params", "max_time", "shiftXY", "addingState", "aa", "ttt", "covariance",
                   "baselineFunction", "bl")  
    newpanel <- a[[1]]
    for (f in attributes1){
      mypanel[[f]] <- newpanel[[f]]    
    }      

    attributes2 <- c( "cohort", "subcohorts", "cohPanel")
    for (f in attributes2){
      print(f)
      print(newpanel[[f]])
      print(mypanel[[f]])
      mypanel[[f]]  <- tryCatch( 
        newpanel[[f]],
        error=function(error){NULL}
      )
    }
    
     if(!is.null(newpanel$cohort)){
       print("COHORT = ")
       print(mypanel$cohort)
       mypanel <- showCohort(mypanel)
     }
    #TODO: subcohorts
    drawTable(mypanel)
    return(mypanel)
  }
  if (tools::file_ext(modelFile)=="R"){ 
    source(modelFile, local=TRUE)
  }
  
  if (is.null(modelFile))
  {
    print("No file chosen")
    return(mypanel)
  }
  if (!(is.null(M)))
    mypanel$M <- M
  else if (!is.null(hfNames))
    mypanel <- makeM_ttt(mypanel, hfNames) #actually, M should be defined too, otherwise params cannot be defined 
  #TODO: warning when functions not loaded before generating the parameters matrix
  if (!is.null(hfFunctionsFile))
    source(hfFunctionsFile)
  
  # necessery for the model: hf matrix (M), parameters matrix(p)
  if (!is.null(hfNames))
    mypanel$hfNames <- hfNames
  else 
    mypanel$hfNames <- makeHf(mypanel$M)
  print("mypanel$hfNames")
  
    #TODO: check consistency
  #  params_new <- generateParameterMatrix(mypanel$M)
  mypanel$params <- params
  mypanel$numStates <- M@states.number
  
  #matrix(FALSE, nrow=mypanel$numStates, ncol=mypanel$numStates)
  if (!(ttt==0)){ 
    mypanel$ttt <- ttt
    # edit(mypanel$ttt)
  } else {
    n<- M@states.number
    ttt <- matrix(FALSE, nrow=n, ncol=n)
    for (g in seq(2,n,1)){
      for (f in seq(1,g-1)){
        #    print(eval(parse(text=hfNames[f,g])))
        if (grepl("ttt",mypanel$hfNames[f,g])){
          ttt[f,g] <- TRUE   
        }
      }
    }
    mypanel$ttt <- ttt
  } 
  
    #may be defined by the user: coviariance, StatesNames, baseline
  if (!(is.null(cohortSize)))
    mypanel$cohortSize <- cohortSize
  else
    mypanel$cohortSize <- 100
  
  #View(mypanel$cohortSize)
  if (!(is.null(statesNames)))
    mypanel$statesNames <- statesNames
  
  if (any(covariance!=0)){
    mypanel$covariance <- covariance
  }
  else 
    mypanel$covariance <- generateParameterCovarianceMatrix(params)
  print(typeof(mypanel$covariance))
  
  
  if (is.function((baselineFunction))){
    mypanel$baselineFunction <- baselineFunction}

  #print("ok till now")
 drawTable(mypanel)
 mypanel
}

# addCohorts
#
# Adds a cohort to mypanel. mypanel$cohorts[[next]] <- cohort defined in the chosen file
# Called only from the GUI level.
# @param 
# mypanel - a GUI in which we want to analyze multiple cohorts
# @keywords addCohort
addCohort <- function(mypanel){
#  if (is.null(fileName)){
  
    modelFile <- tcltk::tk_choose.files(default = "", caption = "Select files",
                                 multi = TRUE, filters = NULL, index = 1)
 # }  else
#    modelFile <- fileName
  print(modelFile)
  if (tools::file_ext(modelFile)=="RData"){ 
    a <- eval(parse(text=load(modelFile)))
    newCohort <- a[[1]]$cohort
    newCohortBl <- as.data.frame(c(as.data.frame(newCohort@baseline), 
                                    as.data.frame(newCohort@time.to.state)))
    
    newCost <- a[[1]]$cost
    if (is.null(newCost))
      newCost <- "empty"
    print(newCost)
    print(newCohort)
   
  #initialize cohorts if that's the first one  
   if (length(mypanel$cohorts)==0){ 
     mypanel$cohorts <- list()
     mypanel$costs   <- list()
     mypanel$baselines     <- list()
     mypanel$subcohorts <- list()
   }
   else { 
     prev_states <- names(mypanel$cohorts[[1]]@time.to.state)
     new_states <- names(newCohort@time.to.state)   
     if (length(new_states)!=length(prev_states)){ 
       print("TODO: message box, cohort not added, states number differ") 
     return(mypanel)
     }
     if (any(new_states != prev_states))
       print("TODO: warining -- states names differes, we will be using states names 
             from the first added cohort or some other cohorts depending on the called function ")
   }
   
    mypanel$cohorts[[modelFile]] <- newCohort
    mypanel$costs[[modelFile]] <- newCost 
    mypanel$baselines[[modelFile]] <- newCohort@baseline
    mypanel$subcohorts[[modelFile]] <- newCohortBl

  }
#MC <- list(mypanel)
#save(MC, file="test_MC.RData")
mypanel
}



# opends the list with names of the cohorts in the editor. names(Mypanel$cohorts) get changed.
editCohortsNames <- function(mypanel){
  cohortsNames_mypanel <- names(mypanel$cohorts)
  cohortsNames <- edit(as.matrix(cohortsNames_mypanel))
  names(mypanel$subcohorts) <- cohortsNames 
  names(mypanel$cohorts) <- cohortsNames 
  if (length(names(mypanel$baselines))==length(cohortsNames)){  
  names(mypanel$baselines) <- cohortsNames 
  }
  if (length(names(mypanel$costs))==length(cohortsNames))
  names(mypanel$costs) <- cohortsNames 
  mypanel
} 

plotCostsMypanel <- function(mypanel){
  plotCosts(mypanel$cohorts, mypanel$costs)
  mypanel
}

# Plots costs for the given cohorts and costs
# TODO: adjust legend so that it is visible and does not cover plot points
plotCosts <- function(cohorts, costs){
  QALY <- c()
  cost <- c()
  for (f in seq(1:length(costs))){ 
    if (!is.null(costs[[f]])){ 
  xy_cost <- countCost(cohorts[[f]], costs[[f]])
# else
 #   lines(xy_cost[1], xy_cost[2], pch=f, type="o", col=f)
  QALY<-c(QALY, xy_cost[1])
  cost<-c(cost, xy_cost[2])
}
else
  print("messagebox: no cost for cohort")
}  
plot(QALY, cost)
for (f in seq(1:length(costs))){ 
  if (f==1)
    plot(QALY[f], cost[f],  pch=f, type="o", col=f, ylim=c(min(cost)-0.2, max(cost)+0.2), 
         xlim=c(min(QALY)-0.1, max(QALY)+0.1), xlab="QALY", ylab ="cost")
  
  lines(QALY[f], cost[f],  pch=f, type="o", col=f, xlab="QALY", ylab ="cost")
}
par(xpd=TRUE)
legend("top" , inset=c(0,-0.2), basename(names(costs)) , cex=0.8, col=1:length(costs),
       pch=1:length(costs) );

}

# opens new rpanel to load and analyze multiple cohorts 
multipleCohorts <- function(mypanel){ 
  costToView <- NULL
  functionToPlot <- NULL
  subcohortToView <- NULL
  StatesToPlot <- NULL
  
  cohorts <- list()
  costs <- list()
  subcohorts <- list()
  bls <- list()
  
  if ("time.to.state" %in% names(attributes(mypanel$cohort)))
  cohorts <- list("first"  =mypanel$cohort)
  
  if (length(mypanel$cost)>2)
  costs <- list("first" = mypanel$cost)
  
  if (all(list("baseline", "time.to.state") %in%  names(attributes(mypanel$cohort))))
  subcohorts <- list("first" = as.data.frame(c(as.data.frame(mypanel$cohort@baseline), 
                                                as.data.frame(mypanel$cohort@time.to.state)))) 

  mypanel$MCohorts <- rpanel::rp.control(numStates=mypanel$numStates,
                                 statesNames = mypanel$statesNames, 
                                 cohorts = cohorts, costs = costs, subcohorts = subcohorts, 
                                 bls=bls)  
  #baseline not in mypanel, but in cohorts. TODO: change everywhere
  rpanel::rp.button(mypanel$MCohorts, addCohort, "Add cohort")
  rpanel::rp.button(mypanel$MCohorts, editCohortsNames, "Edit cohorts names")
  rpanel::rp.textentry(mypanel$MCohorts,costToView, action=ViewCost, initval = c(1), "View cost")
  rpanel::rp.textentry(mypanel$MCohorts,subcohortToView, action=ViewSubcohort, initval = c(1), "View cohort")  
  rpanel::rp.button(mypanel$MCohorts, listSubcohorts, "List cohorts")
  rpanel::rp.button(mypanel$MCohorts , action=saveModel, "Save the model") 
  rpanel::rp.combo(mypanel$MCohorts,functionToPlot, prompt = "", vals=c("transitionProbabilities", "cumulativeIncidence"))#,
  rpanel::rp.textentry(mypanel$MCohorts,StatesToPlot, labels=c("states", "cohorts"),
             initval =c("\"c(1:4)\"", "\"c(1,2)\""),title = "Plot transition functions")
  rpanel::rp.button(mypanel$MCohorts,plotFun, "Plot")          
  rpanel::rp.button(mypanel$MCohorts, plotCostsMypanel, "Plot costs")

return(mypanel)   
}
  
  


# Refresh the GUI. TODO: split into smaller functions??
# @param mypanel
# @return mypanel
drawTable <- function(mypanel){
  #for R cmd check
  max_time <- NULL
  shiftXY <- NULL
  addingState <- NULL
  aa <- NULL
  #for R cmd check
  
  n <- mypanel$numStates
  print(n)
  startRow <- 0
  startColumn <- 0
  if (!is.null(mypanel$startRow)){ 
  startRow <- mypanel$startRow
  startColumn <- mypanel$startColumn
  }
  nShow <- min(12, n)
  
  tmpr <- startRow + nShow - 1
  tmpc <- startColumn + nShow -1
  hnShow <- floor(nShow/2)
    
  for (i in seq(startRow,(startRow+nShow +8),1)){
    for (j in seq(startColumn,(startColumn+nShow+8),1)){
      grid_name = paste("hf", toString(i), toString(j), sep="_")
      rpanel::rp.grid(mypanel, pos=list(row=i-1, column=j-1, width=60, height=20), name=grid_name)  
    }
  }
  maxnShow = 16 
  rpanel::rp.button(mypanel,action=editTTT, "Edit ttt matrix",            pos = list (row = maxnShow + 2, column=5,columnspan = 2), name="editTTT")
  rpanel::rp.button(mypanel,action=editStatesNames, "Edit states names",  pos = list (row = maxnShow + 3, column=5,columnspan = 2), name="editSN")
  
  rpanel::rp.button(mypanel,action=editHf, "Edit hf matrix",              pos = list (row = maxnShow + 4, column=5,columnspan = 2), name="editHF")
  rpanel::rp.button(mypanel,action=graphHf_mypanel, "Plot hf graph",      pos = list (row = maxnShow + 5, column=5,columnspan = 2), name="plotHFGraph")
  rpanel::rp.button(mypanel,action=loadHf, "Load hazard functions",       pos = list (row = maxnShow + 5, column=1,columnspan = 2), name="lhf")
  rpanel::rp.button(mypanel,action=loadBaselineFunction, "Load baseline", pos = list (row = maxnShow + 8, column=1,columnspan = 2), name="lbl")
 # rpanel::rp.button(mypanel,action=editBaselineFunction, "Edit baseline", pos = list (row = maxnShow + 9, column=1,columnspan = 2), name="lbl")
  
  rpanel::rp.button(mypanel, action=saveModel, "Save the model",          pos = list (row = maxnShow + 6, column=1,columnspan = 2), name="saveModel")
  rpanel::rp.button(mypanel,action=loadModel, "Load model",               pos=list(row = maxnShow + 8,    column=3,columnspan = 2), name="lmodel")
  rpanel::rp.button(mypanel,action=updateMypanel, "Update panel",         pos=list(row = maxnShow + 9,    column=3,columnspan = 2), name="lmodel")
  
  rpanel::rp.button(mypanel,action=multipleCohorts, "Multiple cohorts",   pos=list(row = maxnShow + 10,    column=3,columnspan = 2), name="MCohorts")
  num <- mypanel$numStates
  mypanel$num <- mypanel$numStates
  rpanel::rp.textentry(mypanel, num, action=setStatesNum,                 pos = list(row = maxnShow + 3,  column =1,columnspan = 2),
               initval = num, "Number of states", name="lns")
  mypanel$cohortSize_input <- NULL
  cohortSize_input <- NULL
  a <- mypanel$cohortSize
  rpanel::rp.textentry(mypanel, cohortSize_input, action=setCohortSize,   pos = list(row = maxnShow + 3,  column =3,columnspan = 2),
               initval= a, "Cohort Size", name="lns")
  rpanel::rp.textentry(mypanel, max_time, action=setMaxTime,              pos = list(row = maxnShow + 5,  column =3,columnspan = 2),
               initval= max_time, "Time of simulation", name="to")
  #View(mypanel$cohortSize)
  rpanel::rp.button(mypanel, action = guiSimCoh ,                         pos = list(row = maxnShow + 6,  column =3,columnspan = 2), "Simulate Cohort", name = "sc")
  rpanel::rp.textentry(mypanel, shiftXY, action=shiftWindowXY,            pos = list(row = maxnShow + 4,  column =1,columnspan = 4),labels =c("start rows","starts columns"),
               initval = c(1,1), name="shiftX")
  rpanel::rp.textentry(mypanel,addingState, action = addState,            pos = list(row = maxnShow+3, column = 7,columnspan = 2, rowspan=2),
               labels=c("state_name", "after"),initval = list(quote("\"new_state\""),n))   
  
  hfIndex <- array(rep("NULL", n*n), dim=c(n,n))
  parIndex <- array(rep("NULL", n*n), dim=c(n,n))
  
  ##################################################################################################
  #drawing combo boxes                                                                             #  
  #indexing - from startColumn and startRow to startColumn + nShow and startRow + nShow            #
  ##################################################################################################
 
  
  for (i in seq(startRow,tmpr,1)){
    iGui <- i - startRow + 1
    
    rpanel::rp.text(mypanel, mypanel$statesNames[i],function(mypanel){mypanel}, pos=list(row=iGui, column=0))
  }
  
  for (j in seq(startColumn,tmpc,1)){
    jGui <- j - startColumn + 1
    rpanel::rp.text(mypanel, mypanel$statesNames[j],function(mypanel){mypanel}, pos=list(row=0, column=jGui))
  }
  
  #View(tmpr)
  #View(mypanel$hfNames)
  for (i in seq(startRow, tmpr, 1)){
    for (j in seq(startColumn, tmpc, 1)){
      iGui <- i - startRow + 1
      jGui <- j - startColumn + 1
      hfIndex[i,j] <- paste("hf", toString(i) ,toString(j), sep="_")
      parIndex[i,j] <- paste("par", toString(i) ,toString(j), sep="_")
      grid_name <- paste("hf", toString(i), toString(j), sep="_")
      if (i<j){
        myImpossible <- paste("impossible", toString(i), toString(j), sep = "_")
        myExponential <- paste("Exponential", toString(i), toString(j), sep = "_")
        myWeibull <- paste("Weibull", toString(i), toString(j), sep = "_")
        myInitVal=rep(FALSE, FALSE,FALSE, FALSE< FALSE)
        if ((!is.null(mypanel$hfNames[i,j]))&& (!mypanel$hfNames[i,j]=="NULL")&& (!mypanel$hfNames[i,j]=="impossible")){
          hf_base_name <- (strsplit(mypanel$hfNames[i,j], "[_]"))  
          if (hf_base_name[[1]][1] =="hf"){
            myhfName <- mypanel$hfNames[i,j]
          } else {
            myhfName <- paste(mypanel$hfNames[i,j], toString(i), toString(j), sep = "_")
          }
          myvals <- c(myhfName,parIndex[i,j], myImpossible)   
        } else{
          myhfName <- hfIndex[i,j]
          myvals <- c(myImpossible, myExponential, myWeibull, myhfName,parIndex[i,j])
        }
        
        
        rpanel::rp.combo(mypanel, aa,  prompt=" ",
                 vals = myvals, action = open_hf, 
                 myInitVal, pos=list(row=iGui, column=jGui))
      }
      else {
        rpanel::rp.text(mypanel, "  NULL",function(mypanel){mypanel}, pos=list(row=iGui, column=jGui))
      }
    }
  }
  mypanel
} 
 



############ start Cost effectiveness ############################
#TODO: here or in cohort panel? rather in cohort. Or both??

# initialization of the cost structure
initCost <-function(numStates, statesNames= paste("State", 1:numStates) ){
  print(statesNames)
  print(numStates)
  def_units <- list(visit=0, test=0, medicine =0, fibroscan=0)  
  qualy <- as.list(rep(0, numStates))
  names(qualy) <- statesNames  
  stateCost <- as.list(rep(" ",numStates))#list("1" = "2*visit+1*medicine1 + 1*fibroscan", "2"="2*visit+1*medicine1 + 1*fibroscan")
  names(stateCost)<-statesNames
  #cost<-list(units="list",qualy="list", stateCost ="list")
  cost<-list("units"  = def_units, "qualy" = qualy, "stateCost"=stateCost)
}

# opens the editor to edit cost per unit (e.g. medicine, visit, fibroscan etc.)
editCostUnits <- function(mypanel){
  mynames <- names(mypanel$cost[["units"]])
  xx <- edit(t(as.data.frame((mypanel$cost[["units"]]))))
  print(xx)
  mypanel$cost[["units"]] <- as.list(xx)
  names(mypanel$cost[["units"]]) <- rownames(xx)
  mypanel
}

# compare shape of units and the rest -- they differ
editQualy <- function(mypanel){
  mynames <- names(mypanel$cost[["qualy"]])
  xx <- edit(t(as.data.frame((mypanel$cost[["qualy"]]))))
  mypanel$cost[["qualy"]] <- as.list(xx)
  names(mypanel$cost[["qualy"]]) <- mynames
  return(mypanel)
}


editStateCost <- function(mypanel){
  options("de.cellwidth"=20)
  mynames <- names(mypanel$cost[["stateCost"]])
  xx <- edit(t(as.data.frame((mypanel$cost[["stateCost"]]))))
  mypanel$cost[["stateCost"]] <- as.list(xx)
  names(mypanel$cost[["stateCost"]]) <- mynames
  options("de.cellwidth"=NULL)
  return(mypanel)
}

saveCost<-function(mypanel){
  fileName <- tcltk::tclvalue(tcltk::tkgetSaveFile(defaultextension=".RData",
                                     title="Save function...", filetypes="{R Data{.RData}} {{All Files} {*.*}}"))
  cost<- mypanel$cost
  cost_list <- list(cost)
  save(cost_list, file = fileName)
  return(mypanel)
}


loadCost<-function(mypanel){
  snFile <- tryCatch(file.choose(), error = function(e) "") 
  print(snFile)
 if (!snFile==""){ 
  cost_list<-eval(parse(text=load(snFile)))
  mypanel$cost <- cost_list[[1]]
  print(cost_list)
  print(mypanel$cost)
  }
else
 rpanel::rp.messagebox("Cost file not chosen", title="No cost file" )
  return(mypanel)
}


############ End cost effectiveness ########################

editBaselineFunction <- function(mypanel){
  if (is.na(mypanel$baselineFunction))
    mypanel$baselineFunction <- fixByString('new_baseline')
  mypanel$baselineFunction <- fixByString(mypanel$baselineFunction)
  fileName <- tcltk::tclvalue(tcltk::tkgetSaveFile(defaultextension=".R", filetypes="{{R scritpt {.R}} {{All Files} {*.*}}"))
  if (!fileName==""){ 
    saveFunctionFile(mypanel$baselineFunction, fileName)
  }
 
  mypanel
}

# Loading baseline function from the file
# @param mypanel, filename. If filename==NULL, filename from the dialog box
# @return mypanel
# 
loadBaselineFunction <- function(mypanel, fileName = NULL){
  #TODO: naming! bl, baseline, baselineFunction
  # loadBl for both bl function and bl as data drame??
  # or different functions?
  baselineFunction <- NULL
  baselineDf <- NULL
  cohortSize <- mypanel$cohortSize
  if (is.null(fileName))
    blFile <- tryCatch(file.choose(), error = function(e) "")
  
  else 
    blFile <- fileName
  
  if (!blFile==""){
  bl <- source(blFile) #fills in the hfNames matrix
  if (!is.null(bl[[1]])&(is.function(bl[[1]]))){ 
    mypanel$baseline <- bl[[1]](mypanel$cohortSize)
  mypanel$baselineFunction <- bl[[1]]
  }
  if ((!is.null(baselineDf))&(is.data.frame(baselineDf)))
    mypanel$baseline <- baselineDf
  }
  #otherwise - bl not changed. message box!
  return(mypanel)
}


# Source of functions from the directory chosen in a dialog box
# @param mypanel
# @return mypanel
# 
loadHf <- function(mypanel){
  # list of files in which we describe hazard functions. Or is it only one file? 
  #you can repeat sourcing different files to get all you want. ot should it be in a directory?
  mypanel$hfDir <-tcltk::tk_choose.dir(default = "./", caption = "Select directory with hazard functions")# file.choose()#(default = "./", caption = "Select a file with hazard functions")
  tmpwd <- getwd()
  setwd(mypanel$hfDir)
  list_hfFiles <-  list.files(mypanel$hfDir) 
  lapply(list_hfFiles, source)
  print(list_hfFiles)
  setwd(tmpwd)
  #source(list_hfFiles)
  mypanel
}



# open and edit a hazard function or parameters for a pair of states. From GUI level only
# @param mypanel TODO; split into smaller functions
# @return mypanel
# 
open_hf <- function(mypanel) {  
  hf_desc <- mypanel$aa
  hf_desc_list <- (strsplit(hf_desc, "[_]"))
  st1 <- as.integer(hf_desc_list[[1]][2])
  st2 <- as.integer(hf_desc_list[[1]][3])
  
  if (hf_desc_list[[1]][1] %in% c("impossible", "Exponential", "Weibull")){
    mypanel$M[[st1, st2]] <- hf_desc_list[[1]][1]
    mypanel$hfNames[st1, st2] <-hf_desc_list[[1]][1]  
    return(mypanel)
  }
  
  
  
 if (hf_desc_list[[1]][1] =="hf"){
    # even if the function hf_st1_st2 is already defined, it is ok.
    # we are fixing the existing function, hfNames is ok too.
    # mypanel$hfNames[as.integer(st1),as.integer(st2)] <- "impossible"
    mypanel$hfNames[st1, st2] <- paste("hf", st1, st2, sep="_") 
    
    mypanel$M[[st1, st2]] <- fixByString(mypanel$hfNames[st1,st2])
    functionName <- paste(mypanel$hfNames[st1, st2], ".R", sep="")
    fileName <- tcltk::tclvalue(tcltk::tkgetSaveFile(defaultextension=".R", filetypes="{{R scritpt {.R}} {{All Files} {*.*}}"))
    if (!fileName==""){ 
      saveFunctionFile(mypanel$hfNames[st1, st2], fileName)
    }
   
    return(mypanel)
  }
  
  if (hf_desc_list[[1]][1] =="par"){
    #!!!!!!!!! TODO: do not remove good parametrs
    tmp_match <- consistentParamsFunction(mypanel$params[[st1,st2]], mypanel$hfNames[st1,st2])
    if (!tmp_match){
      #shall we ask if we want to change the parameter automatically
      print("changing covariance")
      nn <- length(names(my_formals(mypanel$hfNames[[st1, st2]])))
      mypanel$params[[st1,st2]] <- as.list(rep(0, nn))# remove t, bl  , history
      print("do the params exist")
      names(mypanel$params[[st1,st2]]) <- names(my_formals(mypanel$hfNames[[st1, st2]]))
      mypanel$params[[st1, st2]]["t"] <- NULL
      mypanel$params[[st1, st2]]["bl"] <- NULL
      mypanel$params[[st1, st2]]["history"] <- NULL
      nn <- length(as.list(mypanel$params[[st1, st2]]))
      #print(mypanel$covariance@list.matrix)
      mypanel$covariance[[st1,st2]] <- array(rep(0, nn*nn), dim=c(nn,nn))
    }
    if (length(mypanel$params[[st1,st2]])>0){
      #TODO: covariance only if np >1
      # paramsCov <- list(mypanel$params[[st1, st1]], "covariance"=mypanel$covariance[[st1, st2]])  
      np <- length(as.list(mypanel$params[[st1, st2]]))
      paramsCov <- matrix(rep(0, np*(np+1)), np, np+1)
      colnames(paramsCov) <- c("parameter value",rep("covariance",np))
      rownames(paramsCov) <- names(mypanel$params[[st1,st2]])
      parNames <- names(mypanel$params[[st1,st2]])
      for (f in 1:np){
        paramsCov[f,1] <- mypanel$params[[st1,st2]][[f]]
      }
      paramsCov[1:np, 2:(np+1)] <- as.matrix(mypanel$covariance[[st1,st2]])
      paramsCov <- edit(as.data.frame(paramsCov))
      parmasCov <- paramsCov[!is.na(paramsCov)]
      ll <- which(is.na(paramsCov))
      np <- dim(paramsCov)[1]
      mypanel$params[[st1,st2]] <- as.list(paramsCov[1:np,1])#tmp_params #paramsCov[names(mypanel$params[[st1,st2]][[f]]),1]   
      #print((mypanel$params[[st1,st2]]))
      names(mypanel$params[[st1,st2]]) <- rownames(paramsCov)
      if (np>1){
        mypanel$covariance[[st1,st2]] <- as.matrix(paramsCov[1:np, 2:(np+1)])
      }
      else {
        mypanel$covariance[[st1,st2]] <-0 
      }
    }
    
    if (length(mypanel$params[[st1,st2]]) ==0) {
      print("got here?")
      # if(mypanel$hfNames[st1, st2] %in% c("impossible"))
      defPar <- edit(list())
    }
    return(mypanel)
  } # end of if par
  
  if (mypanel$hfNames[st1,st2] %in% c("Weibull", "Exponential", "impossible"))
    mypanel$hfNames[st1,st2] <- edit(mypanel$hfNames[st1,st2])
  else {
    print(mypanel$hfNames[st1,st2])
    fixByString(mypanel$hfNames[st1,st2])
    tmpTitle <-paste("Do you want to save the file " , mypanel$hfNames[st1, st2], " ?", sep ="") 
    functionName <- paste(mypanel$hfNames[st1, st2], ".R", sep="")
    fileName <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile=functionName, defaultextension=".R",
                                       title="Save function..."))
    if (!nchar(fileName)) {
      tcltk::tkmessageBox(message = "Function not saved!") 
    } else {
      saveFunctionFile(mypanel$hfNames[st1, st2], fileName)
     # tkmessageBox(message = paste("Function ", functionName, " saved as ", fileName)) 
    }
  }
  mypanel
}

# an example of baseline function
# @param cohortSize
# 
baselineFunction <- function(cohortSize) {
  bl <-data.frame(sex=rbinom(cohortSize, 1, .5), age=runif(cohortSize, 10,60))
  return(bl)
}

# shows different part of the hfFunctions matrix
#  @param mypanel 
#  @keywords shiftWindowsXY
shiftWindowXY <- function(mypanel){
  mypanel$startRow <- max(1, min(as.numeric(mypanel$shiftXY[1]), mypanel$numStates-nShow + 1))
  mypanel$startColumn <- max(1, min(as.numeric(mypanel$shiftXY[2]), mypanel$numStates-nShow + 1))
  drawTable(mypanel)
  mypanel 
}


ViewSubcohort <- function(mypanel, subcohorts_nums = -1){
  if (subcohorts_nums ==-1)
    subcohorts_nums <- eval(parse(text=mypanel$subcohortToView))
  print(subcohorts_nums)
  print(mypanel$subcohorts)
  for (f in subcohorts_nums){
    View(as.data.frame(mypanel$subcohorts[f]))
  }
  mypanel
}


ViewCost <- function(mypanel, costs_nums = -1){
  if (costs_nums ==-1)
    costs_nums <- eval(parse(text=mypanel$costToView))
  print(costs_nums)
  print(mypanel$costs)
  for (f in costs_nums){
    View(as.data.frame(mypanel$costs[[f]]))
  }
  mypanel
}

 
countCostMypanel<- function(mypanel){
  cost_list <- countCost(mypanel$cohort, mypanel$cost)
  print(cost_list)
  #TODO: what do I want to show? global cost, global qualy?
  # at which level of the disease the cost is highest?
  # percentage of different elements in cost?
  #TODO: plot. with one cohort only numbers, for many cohorts important comparison. And the optimum line.
mypanel
}
   
updateCohort <- function(mypanel){
  print(mypanel$cohort@baselineFunction)
  newCohort <- update(mypanel$cohort, as.integer(mypanel$cohortSize))
  mypanel$cohort <- newCohort #TODO: check if newCohort ok.
  mypanel$max_time <- newCohort@follow.up
  mypanel <- updateSubcohorts(mypanel)
  mypanel
}


showCohort <- function(mypanel){
  # for R cmd check only
  cohortSize_input <- NULL
  query <- NULL
  subcohortToView <- NULL
  functionToPlot <- NULL
  StatesToPlot <- NULL
  statesToMerge <- NULL
  # end of for R cmd check only
  
  mypanel$cohPanel <- rpanel::rp.control(cohort = mypanel$cohort, subcohorts=mypanel$subcohorts, numStates=mypanel$numStates,
                                 statesNames = mypanel$statesNames, hfNames = mypanel$hfNames, M = mypanel$M,
                                 cost = initCost(mypanel$numStates, mypanel$statesNames))  
  head(mypanel$cohort)
  rpanel::rp.button(mypanel$cohPanel, editCohort, "Edit Cohort")
  #rpanel::rp.textentry(mypanel$cohPanel, cohortSize_input, updateCohort, "Update Cohort", initval = mypanel$cohort@size, width=80)
  rpanel::rp.button(mypanel$cohPanel, listSubcohorts, "List subcohorts")
  rpanel::rp.button(mypanel$cohPanel, listStates, "List states")
  rpanel::rp.button(mypanel$cohPanel, saveModel, "Save the model" )
  rpanel::rp.button(mypanel$cohPanel, plotPaths, "Plot paths" )
  rpanel::rp.button(mypanel$cohPanel, plotPath_next, "Plot next paths")
  rpanel::rp.textentry(mypanel$cohPanel, query, chooseSubcohort, 
               initval =substitute("(cohortBl[\"sex\"] ==1) & (cohortBl[\"age\"]>40)"),
               width = 80, "Add subcohort, eg. ")
  #  subcohorts_vals <- 
  rpanel::rp.textentry(mypanel$cohPanel,subcohortToView, action=ViewSubcohort, initval = c(1), "View subcohort")  
  rpanel::rp.combo(mypanel$cohPanel,functionToPlot, prompt = "", vals=c("transitionProbabilities", "cumulativeIncidence"))#,
  rpanel::rp.textentry(mypanel$cohPanel,StatesToPlot, labels=c("states", "subcohorts"),
               initval =c("\"c(1:4)\"", "\"c(1,2)\""),title = "Plot transition functions")
  rpanel::rp.button(mypanel$cohPanel,plotFun, "Plot")          
  rpanel::rp.textentry(mypanel$cohPanel,statesToMerge, action = mergeStates_mypanel,labels=c("states goups"),
               initval ="list(list(1,2), list(3,4))" ,title = "Merge states")
  print( mypanel$cohPanel$cost)
  print(t(as.data.frame((mypanel$cohPanel$cost[["units"]]))))
  rpanel::rp.button(mypanel$cohPanel, action=editCostUnits, "Edit cost per unit",  name="editCostUnits")
  rpanel::rp.button(mypanel$cohPanel, action=editQualy, "Edit QUALY per state",    name="editQUALY")
  rpanel::rp.button(mypanel$cohPanel, action=editStateCost, "Edit cost per state", name="editStateCost")
  rpanel::rp.button(mypanel$cohPanel, action=saveCost, "Save cost",                name="saveCost")
  rpanel::rp.button(mypanel$cohPanel, action=loadCost, "Load cost",                name="loadCost")
  rpanel::rp.button(mypanel$cohPanel, action=countCostMypanel, "Count cost",       name="Count Cost")
  mypanel
}

##########################################################################
  
#' graphHf
#' @description
#' Plots the possible progression paths in the stochastic model based on the transition matrix. 
#' The endges in the graph exist, if there is a transition function between given two states. 
#' @usage
#' graphHf(hfNames, statesNames = rownames(hfNames))
#' @param hfNames  a \code{matrix} with names of hazard functions;
#' @param statesNames  a \code{list} of names of the states. By default rows names of HfNames 
#' @keywords 
#' graphHF, graph, progression paths
#' @return ghf - a \code{graph} with statesNames as nodes. There are egdes between states, 
#' where the hazard function is not "impossible" nor "NULL"   
#' 
#' @examples
#' 
#' hfNames <- array(rep("Exponential", 36), dim = c(6,6))
#' hfNames[col(hfNames)<=row(hfNames)]<-"NULL"
#' hfNames[3,4:5] <-  rep("impossible",2)
#' graphHf(hfNames)
#' 
#' @export
#
graphHf <- function(hfNames, statesNames=rownames(hfNames)){
  if (is.null(statesNames))
    statesNames <- list()
    for (f in c(1:nrow(hfNames))){
      statesNames[[f]] <- paste("state", f, sep = "_")
    }
  #statesNames <- list(paste("state", 1:ncol(hfNames), sep="_"))
  adj <- as.integer(!hfNames %in% c("NULL", "impossible"))
  adjsq <- array(adj, dim=c(nrow(hfNames), nrow(hfNames)))
  ghf<-igraph::graph.adjacency(adjsq,  mode="directed")
  print(length(statesNames))
  igraph::V(ghf)$label <- statesNames  
 
  lay <- igraph::layout.reingold.tilford(ghf, circular=T)
  print(plot(ghf, layout= lay, vertex.size=30, axes=FALSE))
  #layout.fruchterman.reingold, vertex.size=30))
  return(ghf)
}

graphHf_mypanel <- function(mypanel){
  # plot graph of transitions between states
  ghf <- graphHf(mypanel$hfNames)
  print(igraph::V(ghf)$label)
  print(mypanel$statesNames)
  #g <- set.vertex.attribute(g, "color", value=c("red", "green"))
  mypanel$ghf <- ghf
  #  lay <- layout.sugiyama (ghf, layers = NULL, hgap = 1, vgap = 1, maxiter = 100,
  #                         weights = NULL, attributes = c("default", "all", "none"))
  mypanel 
}


statesPaths <- function(mypanel){
  sortedPaths <- statesPaths_cohort(mypanel$cohort)
  mypanel$sortedPaths <- sortedPaths
  cohort <- mypanel$cohort
  cohortBl <-  as.data.frame(c(as.data.frame(cohort@baseline), 
                               as.data.frame(cohort@time.to.state)))
  
  for (g in c(1:nrow(sortedPaths))){ 
    ind <- rep(TRUE, nrow(cohort@time.to.state))
    for (f in c(1:ncol(cohort@time.to.state))){
      ind <- ind & (is.na(cohort@time.to.state[,f])==is.na(sortedPaths[g, f]))  
    } 
    mypanel$subcohorts[[paste("path_", g, sep="")]] <- as.data.frame(cohortBl[ind,])
  }
  ############ end of adding path subcohorts
  
  View(mypanel$sortedPaths)
  mypanel
}

# @description
# Returns the list of progression paths in a cohort from the most frequent to the least frequent.
# 
# @usage
# statesPaths_cohort(cohort)
# @param 
# \code{cohort} - simulated by \emph{simulateCohort()}
# 
# 
# 
# @keywords paths, plotPaths, statesPaths_cohort,
# 
# @examples
# \code{
# statesNumber <- 3 
# cohortSize <- 100
# hazardf <- generateHazardMatrix(statesNumber)
# hazardf[[1,2]] <- function(t, r1, r2) 
# {
#   ifelse(t<=2, r1 , r2)
# }
# hazardf[[2,3]] <- "Weibull" 
# 
# mu <- generateParameterMatrix(hazardf) 
# mu[[1,2]] <- list(0.33,  0.03) # r1, r2 
# mu[[2,3]] <- list(1,0.84) # shape, scale
# maxTime <- 10
# 
# cohort <- simulateCohort(
#   transitionFunctions = hazardf,
#   parameters = mu,
#   cohortSize = cohortSize,
#   to=maxTime)
# 
# statesPaths_cohort(cohort)
# }
# @export
# 
statesPaths_cohort <- function(cohort){
  # for R cmd check
  freq <- NULL
  # end for R cmd check
  
  paths <- cohort@time.to.state
  paths[!is.na(paths)]<-1
  agg_paths <- as.data.frame(plyr::count(paths)) ## or fcount??? debug
  sortedPaths <- plyr::arrange(agg_paths, freq*(-1))
  return(sortedPaths)
}


# plots the Vpath in the graph g 
color_path<-function(g,Vpath,w=5){
  igraph::E(g)$color<-"black"
  igraph::V(g)$color<-"rosybrown1"
  #V(g)$shape<-"rectangle"
  lg <- length(igraph::V(g))
  lp <- length(Vpath)
  igraph::V(g)[Vpath[lp]]$color<-"red"
  print("random print")
  print(Vpath)
  print(lp)
  igraph::E(g)$width <- 1
  if (lp > 1){
    for (f in seq(1:(lp-1))){
      low <- Vpath[f]
      up <- Vpath[f+1]
      #  num <- (low-1)*((lg-1)+(lg-low+1))/2+up-low
      #  print(num)
      print(low)
      print(up)
      igraph::E(g)[ (igraph::`%--%`(up, low))]$color<-"red"
      igraph::E(g)[ (igraph::`%--%`(up, low))]$width<-w
      igraph::V(g)[Vpath[f]]$color<-"red"
    }}
  lay <- igraph::layout.reingold.tilford(g, circular=T)
  plot(g, layout = lay, axes=FALSE, vertex.size=30)#, vertex.size=30)
}

# plots the most frequent path in the cohort in the cohort in mypanel
plotPaths <- function(mypanel){
  print("plotPaths")
  mypanel <- statesPaths(mypanel)
  mypanel$numPath <- 1
  paths <- mypanel$sortedPaths #must be computed before in statesPaths_cohort
  View(paths)
  n <- ncol(paths)-1
  print("error in graphHf")
  mypanel<-graphHf_mypanel(mypanel)
  ghf<-mypanel$ghf 
  
  # for (f in (c(1:nrow(paths)))){
  f=1
  paths[f,1:n] <-paths[f,1:n]*c(1:n)
  pathToPlot <- paths[f, 1:n]
  pathToPlot <- pathToPlot[!is.na(pathToPlot)]
  color_path(mypanel$ghf,pathToPlot)
  #answer <- readline(prompt="Press \"q\" to escape. Press any other key to see next path. ")
  #if (answer == "q")
  return(mypanel)
}

#' plotProgressionPath
#' @description
#' Plots f-th most frequent progression path in the cohort
#' 
#' @usage
#' plotProgressionPath(cohort, hf, f)
#' 
#' @param cohort  simulated in simulateCohort()
#' @param hf      names of the transition functions between states in the cohort.  
#' @param f       number of the path to be plotted
#' 
#' @examples
#' hfNames <- array(rep("Exponential", 9), dim = c(3,3))
#' hfNames[col(hfNames)<=row(hfNames)]<-"NULL"
#' colnames(hfNames) <- as.list(paste("state", 1:3))
#' rownames(hfNames) <- as.list(paste("state", 1:3))
#' 
#' M <- makeM(hfNames)
#' par <- generateParameterMatrix(M)
#' par[[1,2]] <- list(rate = 1)
#' par[[1,3]] <- list(rate = 2)
#' par[[2,3]] <- list(rate = 0.5 )
#' 
#' cohort <- simulateCohort(
#' transitionFunctions = M,
#' parameters = par,
#' cohortSize = 100,
#' to=10)
#'
#' plotProgressionPath(cohort, hfNames, 2)
#' @export
plotProgressionPath <- function(cohort, hf, f){ 
  sortedPaths <- statesPaths_cohort(cohort)
  ghf <- graphHf(hf)
  plotPath_n(sortedPaths, ghf, f)

}

# @name plotPath_n
# @description 
# Plots n-th most frequent progression path in the cohort
plotPath_n <- function(paths, ghf, f){
  print("plotPath_n")
  n <- ncol(as.data.frame(paths))-1
  print(n)
  print("n printed above")
  if (f > (nrow(paths))){
    ghf <- ghf
    #lay <- layout.sugiyama (ghf, layers = NULL, hgap = 1, vgap = 1, maxiter = 100,
    #                       weights = NULL, attributes = c("default", "all", "none"))
    lay <- igraph::layout.reingold.tilford(ghf, circular=T)
    print(lay)
    print(plot(ghf, layout= lay, vertex.size=30, axes=FALSE))#layout.fruchterman.reingold, vertex.size=30))
    return
  }
  print(paths)
  paths[f,1:n] <-paths[f,1:n]*c(1:n)
  pathToPlot <- paths[f, 1:n]
  print(pathToPlot)
  #  pathToPlot <- colnames(paths)[!is.na(pathToPlot)]
  pathToPlot1 <- pathToPlot[!is.na(pathToPlot)]
  weight <- max(1,5*paths[f,n+1]/paths[1,n+1])
  print("main plotting")  
  print(pathToPlot1)
  color_path(ghf,pathToPlot1,weight)
}



plotPath_next <- function(mypanel){
  print("plotPath_next")
  n<-mypanel$numStates
  mypanel$numPath <- mypanel$numPath + 1
  f <- mypanel$numPath
  paths <- mypanel$sortedPaths 
  plotPath_n(paths, mypanel$ghf, f)
  return(mypanel)
}

# Prepares data frame for plotting 
# 
prepare_ggplot <- function(post, states_to_plot, mypanel ){
  n<-length(post)# check it!
  k<-length(states_to_plot)
  #checking, if required states exist
  if ("time.to.state" %in% names(attributes(mypanel$cohort))){ 
    statesNames <- names(mypanel$cohort@time.to.state)
}  else{ 
    if(!is.null(mypanel$cohorts)) {
      statesNames <- names(mypanel$cohorts[[1]]@time.to.state)}
  }
  check <- tryCatch(statesNames[states_to_plot], 
                    error=function(error){NULL})
  if (is.null(check)){
    rpanel::rp.messagebox("Wrong States, choose different ones", title = "Wrong States")
    return(NULL)  #check for -1 when trying to plot
  }
  subcohorts_names <- names(post)
  postProb <-matrix(rep(list(), n*k), k,n)
  postUpper <- matrix(rep(list(), n*k), k,n)
  postLower <-matrix(rep(list(), n*k), k,n)
  postTimes<-list()
  postTimes <- post[[1]]@times
  for (g in seq(1:n)){
    for (f in seq(1:k)){
      tryCatch(postProb[[f,g]] <- post[[g]]@probabilities[,states_to_plot[f]], error=function(error){NULL})
      if (is.null(postProb[[f,g]])) {return(mypanel)}
      postUpper[[f,g]] <- post[[g]]@upper[,states_to_plot[f]]
      postLower[[f,g]] <- post[[g]]@lower[,states_to_plot[f]]
    }
  }
  
  
  hh <- length(postTimes)
  df<- data.frame(time=double(), subcohort = integer(),
                  state = character(), 
                  probability = double(), 
                  upper = double(),
                  lower = double())
  
  
  for (f in c(1:k)){
    for (g in c(1:n)){
      df_tmp <- (data.frame("time"=as.data.frame(postTimes)))#, "subcohort"="NULL", 
      colnames(df_tmp) <- c("time")
      name_cohort <- subcohorts_names[g] # paste("subcohort",g, sep = " ")#subcohortsName[g])
      print(statesNames)
      name_state <-  (statesNames[states_to_plot[f]])#paste("state",states_to_plot[f], sep = " ") ##statesName[g])
      print(name_state)
      #TODO: name of subcohort instead of ther number only
      df_tmp["subcohort"] <-as.data.frame(rep(name_cohort, hh))  
      df_tmp["state"] <-as.data.frame(rep(name_state, hh)) 
      df_tmp["probability"] <-(as.data.frame(postProb[[f,g]])) 
      df_tmp["upper"] <-(as.data.frame(postUpper[[f,g]]))
      df_tmp["lower"] <-(as.data.frame(postLower[[f,g]]))
      df <- rbind(df, df_tmp)
    }}
  return(df)
}


#' mergeStates
#' @description
#' Merges the states given by the user. 
#' The names of merged states are concatenations of names of the states being merged.
#' \code{merged_cohort\@ time.to.state} is defined as time, when the patient arrives 
#' in the first state which was merged into the new state.
#' The function creates also a \code{hfNames} \code{matrix} for the merged states. They are used only to plot a graph of possible 
#' transmissions between merged states.  
#' @param cohort a \code{cohort} simulated by \code{simulateCohort()} 
#' @param statesGroups a \code{list of lists} of states to be merged.
#' e.g. \code{list(list(1,2,3),list(5,6))}
#' 
#' @return list(cohortGroup, hfNames)
#' \code{cohortGroup} is an object with a structure like a \code{cohort} simulated by \code{simulateCohort}. States names
#' are now concatentations of the names of the states which were merged. Times of transitions 
#' (cohortGroup (at) time.to.state) are recomputed for groups of merged states. The other attributes of the cohorts
#'  stay unchanged. 
#' \code{hfNames} is a \code{matrix} with row names beeing concatenated names of the states that were merged, the values in the matrix are 
#' "impossible", if the transition between given groups of states is impossible, or "possible", if there is some possible transition between 
#' the states in the given two groups of states.
#' 
#' @keywords mergeStates, merge
#' 
#' @seealso
#' simulateCohort
#' 
#' @examples
#' 
#' hfNames <- array(rep("Exponential", 16), dim = c(4,4))
#' hfNames[col(hfNames) <= row(hfNames)]<-"NULL"
#' rownames(hfNames) <- as.list(paste("state", 1:4))
#' colnames(hfNames) <- as.list(paste("state", 1:4))
#' M <- makeM(hfNames)
#' param <- generateParameterMatrix(M)
#' param[[1,2]] <- list(rate = 1)
#' param[[1,3]] <- list(rate = 2)
#' param[[2,3]] <- list(rate = 0.5)
#' 
#' param[[1,4]] <- list(rate = 1)
#' param[[2,4]] <- list(rate = 2)
#' param[[3,4]] <- list(rate = 0.5)
#' 
#' cohort <- simulateCohort(
#' transitionFunctions = M,
#' parameters = param,
#' cohortSize = 100,
#' to=10)
#'  
#' tmp <- mergeStates(cohort, statesGroup = list(list(1,2), list(3,4)))
#' merged_cohort <- tmp[["cohort"]]
#' merged_hfNames <- tmp[["hfNames"]]
#' 
#' @export
mergeStates <- function(cohort, statesGroups){ 
  tts <- cohort@time.to.state 
  singleStates <- setdiff(c(1:cohort@states.number),
                          unlist(statesGroups)) 
  stringSingleStates <- lapply(singleStates, toString)
  stringStatesGroups <- lapply(statesGroups, toString)
  allStates <- c(singleStates, statesGroups)
  stringAllStates <- c(stringSingleStates,stringStatesGroups)
  
  oas <- order(unlist(stringAllStates))
  sas <- stringAllStates[oas]
  allStates <- allStates[oas]
  
  statesNumberGroup <- cohort@states.number + length(statesGroups) - length(unlist(statesGroups))
 
  M <- cohort@transitionFunctions
  M_new <- generateHazardMatrix(length(allStates))
  M_Group <- matrix("NULL", statesNumberGroup, statesNumberGroup)

  for (f in c(1:nrow(M_Group))){
    for (g in c(1:ncol(M_Group))){
      M_Group[f,g] <- any(("impossible"!=M@list.matrix[unlist(allStates[[f]]),
                                                       unlist(allStates[[g]])])
                          &("NULL"!=M@list.matrix[unlist(allStates[[f]]), 
                                                  unlist(allStates[[g]])]))    
    }
  }
    
  hfNames <-  matrix("NULL", statesNumberGroup, statesNumberGroup)
  for (f in c(1:nrow(M_Group))){
    for (g in c(1:ncol(M_Group))){
      hfNames[f,g] <- any(("impossible"!=M@list.matrix[unlist(allStates[[f]]), 
                                                       unlist(allStates[[g]])])
                          &("NULL"!=M@list.matrix[unlist(allStates[[f]]), 
                                                  unlist(allStates[[g]])]))    
    }
  }
  
  if (any(hfNames[col(hfNames)<row(hfNames)]==TRUE))
    rpanel::rp.messagebox( "Merging these states results in loops.
                   Close the window and choose another subset of states.", 
                   title="loop in merged states")
  
  hfNames[hfNames == TRUE] <- "possible"
  hfNames[hfNames == FALSE] <- "impossible"
  statesNames <- list()
  statesNames_old <- names(cohort@time.to.state)
  #looking for the times of the entry into the new merged states
  for (f in c(1:length(allStates))){
      tts[is.na(tts)] <- 10000
    tts[,f] <-apply(as.data.frame(tts[,unlist(allStates[[f]])]), 1, min)
    tts[tts==10000] <- NA
    statesNames[f] <- toString(paste(statesNames_old[unlist(allStates[[f]])], sep="_"))
  }
  tts <- tts[,1:statesNumberGroup]
  colnames(tts) <- statesNames
  dimnames(M_Group) <- list(from=statesNames, to=statesNames)

  #edit(tts)
  cohortGroup <- cohort
  
  #View(as.data.frame(tts))
  cohortGroup@time.to.state <- as.data.frame(tts)
  
  M_new@list.matrix <- M_Group
  colnames(cohortGroup@time.to.state) <-  colnames(tts)
  cohortGroup@transitionFunctions <-M_new
  colnames(hfNames) <- statesNames
  rownames(hfNames) <- statesNames
  return(list(cohortGroup = cohortGroup,  
              hfNames = hfNames))
}



mergeStates_mypanel <- function(mypanel, statesGroups=NULL){
  query <- NULL
  functionToPlot <- NULL
  StatesToPlot <- NULL
  subcohortToView <- NULL
  
  if (is.null(statesGroups))
    statesGroups <- eval(parse(text=mypanel$statesToMerge)) 
  print(statesGroups)
  
  merge_list <- mergeStates(mypanel$cohort, statesGroups)
  M <- merge_list[["cohortGroup"]]@transitionFunctions
  cohortGroup <- merge_list[["cohortGroup"]]
  tts <- cohortGroup@time.to.state
  hfNames <- merge_list[["hfNames"]]
  statesNumberGroup <- cohortGroup@states.number
  statesNames <- names(cohortGroup@time.to.state)
  subcohorts <- list()
  
  subcohorts[["the main cohort"]] <- data.frame(as.data.frame(cohortGroup@baseline), 
                                                as.data.frame(tts))
  print(statesNames)
  mypanel$mergePanel <- rpanel::rp.control(cohort = cohortGroup, subcohorts = subcohorts,statesNames=statesNames,
                                   numStates=statesNumberGroup, M=M, hfNames = hfNames)
  
  print(mypanel$mergePanel)
  rpanel::rp.button(mypanel$mergePanel, editCohort, "Edit Cohort")
  rpanel::rp.button(mypanel$mergePanel, listSubcohorts, "List subcohorts")
  rpanel::rp.button(mypanel$mergePanel, listStates, "List states")
  rpanel::rp.button(mypanel$mergePanel, plotPaths, "Plot paths")
  rpanel::rp.button(mypanel$mergePanel, plotPath_next, "Plot next paths")
  rpanel::rp.textentry(mypanel$mergePanel, query, chooseSubcohort, 
               initval =substitute("(cohortBl[\"sex\"] ==1) & (cohortBl[\"age\"]>40)"),
               width = 80, "Add subcohort, eg. ")
  rpanel::rp.textentry(mypanel$mergePanel,subcohortToView, action=ViewSubcohort, initval = c(1), "View subcohort")  
  rpanel::rp.combo(mypanel$mergePanel,functionToPlot, prompt = "", 
           vals=c("transitionProbabilities", "cumulativeIncidence"))#,
  rpanel::rp.textentry(mypanel$mergePanel,StatesToPlot, labels=c("states", "subcohorts"),
               initval =c("\"c(1:4)\"", "\"c(1,2)\""),title = "Plot transition functions")
  rpanel::rp.button(mypanel$mergePanel,plotFun, "Plot")          
  
  return(mypanel)
}


##########################################################################

editCohort <- function(mypanel){
  cohort <- mypanel$cohort
  bl <- cohort@baseline
  typeof(bl)
  typeof(cohort@baseline)
  typeof(cohort@time.to.state)
  cohortBl <- as.data.frame(c(as.data.frame(bl), as.data.frame(cohort@time.to.state)))
  edit(cohortBl)
  mypanel
}
