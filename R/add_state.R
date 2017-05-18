
#' addStateMatrix
#' @description
#' 
#' Adds a new state to a transitions functions matrix. 
#' @usage
#' addStateMatrix(newMatrix, oldMatrix,
#'                stateName=paste("state", after+1, sep=" "),
#'                after=dim(oldMatrix)[1], newCell="NULL")
#' @param newMatrix an empty matrix with one more row and one more column than the old
#' @param oldMatrix a matrix to which we are adding a new state; 
#' @param stateName the name of the state to be added; 
#' @param after the number of the state after which a new state should be added
#' @param newCell the value in the state to be added;
#' @return \code{newMatrix}
#' a matrix created by adding a new state with the value \code{newCell} 
#' to the old matrix
#' @examples
#' M <- generateHazardMatrix(5)
#' largeM <- generateHazardMatrix(6)
#' oldM <- M@@list.matrix
#' newM <- largeM@@list.matrix
#' M_addedState <- addStateMatrix(newM, oldM, stateName ="newState", after =3, newCell=0)
#' @export
addStateMatrix<- function(newMatrix, oldMatrix, stateName=paste("state", after+1, sep=" "),
                          after=dim(oldMatrix)[1], newCell="NULL"){
  
  n <-dim(oldMatrix)[1]
  after <- min(after, n)
  newNames <- append(rownames(oldMatrix), stateName, after=after)
  rownames(newMatrix) <- newNames
  colnames(newMatrix) <- newNames
  newMatrix[1:after, 1:after] <- oldMatrix[1:after, 1:after]
  
  if (after < n){
    newMatrix[1:after, (after+2):(n+1)] <- oldMatrix[1:after, (after+1):n]
    newMatrix[(after+2):(n+1), 1:after] <- oldMatrix[(after+1):n, 1:after]
    newMatrix[(after+2):(n+1), (after+2):(n+1)]<- oldMatrix[(after+1):n, (after+1):n]
    newMatrix[(after+2):(n+1),(after+2):(n+1) ]<- oldMatrix[(after+1):n,(after+1):n] 
  }
newMatrix
}


addStateHfNames <-  function(mypanel, state_name=paste("state", after+1, sep=" "), 
                             after=mypanel$numStates, newCell="impossible"){
    #TODO: when NULL, when impossible
 
  print("can we start?")
  n <- mypanel$numStates-1 
  hfNew <- initHfNames(n+1)
  dimnames(mypanel$hfNames) <- dimnames((mypanel$M)@list.matrix)
  states_names_from <- (dimnames((mypanel$M)@list.matrix))["from"][[1]]
  states_names_to <- (dimnames((mypanel$M)@list.matrix))["to"][[1]]
  hfNew <- addStateMatrix(hfNew, mypanel$hfNames, state_name, after, newCell)
  dimnames(hfNew) <- list(from= append(states_names_from, state_name, after), 
                          to= append(states_names_to, state_name, after))
  mypanel$hfNames <- hfNew 
  View(mypanel$hfNames)
  mypanel
}
  

addStateM <- function(mypanel, state_name=paste("state", after+1, sep=" "), after=mypanel$numStates){
  n <- mypanel$numStates-1
  after <- min(after, n)
  MNew <- generateHazardMatrix(n+1)
  dimnames(MNew@list.matrix) <- dimnames(mypanel$hfNames)
  MNew_list <- MNew@list.matrix
  MNew_list <- addStateMatrix(MNew_list, (mypanel$M)@list.matrix, state_name, after, "impossible")
  MNew@list.matrix <- MNew_list
  mypanel$M<-MNew
  mypanel
}


addStatePar <- function(mypanel,state_name, after=mypanel$numStates, newCell=list()){ #states names are already redefined
  n <- mypanel$numStates-1
  after <- min(after, n)
  View(after)
  parameterNew <-generateParameterMatrix(mypanel$M)
  parameterNew_list <- parameterNew@list.matrix
#  parameterNew@list.matrix[col(parameterNew@list.matrix)>row(parameterNew@list.matrix)] <-list()
  parameterNew_list <- addStateMatrix(parameterNew_list, (mypanel$params)@list.matrix, state_name, after, newCell)
  parameterNew@list.matrix <- parameterNew_list
  mypanel$params<-parameterNew
  mypanel
}


addState.ttt <- function(mypanel, ttt,state_name, after=mypanel$numStates, newCell=FALSE){ #states names are already redefined
  n <- mypanel$numStates-1
  ttt_new <-matrix(FALSE, nrow=n+1, ncol=n+1) 
  after <- min(after, n)

  ttt_new <- addStateMatrix(ttt_new, mypanel$ttt, state_name, after, newCell)
  dimnames(ttt_new) <- dimnames((mypanel$M)@list.matrix)
  mypanel$ttt <- ttt_new
  mypanel
}



addStateCov <- function(mypanel, state_name,
                        after=mypanel$numStates){ #states names are already redefined
  n <- mypanel$numStates-1
  after <- min(after, n) 
  covNew <-generateParameterCovarianceMatrix(mypanel$parameters)
  covNew_list <- covNew@list.matrix
  covNew_list <- addStateMatrix(covNew_list, (mypanel$covariance)@list.matrix, state_name, after )
  covNew@list.matrix <- covNew_list
  View(covNew@list.matrix)
  mypanel$covariance <- covNew
  mypanel
}

# addStateMypanel
# 
# Adds a new state to all matrices in mypanel (hfNames, M, params, covariance, ttt, StatesNames).
# From GUI level only or a function called 
# from an exported function  
# @param 
# mypanel - a structure, which has attributes:
# numStates, statesNames, M, params, HfNames, ttt, covariance 
# state_name - name of the state to be added
# after_state - where to place the new state
# @keywords addState
addStateMypanel <- function(mypanel, state_name, after_state){
 mypanel$numStates <- mypanel$numStates + 1 
 mypanel$statesNames <- append(dimnames((mypanel$M)@list.matrix)["from"][[1]], state_name, after_state)
 print(mypanel$statesNames)
 mypanel <- addStateHfNames(mypanel, state_name, after_state)
 View(mypanel$hfNames)
 mypanel<- addStateM(mypanel, state_name, after_state)
 mypanel <- addStatePar(mypanel, state_name, after_state)
 print(mypanel$M)#)
 mypanel <- addState.ttt(mypanel,state_name, after_state)
 
 if (exists("mypanel$covariance")){ 
   print("covariance defined??")
#  mypanel <- addStateCov(mypanel, state_name, after_state)
 }
 print("all states added")
 drawTable(mypanel)
 mypanel
}

# add new state to the model. 
# @export
addState <- function(mypanel){
  #check if history in parameters of functions after added state
  after <- as.numeric(mypanel$addingState["after"])
  "chdir"%in%as.list(names(my_formals("source")))
  no_adding <- FALSE
  print(mypanel$numStates)
  if (after < mypanel$numStates){ 
    for (f in seq(min(after+1, mypanel$numStates), mypanel$numStates)){
      for(g in seq(1, mypanel$numStates)){ 
        #    print("in checking the history")
        print(names(my_formals(mypanel$M@list.matrix[[g,f]])))
        print(names(my_formals(mypanel$M@list.matrix[[f,g]])))
        if ("history"%in%names(my_formals(mypanel$M@list.matrix[[g,f]])))
          no_adding <- TRUE
        if ("history"%in%names(my_formals(mypanel$M@list.matrix[[f,g]])))
          no_adding <- TRUE
      }
    }
  }
  if (no_adding){
    rpanel::rp.messagebox("History as the parameter in hazard functions after state to be added.You have to redefine the model,
                  it is not clear how you want to redefine it.", title="No adding state")
    return(mypanel)    
  }
  
  View(mypanel$addingState)
  mypanel <- addStateMypanel(mypanel, as.character(mypanel$addingState["state_name"]),
                             as.numeric(mypanel$addingState["after"]))
  View(mypanel$hfNames)
  print(mypanel$numStates)
  print(mypanel$statesNames)
  drawTable(mypanel)
  drawTable(mypanel)
  mypanel
}