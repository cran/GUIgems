# @title fixing a function, the name can be given by a variable
# @description
# Like fix(). But the argument  
# can be also as a value of a variable.
# @param x - the name of an R object, as a name or a character string 
# or a value of a variable.
fixByString <- function (x, ...) 
{
  subx <- x
  if (is.name(subx)) 
    subx <- deparse(subx)
  if (!is.character(subx) || length(subx) != 1L) 
    stop("'fix' requires a name")
  parent <- parent.frame()
  if (exists(subx, envir = parent, inherits = TRUE)) 
    x <- edit(get(subx, envir = parent), title = subx, ...)
  else {
    x <- edit(function() {
    }, title = subx, ...)
#    environment(x) <- .GlobalEnv    ## changing the globalEnv for check(), test if this is fine.
#  }
#  assign(subx, x, envir = .GlobalEnv)
    environment(x) <- parent    ## changing the globalEnv for check(), test if this is fine.
  }
  assign(subx, x, envir = parent)
}
