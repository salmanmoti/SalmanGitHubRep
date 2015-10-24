## -----------------DESCRIPTION--------------------
## The purpose of this script is enable the user to 
## cache the inverse of a matrix when the values are not
## expected to change. Instead of reinverting a 
## matrix that is not expected to change
## this script will store the inverted matrix so it
## may be looked up as needed instead of 
## being redone each time.
## ------------------------------------------------
## ------------------------------------------------
## ------------------------------------------------
## ----------DESCRIPTION OF FUNCTION---------------
## This function creates an object called CacheMatrix.
##
makeCacheMatrix <- function(x = matrix()) 
  {
    InvCache<-NULL
    Fix<-function(a) 
    {
      x<<-a
      InvCache<<-NULL
    }
  Pull<-function() x
  setInv<-function(Inv) InvCache<<-Inv
  getInv<-function() InvCache
  list(set=Fix, get=Pull, setInv=setInv, getInv = getInv)
  }
## ------------------------------------------------
## ------------------------------------------------
## ------------------------------------------------
## ----------DESCRIPTION OF FUNCTION---------------
## This function returns the inverse of CacheMatrix.
##
cacheSolve <- function(x, ...) 
  {
  InvFunction<-x$getInverse()
  IF(!is.null(InvFunction))
    {
    Message("Cached Data Being Retrieved. Please Standby.")
    Return(InvFunction)
    }
  RawData<-x$getInverse()
  InvFunction<-Solve(RawData,...)
  InvFunction
  }
## --------------------END-------------------------