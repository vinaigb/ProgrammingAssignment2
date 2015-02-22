## makeCacheMatrix Function is descibed as follows:
## makeCacheMatrix is defined by  mypseudoMatrix, which is a list of THREE functions (the course example
## produced a list of FOUR functions, but the "set(Y)" function [which I have commented out in the R code

## "get()", "setMatInv(MatInverse)", & "getMatInv()":
## "get()" simply outputs the argument into makeCacheMatrix, which I've called myMatrix .
##
## "setMatInv(MatInverse)" simply assigns to the parent-environment variable Minv (i.e.
## the makeCacheMatrix-environment variable Minv) the setMatInv argument MatInverse.
##
## "getMatInv()" simply outputs the parent-environment variable Minv


makeCacheMatrix <- function(x = matrix()) {
 Minv <-NULL
set<-function(Y){
myMatrix<<- Y
Minv <<-NULL
}
get<-function() myMatrix
setMatInv <-function(MatInverse) Minv <<- MatInverse
getMatInv<-function() Minv
mypseudoMatrix<<-list(get=get,setMatInv=setMatInv,getMatInv=getMatInv)
mypseudoMatrix

}


## cacheSolve Function Description:
## cacheSolve takes as its argument a list of fuctions,
## list produced by makeCacheMatrix(myMatrix), and produces the matrix inverse 
## matching the makeCacheMatrix(myMatrix)-environment variable Minv) using R's matrix "solve" command.
## In the execution of cacheSolve(get_setMatInv_getMatInv_of_myMatrix), it is first checked to see if the
## argument-associated Minv already has a "cached" value assigned, in which case that value is simply
## retrieved. If, on the other hand, the argument-associated Minv starts out NULL then we get via
## makeCacheMatrix the argument-associated myMatrix, assign myMatrix to DataMatrix, & determine the inverse
## matrix of DataMatrix, assigning that inverse to the makeCacheMatrix(myMatrix)-environment variable Minv.
## For brevity/laziness, the cacheSolve argument get_setMatInv_getMatInv_of_myMatrix can in default omitted,
## provided that cacheSolve is determining the inverse of the myMatrix of the most-recently executed
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
Minv<- get_setMatInv_getMatInv_of_myMatrix$getMatInv()
if(!is.null(Minv)){
message("getting cached matrix inverse")
return(Minv)
}
DataMatrix<-get_setMatInv_getMatInv_of_myMatrix$get()
Minv<-solve(DataMatrix,...)
get_setMatInv_getMatInv_of_myMatrix$setMatInv(Minv)
Minv   
}
