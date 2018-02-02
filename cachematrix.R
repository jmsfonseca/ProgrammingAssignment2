## Cathing the inverse of a Matrix
## Below are two functions that are used to create a special object that stores a matrix and catch the inverse

##This function creates a special "matrix" objectthat cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<-y
		inv <<- NULL
  }
    get <- function(inverse) inv <<-inverse
    setInverse <-function() inv
    getInverse <-function() inv
    list(set=set
         get=get
         setInverse=setInverse
         getInverse=getInverse)
    }     


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatriz above.If the inverse has already been calculated
## (and the matrix not changed), then it should retrive the inverse from
## cache


cacheSolve <- function(x, ...) {
  
    inv <- x$getInverse()
    if (!is.null(inv)){
    message ("getting cached data")
    return (inv)      
}
    mat <-x$get()
    inv <-solve(mat,...)
    x<-setInverse(inv)
    inv
    }