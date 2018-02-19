#2 functions created, one to set values and also get values from, other to check presence of inverse and 
# perform inversion in case inverse isn't already present. The functions are makeCacheMatrix and cacheSolve respectively.

# makeCachematrix creates a special "matrix" object that can cache its inverse.
#list elements perform the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     mat = NULL
     setter <- function(y)
     {
          x<<-y
          mat<<-NULL
          
     }
     getter <- function() x
     setterinv <- function(inv) mat<<- inv
     getterinv<- function() mat
     list(setter = setter, getter = getter, setterinv = setterinv, getterinv=getterinv)
}


# cacheSolve Checks whether inverse already exists and returned, if inverse is not present, inverse in calculated, then displayed

cacheSolve <- function(x, ...) {
     #get matrix
     mat <- x$getterinv()  #if present use pre-existing value
     if (!is.null(mat)){
          message("getting inverted matrix")
          return(mat)
     }
     data<- x$getter()      # if null, calc inverse using solve  
     mat <- solve(data,...)
     x$setterinv(mat)
     mat
     
        ## Return a matrix that is the inverse of 'x'
}
