##This first function creates a vector that contains the functions to create the inverse of a matrix. It saves the result in another element of the list

makeCacheMatrix <- function(x = matrix()) { 
      inv <- NULL                            
      set <- function(y) {                   
            x <<- y                           
            inv <<- NULL                       
        }
      get <- function() x                     
      
        setinverse <- function(inverse) inv <<- inverse  
        getinverse <- function() inv                    
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                      
}


#If the inverse of the matrix has been calculated previously, and it has not changed, cacheSolve uses the function get inverse from above
#And returns the inverse previously calculated. Conversely, if it hasn't, it calculates the inverse.
  cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
             message("getting cached data")
             return(inv)
         }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
       inv
  }
  
  ##Testing the function.

  tau <- c(0, 0, 0, 0, 1, 
           1, 0, 0, 0, 0, 
           0, 1, 0, 0, 0, 
           0, 0, 1, 0, 0, 
           0, 0, 0, 1, 0) 
  tau<-matrix(tau, ncol=5, byrow=TRUE) 
  
mat<- makeCacheMatrix(tau)
cacheSolve(mat)
