## Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special "vector", which is really a list 
## containging a fuction to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)       
}


## cacheSolve function calculates the inverse matrix of the special "vector" 
## created with the above function. It first checks to see if the inversed matrix 
## has already been calculated. If so, it gets the mean from the cache and skip 
## the computation. Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
       m <- x$getInverse()
       if(!is.null(m)){
               message("getting cached matrix")
               return(m)
       }
       data <- x$get()
       m <- solve(data)
       x$setInverse(m)
       m
}

