## Write two functions that are used to create a special object that
## stores a numeric squared invertible matrix and caches its inverse

## Creates a special "matrix" object that can cache its inverse
## by returning a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
				inv <- NULL
				set <- function(y) {
								x <<- y
								inv <<- NULL
				}
				get <- function() x        
				setinverse <- function(inverse) inv <<- inverse    
				getinverse <- function() inv    
				list(set = set, get = get,             
						setinverse = setinverse,
						getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above, and retrieve the inverse
## from the cache if the inverse has already been calculated and the matrix has not changed

cacheSolve <- function(x, ...) {
				inv <- x$getinverse()        
				if(!is.null(inv)) {                
								message("getting cached data")               
								## Return a matrix that is the inverse of 'x'        
								return(inv)        
				}        
				data <- x$get()        
				inv <- solve(data, ...)        
				x$setinverse(inv)               
				## Return a matrix that is the inverse of 'x'
				inv
}
