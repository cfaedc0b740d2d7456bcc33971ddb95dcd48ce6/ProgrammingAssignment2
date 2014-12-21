##
# The pourpouse of this file is to demonstrate the basics of lexical scopeing
# on the basis of cacheing an inverse of a matrix 
# This code is not meant to be used as it is unsecure and unoptimized
##

# getters and setters for the matrix and its inverse
# this is not very secure since it is possible to change the inverse without changeing original matrix
makeCacheMatrix <- function(data = matrix()) {
    
    inverse <- matrix()
    
    set <- function(newData) {
        data <<- newData
        inverse <<- matrix()
    }
    
    get <- function() {
        return (data)
    }
    
    setInverse <- function(newInverse) {
        inverse <<- newInverse
    }
    
    getInverse <- function() {
        return (inverse)
    }
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

#function for computing and fetching cashed inverse
cacheSolve <- function(data, ...) {
    
    #assigning new variables for the sake of clarity
    inverse <- data$getInverse()
    
    # chceckig if the inverse is already cached
    if( !is.na (inverse[1,1]) ){
        message("fetching cached data")
        return (inverse)
    }
    
    matrix <- data$get()
    
    #inverting
    inverse <- solve(matrix)
    
    #cacheing
    data$setInverse(inverse)
    
    return (inverse)
}

##
#   https://xkcd.com/1296/
##
