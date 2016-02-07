## The functions below help to caching the inverse of a matrix rather than compute it repeatedly.
## 
## Example:
## > test_matrix_a <- matrix(1:4, 2, 2)
## > cache_matrix_a <- makeCacheMatrix(test_matrix_a)
## > ## call the FIRST time - do a real computation!
## > cacheSolve(cache_matrix_a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >
## > ## call the SECOND time - take inverse value from the cache!
## > cacheSolve(cache_matrix_a)
## NB! getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >

# This function, makeCacheMatrix creates a special "vector",
# which is really a list containing a function to set and get the cached matrix
# and inverse matrix values.
makeCacheMatrix <- function(m = matrix()) {
    
    # inverse value of matrix 'm'
    m_inverse <- NULL
    
    # sets the value of the matrix
    set <- function(m_) {
        m <<- m_
        m_inverse <<- NULL
    }
    
    # gets the value of the cached matrix
    get <- function() {
        m
    }
    
    # sets inverse value of the matrix
    set_inverse <- function(inverse) {
        m_inverse <<- inverse
    }
    
    # gets inverse value of the matrix
    get_inverse <- function() {
        m_inverse
    }
    
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$get_inverse()
    # found cached!
    if(!is.null(inverse)) {
        message("NB! getting cached data")
        return(inverse)
    }
    
    # not found in the cache
    data <- x$get()
    
    # calculate inverse matrix
    inverse <- solve(data, ...)
    # set it into the cache
    x$set_inverse(inverse)
    
    # return inverse matrix
    inverse
}
