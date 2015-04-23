## Calculating inversion of matrix and keeping it in cache for repeating calculations

## "makeCacheMatrix"
## Make the list of functions, which keep cache data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {        # insert new matrix
                x <<- y             # change matrix in parent environment
                inv <<- NULL        # clear cache  
        }
        get <- function() x        # return matrix
        set_inv <- function(invers) inv <<- invers    # save inverse matrix into cache
        get_inv <- function() inv                     #return cached inverse matrix     
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## "cacheSolve"
## Check if we've already calculated inverse of matrix, if yes - return cached value
## if no - calculate inversion and put it into cache = list of makeCacheMatrix

cacheSolve <- function(x, ...) {          
                inv <- x$get_inv()            # checking if we have cache data
                if(!is.null(inv)) {
                        message("getting cached matrix")
                        return(inv)           # return cached inverse matrix
                }
                data <- x$get()               # get original matrix
                m <- solve(data)              # calculate inversion
                x$set_inv(inv)                 # save inverse matrix in cache
                inv
        }


