## Over all the purpose of both functions are to save the inverse of a matrix in catche and grant user ability to 
## pull the inverse out or modify it anytime they want to and save precious computation time. 

## The purpose of makeCacheMatrix is to take an matrix and grant it ability to get and set its inverse in the cache. 

makeCacheMatrix <- function(x = matrix()) {
        # create a empty space to store inverse
        inverse <- NULL

        # initialize the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        # obtain the value of the matrix
        get <- function() x

        # set the value of the inverse matrix
        setInverse <- function(solve) inverse <<- solve(x)

        # get the value of the inverse matrix
        getInverse <- function() inverse

        # make sure all terms are consistent
        list (set = set, get = get, 
                setInverse = setInverse, getInverse = getInverse)

        }

}


# The function of cacheSolve is to first check whether there is already a inverse stored in the cached file. If so, return the value
# If not, calculate the value and write the inversed value into the cache while returning the inverse of the matrix. 

cacheSolve <- function(x, ...) {
       
        #Obtain inverse from the cache
        inverse <- x$getInverse()

        #Check whether the inverse is empty, if not, directly return value already computed
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        #Load data of the matrix from cache
        data <- x$get()
        
        #compute and define inverse
        inverse <- solve(data)

        #set inverse into the cache as inverse
        x$setInverse(inverse)

        #return the inverse of x
        inverse
}
}
