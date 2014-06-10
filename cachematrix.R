## makeCacheMatrix function provides 4 different functions to be used to:
# 1) set the matrix
# 2) get the matrix set
# 3) calculate the inverse via the solve() fx
# 4) get the inverse
# to test it:
# - create an invertible matrix (ex. c=rbind(c(1, -1/4), c(-1/4, 1)) )
# Call  the makeCacheMatrix () function and assign it's
#  return value (a list of four functions) to a variable, v
#  v is now a list of the four functions seen above
# use v's set function to set the matrix "c" created above ==> v$set(c)
#pass the matrix v to the cachematrix() function
#   the inverse of the original matrix should be returned
#pass again the same matrix
# the same inverse of the original matrix should be returned but
#  also a message "retrieving value from cache" should be displayed
# indicating that the inverse has not been calculated but retrieved from a 
# copy resident in memory

cachemean(v)
v <- makeVector()
# 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve fx gets in input a matrix and return the inverse of it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ##if the inverse has been already calculatd, 
        #the function retrieves cached data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if the matrix is new or the old one has been changed....
        #the function recalculates the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
