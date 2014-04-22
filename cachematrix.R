## The code descibed below use two functions. The function makeCacheMatrix first calculate the inverse of a matix and save that for caching if same matrix is passed as an argument using cacheSolve funciton.

## This funciton uses solve() function from "base" packages to calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve function check first if the matrix inverse is already calculated, else calculates using solve() function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## This method return a matrix calles "m" that is the inverse of 'x'
}