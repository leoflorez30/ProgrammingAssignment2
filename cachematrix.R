## The functions written here, when combined, make an improvement of the solve function.
# The solve function takes an argument of a square matrix and returns its inverse. What we 
# are doing here is to make two functions, makeCacheMatrix and cachesolve, in order to use
# them instead of the function solve, such that if the inverse of a matrix is already 
# calculated, it is stored in the cache, so when we calculate again the inverse of the
# same matrix, it checks if it's already in the cache and in that case it retrieves 
# it from the cache instead of doing the -longer- process of calculating the inverse 
# with solve. Making the process of getting the inverse of a matrix more efficient.


## The function makeCachematrix has a series of 'set' and 'get' functions,
# and returns a list with the four created functions. It starts with the set
# function, it takes as an argument a value y (whose expected class is a square 
# matrix) and assigns the value y to the object x and NULL to m, both assignments
# being done in the parent environment. The function get, gets x. The function
# setsolve takes an argument solve and sets m to solve in the parent environment.
# And the function getsolve, who gets m. The return of this function (makeCachematrix)
# is a named list with these four functions.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve takes as an argument an object who is an output of the
# function makeCachematrix. Then it takes the member $getsolve of the list and 
# checks if it's null. If the inverse of the matrix has been previously calculated
# by cacheSolve, then m must be not null and the cache it's retrieved. If it's the
# first time that cacheSolve receives that matrix as an input, then it will do
# the normal process of calculating the inverse of the matrix.

# An example of how to use the functions is the next:
# Set z <- makeCacheMatrix(matrix(1:4,2,2)), which stores a list of the four functions
# stated above in the variable z.
# Then, evaluate cacheSolve(z), it will return the first time the inverse of the 
# matrix(1:4,2,2), which was given as an input of makeCacheMatrix.
# Then, if we evaluate cacheSolve(z) again, it will retrieve the inverse from the
# cache, making the process more efficient.

# An example code should look like this:
# d <- matrix(1:4,2,2)
# z <- makeCacheMatrix(d)
# cacheSolve(z)
# cacheSolve(z)
# The two times we call cacheSolve we'll get the inverse of d, but the second time
# it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
     
}

#As a final note, the use of the function set is to change the value of the matrix,
# so we just call makeCacheMatrix one time.
# Example code:
# d <- matrix(1:4,2,2)
# z <- makeCacheMatrix(d)
# cacheSolve(z)
# cacheSolve(z)    --> This time the inverse will be retrieved from cache
# z$set(matrix(5:8,2,2))
# cacheSolve(z)
# cacheSolve(z)    --> Here the inverse will be retrieved from cache, 
                      #this time of the second matrix.
