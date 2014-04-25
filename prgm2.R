# E. Lonvick; programming assignment 2
makeCacheMatrix <- function(x = matrix()) {
        CacheMatrix <- NULL

        set <- function(y) {
                x <<- y
                CacheMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(CacheMatrix)  CacheMatrix <<- x 
        getMatrix <- function()  CacheMatrix
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)

}

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}