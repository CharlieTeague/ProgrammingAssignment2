## Here there are two functions, makeCacheMatrix & cacheSolve


## MakeCacheMatrix contains set,get,setinv,getinv

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL               ##set inverse as NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get <- function() {x}        ##function to get matrix x
        setinv <- function(inverse) {inv<<-inverse}
        getinv <- function(){inv}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve is used to get the cached data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){ ##checking IF inverse is null
                message("getting the cached data")
                return(inv) ##returns inverse val
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv ##return a matrix that is the inverse of the original matrix
}

