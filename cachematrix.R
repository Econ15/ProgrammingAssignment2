
#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of the matrix
#4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        Inv<-NULL
        set<-function(y) {
        x <<- y
        Inv<<-NULL
}
get<- function() x
setInverse<- function(solve) Inv<<- solve
getInverse<- function() Inv
list(set = set, get = get, 
     setInverse = setInverse, 
     getInverse = getInverse)

}


# The following function, 'cacheSolve' calculates the inverse of the special "matrix"
#created with the above function, "makeCacheSolve'. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setInverse`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv<- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return (Inv)
        }
        data<-x$get()
        Inv<- solve(data,...)
        x$setInverse(Inv)
        Inv
}




