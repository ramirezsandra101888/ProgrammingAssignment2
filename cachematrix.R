## Caching the inverse of a Matrix
## Matrix inversion is usually an expensive computation and there may be some

## advantage to caching the inverse of a matrix rather than compute it repeatedly.

##This function creates a "matrix" object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function() x
setInverse<-function(inverse) inv<<-inverse
getInverse<-function() inv 
list(set=set,get-=get,setInverse=setInverse,getInverse=getINverse)
}


## This function compures the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then it should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
        message("getting chached data")
        return(inv)
}
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
        }
        
