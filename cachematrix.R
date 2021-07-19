## makeCacheMatrix and cacheSolve work together to compute and save the inverses of matrices.
## By creating a cache and saving the computed result, the program saves energy by directly 
## returning the saved value when requested, instead of computing the inverse matrix again.

## makeCacheMatrix creates a list of functions that can be refereed to by other functions.
## the '<<-' operator makes variables that can be accessed across environments.

makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<- function(inversematrix) m<<-inversematrix
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix) # turns the functions into a list that can called upon
}


## cacheSolve obtains the values and information needed from makeCacheMatrix, and returns
## the inverse matrix and saves the results back into makeCacheMatrix, setmatrix().

cacheSolve<-function(x,...)
{
        m<-x$getmatrix()
        if(!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data<-x$get()
        im<-solve(data,...)
        x$setmatrix(im)
        im
}
        ## Return a matrix that is the inverse of 'x'
