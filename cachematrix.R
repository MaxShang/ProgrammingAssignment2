## Put comments here that give an overall description of what your
# makeCacheMatrix is a function which takes a matrix as input and return a list of 
# four functions used to set matrix, get matrix, set the inverse of the matrix, get the inverse of the matrix
# cacheSolve is a fuction which takes makeCacheMatrix(matrixA) as input and calculate the inverse of matrixA(if the
# inverse had already bean calculated before, it will skip the calculation process and get the inverse directly 
# from the computer memory(the environment).


## Write a short comment describing this function
## functions makeCacheMatrix 
#input: a squre matrix for which we want to calculate its inverse
#output: a list of four functions: set, get,set.inverse and get.inverse

makeCacheMatrix<-function(x=matrix()){
        inverse<-NULL
        set<-function(y){
                x <<- y
                inverse <<- NULL
        }
        get<-function(){x}
        set.inverse<-function(ivs){inverse<<-ivs} 
        get.inverse<-function()inverse
        
        list(set=set,
             get=get,
             set.inverse=set.inverse,
             get.inverse=get.inverse)
}

## Write a short comment describing this function
# cacheSolve is a fuction which takes makeCacheMatrix(matrixA) as input and calculate the inverse of matrixA(if the
# inverse had already bean calculated before, it will skip the calculation process and get the inverse directly 
# from the computer memory(the environment).
cacheSolve<-function(x,...){
        inverse<-x$get.inverse()
        
        if(!is.null(inverse)){
                message("getting cached inverse of the matrix")
                return(inverse)
        }
        # if the inverse is not calculated(is.null(inverse=TRUE)), then we calculate it and save it in the 
        # memory use function set.inverse
        inverse<-solve(x$get())
        x$set.inverse(inverse)
        inverse
}

##examples of how to use the two functions:
# mat<-matrix(data = rnorm(9),nrow = 3,ncol = 3)
# mcm<-makeCacheMatrix(mat)
# 
# cacheSolve(mcm)
# cacheSolve(mcm)
# temp<-makeCacheMatrix(matrix(data = rnorm(9),nrow = 3,ncol = 3))
# cacheSolve(temp)
# cacheSolve(mcm)
