## makeCacheMatrix(input_matrix) "prepares" the input_matrix to be associated with a cache of its inverse matrix value
## Once the matrix has been prepared, it can be updated, read (as well as the inverse in the cache) with 4 special functions
##
##  set_cache_matrix (can be used by the operator from the console)
##     Format : matrix_name$set_cache_matrix(updated_matrix)
##
##  get_cache_matrix (can be used by the operator from the console)
##     Format : matrix_name$get_cache_matrix()
##
##  set_cache_inverse_matrix (used by cacheSolved)
##
##  get_cache_inverse_matrix (used by cacheSolved. Can be used from the Console)
##     Format : matrix_name$get_inverse_cache_matrix()
##
## Attention : Once the matrix has been prepared with makeCacheMatrix ONLY use the 4 operations. 

makeCacheMatrix <- function(input_matrix = matrix()) {
        
        #Create and initialise (with NA) the cache matrix intended to store later the inverse of the input_matrix.
        cache_inverse_matrix = matrix(data=NA, nrow=nrow(input_matrix), ncol=ncol(input_matrix))
        
        ###############################################
        # Define 4 functions that manipulate the Cache Matrix (input_matrix plus the corresponding Cache_inverse_matrix)
        #  set_cache_matrix (can be used by the operator from the console)
        #  get_cache_matrix (can be used by the operator from the console)
        #  set_cache_inverse_matrix (used by cacheSolved)
        #  get_cache_inverse_matrix (used by cacheSolved. Can be used also from the Console)
        ###############################################
        
        #set_cache_matrix to modify the contents and/or dimensions of the Special Matrix
        set_cache_matrix <- function(input_mat) {
                #Could add tests here to validate the assumption that 
                #"input_mat is a well formed inversible square matrix of dimension >=1"
                
                #Update the Input Matrix with new values (important to use the same symbol as the input parameter of makeCacheMatrix)
                input_matrix <<- input_mat
                #Reset the cache_inverse_matrix with NA (to force a new inverse matrix to be recalculated later if required)
                #Important to use the same symbol as the one used in makeCacheMatrix
                cache_inverse_matrix<<-matrix(data=NA, nrow=nrow(input_mat), ncol=ncol(input_mat))
        }
        
        #get_cache_matrix to get the value of the matrix
        get_cache_matrix <- function()
        {return(input_matrix)
        }
        
        #set_cache_inverse_matrix updates the cache_inverse_matrix with the matrix inverse calculated by cacheSolve
        set_cache_inverse_matrix <- function(inverse_matrix)
        {cache_inverse_matrix <<- inverse_matrix
        }
        
        #get_cache_inverse_matrix called by cacheSolve to get the current inverse matrix cached and return it to the caller
        get_cache_inverse_matrix <- function()
        {return(cache_inverse_matrix)
        }
        
        #Build the list of 4 elements (each one is a function name) that are to be returned to the caller
        #Note : names could have been shortened on the lesource"cachematrix"ft side... but I kept it unambiguous.
        list(set_cache_matrix = set_cache_matrix,
             get_cache_matrix = get_cache_matrix,
             set_cache_inverse_matrix = set_cache_inverse_matrix,
             get_cache_inverse_matrix = get_cache_inverse_matrix
        )
}


## cacheSolve always return the inverse of the cache matrix that is identified in the calling parameter (matrix_name)
## If the value is available in the cache_inverse_matrix it is returned "as is".
## Otherwise it is (1) Computed, (2) Stored in cache inverse matrix for future reference (3) returned to the caller.
cacheSolve <- function(matrix_name, ...) {
        ## Get my Cached Inverse Matrix in temporary variable
        ## Note that R detects an error if matrix_name has not been previously "prepared" with makeCacheMatrix
        temporary_matrix <- matrix_name$get_cache_inverse_matrix()
        
        if(!is.na(temporary_matrix[1,1]))
        {##Inverse matrix data already available. Just return it... no extra work.
                message("getting cached data")
                return(temporary_matrix)
        }
        else
        {##Inverse Matrix not available. Need to (1) get, (2) compute inverse, (3) store it and (4) return
                stored_matrix <- matrix_name$get_cache_matrix()
                inverse_matrix <- solve(stored_matrix, ...) #solve compute the inverse of the matrix.
                matrix_name$set_cache_inverse_matrix(inverse_matrix) #Store new inverse for future reference
                return(inverse_matrix)
        }
}

### Test Scenario (from a 2:2 matrix to a 3:3 matrix and then back to the 2:2) 
#   source("cachematrix.R")
#   M1<-makeCacheMatrix(matrix(data=c(2,4,3,5), nrow=2))
#   M1$get_cache_matrix()
#   M1$get_cache_inverse_matrix()
#   cacheSolve(M1)        Is Equal to (-2.5, 2.0, 1.5, -1.0)
#   cacheSolve(M1)
#
#   M1$set_cache_matrix(matrix(data=c(-1,1,-2,2,2,8,5,3,10), ncol=3))
#   M1$getCacheMatrix()
#   M1$get_cache_matrix()
#   M1$get_cache_inverse_matrix()
#   cacheSolve(M1)        Is Equal to (-0;125, -0.5, 0,375, 0.625, 0, 0,125, -0,125, 0.250, -0.125)
#   cacheSolve(M1)
# 
#   M1$set_cache_matrix(matrix(data=c(2,4,3,5), ncol=2))
#   M1$get_cache_matrix()
#   M1$get_cache_inverse_matrix()
#   cacheSolve(M1)  Same results as previous 2:2 matrix.
#   cacheSolve(M1)

