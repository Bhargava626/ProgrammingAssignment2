## Function to cache Inverse of a matrix, and the actual matrix
## Input(Optional):
##        Invertible Matrix 
##        Input matrix defaulted to Zero Matrix if not supplied with function call        
## Output:
##        Returns list of R Object Functions
##
## Internal Functions:
##  1) setmtrx:
##               Catches the input matrix to Matrix Object
##  2) getmtrx:
##               Returns the cached Matrix Object
##  3) setinvmtrx:
##               Catches the Inverse of Input Matrix to Matrix Object
##  4) getinvmtrx:
##               Returns the Cached Inverse Matrix Object
##
makeCacheMatrix <- function(mtrx=matrix(0))
{
                
        invmtrx <- matrix(0)   
        setmtrx <- function(smtrx)
        {
              mtrx <<- smtrx
              invmtrx <<- matrix(0)
        }
        getmtrx <- function() mtrx
        setinvmtrx <- function(sinvmtrx)
        {
          invmtrx <<- sinvmtrx
        }
        getinvmtrx <- function() invmtrx
        
        list(setmtrx=setmtrx,getmtrx=getmtrx,setinvmtrx=setinvmtrx,getinvmtrx=getinvmtrx)
}

## if input matrix identical to matrix in cache, Function returns Inverse matrix from cache.
## if input matrix is new,  inverse of an input matrix is solved and cached to matrix object, also 
## Input matrix is cached to matrix object
##
## Input:
##         1)  List of Function objects
##         2)  Invertible Matrix  
## Output:
##        Returns inverse of a matrix from cache
##       or solves the inverse of a matrix and catches inverse  to matrix object.
##
##

cacheSolve <- function(chmtrx,mtrx)
{
      
   inmtrx <- chmtrx$getmtrx()
   
   
   if (identical(mtrx,inmtrx)) 
   {
     print ('Returns inverse matrix from cache') 
     sinvmtrx <- chmtrx$getinvmtrx()
      if (all(is.na(sinvmtrx)))
      {
         print('Singular matrix not Invertible, return NA Matrix ')
      }
     return (sinvmtrx)
      
     
   }else
   {
      print ('New matrix, catch inverse matrix, return Inverse Matrix')
      chmtrx$setmtrx(mtrx)
      if (det(mtrx) != 0)
      {
        sinvmtrx <- solve(mtrx)  
      }else
      {
        print ('Not Invertible, singular matrix, cache NA Matrix')
        sinvmtrx <- matrix(NA,nrow(mtrx),ncol(mtrx))
      }
      
      chmtrx$setinvmtrx(sinvmtrx)
   }
   sinvmtrx
}