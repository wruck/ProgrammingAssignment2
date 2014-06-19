# O calculo de inversao de uma matriz geralmente dispende muito do poder de processamento do 
# computador, de forma que seu armazenamento em cache e vantajoso, ao inves de executa-lo varias
# vezes.

# A funcao MakeCacheMatrix cria uma lista que contem uma funcao que:
  # Defini o valor da matriz 
  # Calcula o valor da matriz 
  # Defini o valor da matriz inversa 
  # Calcula valor da matriz inversa


# The calculation of the inverse of a matrix usually spends a lot of processing power 
# computer, so that your caching is advantageous, rather than run it several times. 

# The MakeCacheMatrix function creates a list that ocntém a function that: 

  # Define the value of the matrix 
  # Calculates the matrix 
  # Define the value of the inverse matrix 
  # Estimated value of the inverse matrix
  
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# A proxima funcao e usada para calcular a matriz inversa. Ela primeiro checa se a
# matriz inversa foi calculada; se ja foi, ela mostra o resultado e ignora o restante
# do calculo. Se ainda nao foi, ela testa se a matriz e invertivel,
# se nao for, ela mostra uma mensagem de erro e para o processamento; se for,
# ela calcula a inversa e defini o valor no cache por meio da
# funcao setinverse.

# The next function is used to calculate the inverse matrix. It first checks if the 
# was calculated inverse matrix; if is, it shows the result and ignores the rest 
# the calculation. If is not, it tests whether the matrix is invertible,
# if not invertible, displays a message stating that the matrix is not invertible and stop the
# processing; if is, computing the inverse and set the value in the cache by 
# setinverse function.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    test<-det(data)
    if (test == 0){
         stop("matrix is not invertible")
     }
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


## Sample run:
##  x = rbind(c(5, -1/7), c(-1/7, 5))
##  m = makeCacheMatrix(x)
##  m$get()
##        [,1]       [,2]
## [1,]  5.0000000 -0.1428571
## [2,] -0.1428571  5.0000000

## No cache in the first run
##  cacheSolve(m)
##             [,1]        [,2]
## [1,] 0.200163399 0.005718954
## [2,] 0.005718954 0.200163399

## Retrieving from the cache in the second run
##  cacheSolve(m)
## getting cached data.
##             [,1]        [,2]
## [1,] 0.200163399 0.005718954
## [2,] 0.005718954 0.200163399