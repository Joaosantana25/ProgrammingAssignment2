makeCacheMatrix <- function(x = matrix()){
  #assumimos que a matriz possa ser invertida
  inv <- NULL
  set <- function(y){
    x <<- y #muda o valor paterno 
    inv <<- NULL
  }
  get <- function() x #para receber a matriz
  setinv <- function(inverse) inv<<- inverse #definir a inversão da matriz
  getinv<- function(){
    inver <- ginv(x)
    inver%*%x
  }
  list(set= set, get= get, setinv = setinv, getinv= getinv)
  
}
#começando a segunda parte
cacheSolve <- function(x, ...)#recebe os dados do cache 
{
  inv <-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv #retorna o valor da inversa
} 