#' create the class "sparse.matrix" that has add `+`, multiply `%*%`, 
#' and transpose `t()` methods
#'
#' @description This function is used to create the class "sparse.matrix". 
#' Class "sparse.matrix" includes two parts:
#' The first part is a sparse.matrix indicating the location and value 
#' of non-zero entries.
#' The second part is the dimension of matrix
#' @param i A vector indicating the row coordinates of non-zero entries of the matrix
#' @param j A vector indicating the column coordinates of non-zero entries of the matrix
#' @param x A vector indicating the values of entries at (i,j) positions of the matrix
#' @param dims The dimension of sparse matrix, default is the largest coordinates
#' @return An "sparse.matrix" object
#' @examples
#' sm<- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
#' @export

sparse.matrix<-function(i,j,x,dims=c(max(i),max(j)))
{
  if (length(i)!=length(j))
    stop("Length of row coordinates and length ofcolumn coordinates do not match!")
  sm<-cbind(i,j,x) 
  #sort the order of rows firstly based on i, then based on j 
  sm<-sm[order(sm[,1],sm[,2]),]
  sm<-matrix(as.numeric(sm), ncol=3,byrow=F)
  colnames(sm)<-c("i","j","x")
  sm<-data.frame(i=sm[,1],j=sm[,2],x=sm[,3])
  rownames(sm)<-1:dim(sm)[1]
  m<-list(sparse.matrix=sm,dims=dims)
  class(m)<-"sparse.matrix"
  return(m)
}


#' Use for adding two "sparse.matrix" objects
#'
#' @description This function is used for adding two "sparse.matrix" objects
#' @param a A "sparse.matrix object"
#' @param b A "sparse.matrix object"
#' @return A "sparse.matrix" object
#' @examples
#' sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(2, 1),dims=c(2,3))
#' sm1 <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3),dims = c(2, 3))
#' sm0+sm1
#' @export
#' 
'+.sparse.matrix'<-function(a,b)
{  
  if(!inherits(b,"sparse.matrix") )
    stop("b argument is not a sparse.matrix type")
  if(a$dims[1]!=b$dims[1]|a$dims[2]!=b$dims[2])
    stop("sparse.matrix a and b have different dimensions")
  c <- merge(a$sparse.matrix, b$sparse.matrix, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c<-c[,c("i","j","x")]
  m<-list(sparse.matrix=c,dims=a$dims)
  class(m)<-"sparse.matrix"
  return(m)
}


#' Use for matrix multiple based on the class of matrix
#'
#' @description This function is used for matrix multiple
#' @param x A certain class matrix or vector object
#' @param y A certain class matrix or vector object
#' @return A certain class matrix or vector object
#' @export
#' 
'%*%'<-function(x,y){
  UseMethod("%*%",x)
}

#' Use for matrix multiple
#'
#' @description This function is used for matrix multiple
#' @param x A certain class matrix or vector object
#' @param y A certain class matrix or vector object
#' @return A certain class matrix or vector object
#' @export
#' 

'%*%.default'<- function(x,y){
  .Primitive("%*%")(x,y)
}

#' Use for multipling two "sparse.matrix" objects
#'
#' @description This function is used for multipling two "sparse.matrix" objects
#' @param x A "sparse.matrix object"
#' @param y A "sparse.matrix object"
#' @return A "sparse.matrix" object
#' @export
#' 
'%*%.sparse.matrix'<-function(x,y)
{
  if(!inherits(y,"sparse.matrix") )
    stop("y argument is not a sparse.matrix type")
  if(x$dims[2]!=y$dims[1])
    stop("The column number of matrix x is not equal to row number of matrix y")
  colnames(y$sparse.matrix) <- c("i2", "j2", "x2")
  c <- merge(x$sparse.matrix, y$sparse.matrix, by.x = "j", by.y = "i2",all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$index <- paste(c$i, c$j2, sep = ",")
  x_value <- tapply(c$x, c$index, sum)
  index <- strsplit(names(x_value), ",")
  d <- data.frame(i = as.numeric(sapply(index, getElement, 1)),
                  j = as.numeric(sapply(index, getElement, 2)),
                  x = as.numeric(x_value))
  m<-list(sparse.matrix=d,dims=c(x$dims[1],y$dims[2]))
  class(m)<-"sparse.matrix"
  return(m)
}

#' Use for transposing a "sparse.matrix" object
#'
#' @description This function is used for transposing s "sparse.matrix" object
#' @param x A "sparse.matrix object"
#' @return A "sparse.matrix" object
#' @examples
#' sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1))
#' t(sm0)
#' @export
#' 
t.sparse.matrix<-function(x)
{
  b<-x$sparse.matrix[, c(2,1,3)]
  colnames(b)<-c("i","j","x")
  m<-list(sparse.matrix=b,dims=c(x$dims[2],x$dims[1]))
  class(m)<-"sparse.matrix"
  return(m)
}
