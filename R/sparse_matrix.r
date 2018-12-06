#' create the class "sparse.matrix" that has add `+`, multiply `%*%`, 
#' and transpose `t()` methods
#'
#' @description create sparse matrix using sparse.matrix, operations defined including add, multiply and transpose
#' @param i row indeces of a nontrival element
#' @param j col indeces of a nontrival element
#' @param x value of the element corresponding to i, j
#' @param dims dimensions of the sparse matrix
#' @return A sparse.matrix object
#' @export

# define class
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  structure(list(data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1)), dims), class = "sparse.matrix")
}

# add
`+.sparse.matrix` <- function(a, b){
  if (!identical(a[[2]], b[[2]]))
    stop("dimensions not match")
  c <- merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
  sparse.matrix(c$i, c$j, c$x, dims = a[[2]])
}

# multiply 
# %*% is not S3 object
`%*%.default` = .Primitive("%*%")  # keep defalut
`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

`%*%.sparse.matrix` <- function(a, b){
  if ((a[[2]][2] != b[[2]][1]))
    stop("dimensions not match")
  colnames(b[[1]]) <- c("i2", "j2", "x2")
  c <- merge(a[[1]], b[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  sparse.matrix(c$i, c$j, c$x, dims = c(a[[2]][1], b[[2]][2]))
}

# transpose
t <- function (x, ...) {
  UseMethod("t", x)
}

`t.sparse.matrix` <- function(a){
  temp <- a[[1]]$i
  a[[1]]$i <- a[[1]]$j
  a[[1]]$j <- temp
  a[[2]] <- rev(a[[2]])
  return(a)
}