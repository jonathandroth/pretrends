rbind.all.columns <- function(x, y) {

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA

  y[, c(as.character(x.diff))] <- NA

  return(rbind(x, y))
}


rbindapply.allColumns <- function(X, FUN, ...){
  resultsList <- base::lapply(X,FUN,...)
  df <- Reduce(rbind.all.columns, resultsList)
  return(df)
}

basisVector <- function(index = 1, size = 1){
  v <- matrix(0, nrow = size, ncol = 1)
  v[index] = 1
  return(v)
}

