hellwig <- function(y, x)
{
  n <- ncol(x)
  l <- (2^n)-1
  
  R0 <- cor(y, x)
  R <- abs(as.matrix(cor(x)))

  argument <- replicate(n, c(0, 1), simplify = FALSE)
  comb <- as.matrix(expand.grid(argument))
  comb <- comb[-1,]
  
  h <- matrix(0, l, n)
  
  for(i in 1:l) 
  {
    for(j in 1:n)
    {
      h[i,j] <- (comb[i, j] * (R0[j]^2))/ (comb[i,] %*% as.vector(R[,j]))
    }
  }

  
  m=which.max(rowSums(h))
  colnames(comb) <- colnames(x)
  
  return((comb[m,]))
}

#hellwig(zmienna_objaśniana, zmienna objaśniająca)

