##
## S3 method to 'aov' object
##

SK.aov <- function(x, which=NULL, id.trim=3, sig.level=.05, ...)   
{
  if(is.null(which))
    which <- names(x$model)[2]
  mt <- model.tables(x, "means")      # summary tables for model fits
  if(is.null(mt$n))
    stop("No factors in the fitted model!")
  tabs  <- mt$tables[-1][which]       # specified group means
  r     <- mt$n[names(tabs)][[which]] # groups and its number of replicates
  MSE   <- sum(resid(x)^2) /
             x$df.residual
  tab   <- tabs[[which]]              # tab=means
  m     <- as.vector(tab)             # means
  nms   <- names(tab)
  ord   <- order(m, decreasing=TRUE)
  l     <- nlevels(x$model[, which])
  m.inf <- matrix(nrow=l, ncol=3)
  for(i in 1:l) { 
    v <- x$model[, 1][x$model[, which] == levels(x$model[, which])[i]]
    m.inf[i, 1] <- mean(v)
    m.inf[i, 2] <- min(v)
    m.inf[i, 3] <- max(v)  
  } 
  m.inf  <- cbind(m.inf[, 1][ord], m.inf[, 2][ord], m.inf[, 3][ord])   
  dimnames(m.inf) <- list(strtrim(nms[ord], id.trim), c('mean', 'min', 'max'))
  mMSE   <- MSE / r
  dfr    <- x$df.residual             # residual degrees of freedom
  g      <- nrow(m.inf)
  groups <- MaxValue(g, m.inf[, 1], mMSE, dfr, sig.level=sig.level, 1,
              rep(0, g), 0, rep(0, g))
  res    <- list(av=x, groups=groups, nms=nms, ord=ord, m.inf=m.inf,
              sig.level=sig.level)
  class(res) <- c('SK', 'list')
  invisible(res)
}
