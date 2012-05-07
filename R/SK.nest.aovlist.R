##
## S3 method to 'aovlist' object
##

SK.nest.aovlist <- function(x, which, id.trim=3, error, fl2, fl3=0,
                     sig.level=.05, ...)
{
  mt   <- model.tables(x, "means")    # summary tables for model fits
  if(is.null(mt$n))
    stop("No factors in the fitted model!")
  tabs <- mt$tables[-1][which]        # specified group means
  r    <- mt$n[names(tabs)][[which]]  # groups and its number of replicates
  MSE  <- sum(resid(x[[error]])^2) /
            x[[error]][[8]]
  tab  <- tabs[[which]]               # tab=means
  ta   <- model.frame.aovlist(x) 
  if (fl3 == 0)                       # split-plot
  {                       
    m      <- as.vector(tab[, fl2])
    which1 <- names(dimnames(tab))[1]
    which2 <- names(dimnames(tab))[2]
    l      <- nlevels(ta[, which1])
    m.inf  <- matrix(nrow=l, ncol=3)
    for(i in 1:l){ 
      v <- ta[, 1][(ta[, which1] == levels(ta[, which1])[i])
             &(ta[, which2] == levels(ta[, which2])[fl2])] 
      m.inf[i, 1] <- mean(v)
      m.inf[i, 2] <- min(v)
      m.inf[i, 3] <- max(v)
    }
    f1 <- paste(strtrim(which1, id.trim), 1:l, sep='_')
    f2 <- paste(strtrim(which2, id.trim), fl2, sep='_')
    fl <- tolower(paste(f2, f1, sep='/'))
    rownames(m.inf) <- fl
  }   
  else                                #split-split-plot
  {
    m  <- as.vector(tab[, fl2, fl3]) 
    which1 <- names(dimnames(tab))[1]
    which2 <- names(dimnames(tab))[2]  
    which3 <- names(dimnames(tab))[3]
    l      <- nlevels(ta[, which1])
    m.inf  <- matrix(nrow=l, ncol=3)
    for(i in 1:l){ 
      v <- ta[, 1][(ta[, which1] == levels(ta[, which1])[i])
             &(ta[, which2] == levels(ta[, which2])[fl2])
             &(ta[, which3] == levels(ta[, which3])[fl3])] 
      m.inf[i, 1] <- mean(v)
      m.inf[i, 2] <- min(v)
      m.inf[i, 3] <- max(v)  
    }
    f1 <- paste(strtrim(which1, id.trim), 1:l, sep='_')
    f2 <- paste(strtrim(which2, id.trim), fl2, sep='_')
    f3 <- paste(strtrim(which3, id.trim), fl3, sep='_')
    fl <- tolower(paste(f3, f2, f1, sep='/'))
    rownames(m.inf) <- fl
  }
  nms   <- dimnames(tab)[[1]]
  ord   <- order(m, decreasing=TRUE)
  mMSE  <- MSE / r
  dfr   <- x[[error]][[8]]            # residual degrees of freedom
  m.inf <- cbind(m.inf[, 1][ord], m.inf[, 2][ord], m.inf[, 3][ord])
  colnames(m.inf) <- c('mean', 'min', 'max')
  g      <- nrow(m.inf)
  groups <- MaxValue(g, m.inf[, 1], mMSE, dfr, sig.level=sig.level, 1,
              rep(0, g), 0, rep(0, g))
  res    <- list(av=x, groups=groups, nms= nms, ord=ord, m.inf=m.inf,
              sig.level=sig.level, r=r, which=which, tab=tab, fl2=fl2, fl3=fl3)
  class(res) <- c('SK.nest', 'SK', 'list')
  invisible(res)
}

