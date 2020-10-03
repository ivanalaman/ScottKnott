xtable.SK <- function(x,...){
  
  aux1 <- x$out$Result
  aux3 <- c(x$out$Sig.level,rep(NA,length(x$out$Result[,1])-1))
  aux4 <- data.frame(aux1,
                     'Sig.level' = aux3)

  res <- xtable::xtable(aux4,...)
  class(res) <- c('xtable.SK', 'xtable', 'data.frame')
  return(res)
}
