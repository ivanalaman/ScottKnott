##
## S3 method to 'aov' object
##

SK.aov <- function(x,
		   which=NULL,
		   id.trim=3,
		   sig.level=.05, ...)   
{
	if(is.null(which))
		which <- names(x$model)[2]
	mt <- model.tables(x, "means")      # summary tables for model fits
	if(is.null(mt$n))
		stop("No factors in the fitted model!")
	r     <- mt$n[names(mt$tables)][[which]] # groups and its number of replicates
	MSE   <- deviance(x)/df.residual(x)
	m     <- as.vector(mt$tables[[which]])             # means
	nms   <- names(mt$tables[[which]])
	ord   <- order(m, decreasing=TRUE)
	l     <- nlevels(x$model[, which])
	m.inf <- aggregate(x$model[,1],
			   by=list(x$model[[which]]),
			   function(x) c(mean=mean(x),
					 min=min(x),
					 max=max(x)))[,2]
        rownames(m.inf) <- nms  
	m.inf <- m.inf[order(m.inf[,1],
			     decreasing=TRUE),]
      	mMSE   <- MSE / r
	dfr    <- x$df.residual             # residual degrees of freedom
	g      <- nrow(m.inf)
	sig.level=0.05
	groups <- MaxValue(g,
			   m.inf[, 1],
			   mMSE,
			   dfr,
			   sig.level=sig.level,
			   1,
			   rep(0, g), 0, rep(0, g))
	res <- list(av=x,
		    groups=groups,
		    nms=nms,
		    ord=ord,
		    m.inf=m.inf,
		    sig.level=sig.level)
	class(res) <- c('SK', 'list')
	invisible(res)
}
