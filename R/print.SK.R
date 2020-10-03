##
## S3 method to print 'SK' object
##

print.SK <- function(x,
		     digits=2L,
		     ...)
{
  cat('Results\n')  
  print(x$out$Result,
        ...)

  cat('\nSig.level\n',
      x$out$Sig.level)

  cat('\n\nStatistics\n')
  print(x$stat,
	digits=digits,
        ...)

  cat('\n\nClusters\n')
  cc  <- x$clus
  i <- 0
  while(i < length(cc)){
	  i <- i+1
	  cat(paste('#################  Cluster ',i,' ################\n'))
	  gro <- strsplit(cc[[i]],',')
	  cat(gro[[1]][1],sep='\n',fill=TRUE,labels = "{G1}:")
	  cat(gro[[1]][2],sep='\n',fill=TRUE,labels = "{G2}:")
  }
 }
