##
## S3 method to sumarize 'SK' object
##
summary.SK <- function(object, ...)
{
	if(!inherits(object,
		     'SK'))
		stop("Use only with \"SK\" objects!")

	cat('Goups of means at sig.level =',
	    object$out$Sig.level,
	    '\n')

	print(object$out$Result)	

}
