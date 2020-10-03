##
## S3 method to design matrix and response variable or data.frame objects
##

SK.default <- function(x,
                           ...)
{
  stop(paste("class", 
             class(x), 
             "objects are not valid for SK" ))
}
