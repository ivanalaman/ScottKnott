m.infos.lm <- function(x,
                       my,
                       forminter,
                       which,
                       sig.level,
                       aux_mt,
                       ...)
{

  aux_m.inf <- aggregate(forminter,
                         data = x$model,
                         function(x) c(min = min(x),
                                       max = max(x),
                                       sd  = sd(x),
                                       se  = sd(x)/length(x)))

  aux_m.inf1 <- data.frame(groups    = aux_m.inf[names(aux_m.inf)!=my],
                           means     = with(aux_mt,estimate),
                           aux_m.inf[[my]][,1:2],
                           'linf_sd' = with(aux_mt,estimate) - aux_m.inf[[my]][,3],
                           'lsup_sd' = with(aux_mt,estimate) + aux_m.inf[[my]][,3],
                           'linf_se' = with(aux_mt,estimate) - abs(qt(sig.level,with(aux_mt,statistic)))*aux_m.inf[[my]][,4],
                           'lsup_se' = with(aux_mt,estimate) + abs(qt(sig.level,with(aux_mt,statistic)))*aux_m.inf[[my]][,4], 
                           'linf_sepool' = with(aux_mt,estimate) - abs(qt(sig.level,with(aux_mt,statistic)))*with(aux_mt,std.error),
                           'lsup_sepool' = with(aux_mt,estimate) + abs(qt(sig.level,with(aux_mt,statistic)))*with(aux_mt,std.error))

  aux_m.inf2 <- aux_m.inf1[order(aux_m.inf1[['means']],
                                 decreasing = TRUE),]

  m.inf <- list(Means = aux_m.inf2[,c(1:2)],
                mm = aux_m.inf2[,c(1,3:4)],
                sd = aux_m.inf2[,c(1,5:6)],
                ic = aux_m.inf2[,c(1,7:8)],
                icpool = aux_m.inf2[,c(1,9:10)])
}
