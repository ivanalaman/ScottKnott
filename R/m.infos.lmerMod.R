m.infos.lmerMod <- function(x,
                            my,
                            forminter,
                            which,
                            sig.level,
                            aux_mt,
                            ...) {
  ## emmeans uses 'lower.CL'/'upper.CL' for df-based CIs and
  ## 'asymp.LCL'/'asymp.UCL' for asymptotic (infinite-df) CIs.
  ci_lower <- intersect(c("lower.CL", "asymp.LCL"), names(aux_mt))[1]
  ci_upper <- intersect(c("upper.CL", "asymp.UCL"), names(aux_mt))[1]

  aux_m.inf <- aggregate(forminter,
    data = x@frame,
    function(x) {
      c(
        min = min(x),
        max = max(x),
        sd = sd(x),
        se = sd(x) / length(x)
      )
    }
  )

  aux_m.inf1 <- data.frame(
    groups = aux_m.inf[names(aux_m.inf) != my],
    means = with(aux_mt, emmean),
    aux_m.inf[[my]][, 1:2],
    "linf_sd" = with(aux_mt, emmean) - aux_m.inf[[my]][, 3],
    "lsup_sd" = with(aux_mt, emmean) + aux_m.inf[[my]][, 3],
    "linf_se" = with(aux_mt, emmean) - abs(qt(sig.level, with(aux_mt, emmean / SE))) * aux_m.inf[[my]][, 4],
    "lsup_se" = with(aux_mt, emmean) + abs(qt(sig.level, with(aux_mt, emmean / SE))) * aux_m.inf[[my]][, 4],
    "linf_sepool" = aux_mt[[ci_lower]],
    "lsup_sepool" = aux_mt[[ci_upper]]
  )

  aux_m.inf2 <- aux_m.inf1[order(aux_m.inf1[["means"]],
    decreasing = TRUE
  ), ]

  m.inf <- list(
    Means = aux_m.inf2[, c(1:2)],
    mm = aux_m.inf2[, c(1, 3:4)],
    sd = aux_m.inf2[, c(1, 5:6)],
    ic = aux_m.inf2[, c(1, 7:8)],
    icpool = aux_m.inf2[, c(1, 9:10)]
  )
}
