m.infos.nest.lmerMod <- function(x,
                                 my,
                                 forminter,
                                 which,
                                 fl1,
                                 fl2,
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

  aux_m.inf1 <- data.frame(aux_m.inf[names(aux_m.inf) != my],
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

  nf1 <- unlist(strsplit(which,
    split = ":"
  ))[1] # nome do primeiro fator do which

  nf2 <- unlist(strsplit(which,
    split = ":"
  ))[2] # nome do segundo fator do which

  nf3 <- unlist(strsplit(which,
    split = ":"
  ))[3] # nome do terceiro fator do which

  if (is.null(fl2)) {
    f2 <- levels(x@frame[[nf1]])[fl1] # corresponde ao fator onde se esta fazendo o desdobramento!

    aux_m.inf21 <- subset(
      aux_m.inf2,
      eval(parse(text = nf1)) == f2
    ) # pegando as medias de interesse

    m.inf <- list(
      Means = aux_m.inf21[, c(1:3)],
      mm = aux_m.inf21[, c(1:2, 4:5)],
      sd = aux_m.inf21[, c(1:2, 6:7)],
      ic = aux_m.inf21[, c(1:2, 8:9)],
      icpool = aux_m.inf21[, c(1:2, 10:11)]
    )
  } else {
    f2 <- levels(x@frame[[nf2]])[fl2]

    f3 <- levels(x@frame[[nf1]])[fl1]

    aux_m.inf21 <- subset(
      aux_m.inf2,
      eval(parse(text = nf1)) == f3 & eval(parse(text = nf2)) == f2
    ) # pegando as medias de interesse

    m.inf <- list(
      Means = aux_m.inf21[, c(1:4)],
      mm = aux_m.inf21[, c(1:3, 5:6)],
      sd = aux_m.inf21[, c(1:3, 7:8)],
      ic = aux_m.inf21[, c(1:3, 9:10)],
      icpool = aux_m.inf21[, c(1:3, 11:12)]
    )
  }
}
