##
## Examples: Completely Randomized Design (CRD)
##

## Input classes accepted: formula, aov, lm, and lmerMod.

## Example 1: a small experiment
library(ScottKnott)
data(CRD1)

## From: formula - balanced
sk1 <- with(
  CRD1,
  SK(y ~ x,
    data=dfm
  )
)
sk1
summary(sk1)

plot(sk1)

plot(sk1,
  dispersion='sd'
)

plot(sk1,
  dispersion='ci',
  d.col='red'
)

plot(sk1,
  dispersion='cip',
  d.col='red'
)

## From: formula - unbalanced
u_sk1 <- with(
  CRD1,
  SK(y ~ x,
    data=dfm[-1, ]
  )
)
u_sk1
summary(u_sk1)

## From: aov - balanced
av1 <- with(
  CRD1,
  aov(y ~ x,
    data=dfm
  )
)
summary(av1)

sk2 <- SK(av1)
sk2
summary(sk2)

## From: lm - unbalanced
u_lm1 <- with(
  CRD1,
  lm(y ~ x,
    data=dfm[-1, ]
  )
)
summary(u_lm1)

u_sk2 <- SK(u_lm1)
u_sk2
summary(u_sk2)

## Example 2: many treatment groups
data(CRD2)

## From: formula - balanced
sk3 <- with(
  CRD2,
  SK(y ~ x,
    data=dfm
  )
)
plot(sk3,
  id.las=2,
  yl=FALSE,
  dispersion='sd',
  d.lty=3,
  d.col='red'
)

## From: formula - unbalanced
u_sk3 <- with(
  CRD2,
  SK(y ~ x,
    data=dfm[-1, ]
  )
)
plot(u_sk3,
  id.las=2,
  yl=FALSE,
  dispersion='sd',
  d.lty=3,
  d.col='red'
)

## From: aov - balanced
av2 <- with(
  CRD2,
  aov(y ~ x,
    data=dfm
  )
)
summary(av2)

sk4 <- SK(av2)
plot(sk4,
  id.las=2,
  yl=FALSE,
  dispersion='sd',
  d.lty=4,
  d.col='darkgreen'
)

## From: lm - unbalanced
u_lm2 <- with(
  CRD2,
  lm(y ~ x,
    data=dfm[-1, ]
  )
)
summary(u_lm2)

u_sk4 <- SK(u_lm2)

plot(u_sk4,
  id.las=2,
  yl=FALSE
)
