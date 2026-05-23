##
## Example: Randomized Complete Block Design (RCBD)
##

## Input classes accepted: formula, aov, lm, and lmerMod.

library(ScottKnott)
data(RCBD)

## From: formula - which='tra'
sk1 <- with(
  RCBD,
  SK(y ~ blk + tra,
    data=dfm,
    which='tra'
  )
)
summary(sk1)
plot(sk1,
  dispersion='sd',
  d.col='red',
  d.lty=3
)

## From: formula - which='blk' is implicit (first term in the model formula)
sk2 <- with(
  RCBD,
  SK(y ~ blk + tra,
    data=dfm
  )
)
summary(sk2)
plot(sk2,
  dispersion='sd',
  d.col='red',
  d.lty=3
)

av1 <- with(
  RCBD,
  aov(y ~ blk + tra,
    data=dfm
  )
)
summary(av1)

## From: aov - which='blk' is implicit (first term in the model formula)
sk3 <- SK(av1)
summary(sk3)

## From: aov - which='blk' explicit
sk4 <- SK(x=av1,
          which='blk')
summary(sk4)

## From: aov - which='tra' explicit
sk5 <- SK(x=av1,
          which='tra')
summary(sk5)
