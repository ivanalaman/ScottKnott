##
## Example: Split-Plot Experiment (SPE)
##

## Input classes accepted: formula, aov, lm, and lmerMod.

## Note: uppercase letters denote factors; lowercase letters denote their levels.

library(ScottKnott)
data(SPE)

## From: formula
## Main factor: SP
sk1 <- with(
  SPE,
  SK(y ~ blk + P*SP + Error(blk/P),
    data=dfm,
    which='SP'
  )
)
summary(sk1)

## Nested: p1/SP
sk2 <- with(
  SPE,
  SK(y ~ blk + P*SP + Error(blk/P),
    data=dfm,
    which='P:SP',
    fl1=1
  )
)
summary(sk2)
plot(sk2,
  dispersion='sd',
  d.col='red',
  d.lty=3
)

## Nested: sp1/P - it is necessary to specify how to combine the error terms
sk3 <- with(
  SPE,
  SK(y ~ blk + P*SP + Error(blk/P),
    data=dfm,
    which='SP:P',
    error='Within/blk:P',
    fl1=1
  )
)
summary(sk3)

## From: lm
lm1 <- with(
  SPE,
  lm(y ~ blk*P + P*SP,
    data=dfm
  )
)

## Nested: p1/SP
sk4 <- SK(lm1,
          which='P:SP',
          fl1=1)
summary(sk4)

## Nested: sp1/P - it is necessary to specify how to combine the error terms
sk5 <- SK(lm1,
          which='SP:P',
          error='Within/blk:P',
          fl1=1)
summary(sk5)

## From: aov
av1 <- with(
  SPE,
  aov(y ~ blk + P*SP + Error(blk/P),
    data=dfm
  )
)
summary(av1)

## Main factor: SP
sk6 <- SK(av1,
          which='SP',
          sig.level=0.1)
summary(sk6)

## Main factor: P
## It is necessary to specify the appropriate error term for the test
sk7 <- SK(av1,
          which='P',
          error='blk:P')
summary(sk7)

## Nested: p1/SP - testing SP within level one of P
sk8 <- SK(av1,
          which='P:SP',
          fl1=1)
summary(sk8)

## Nested: p2/SP
sk9 <- SK(av1,
          which='P:SP',
          fl1=2)
summary(sk9)

## Nested: p3/SP
sk10 <- SK(av1,
           which='P:SP',
           fl1=3)
summary(sk10)

## Nested: sp1/P - it is necessary to specify how to combine the error terms
sk11 <- SK(av1,
           which='SP:P',
           error='Within/blk:P',
           fl1=1)
summary(sk11)
