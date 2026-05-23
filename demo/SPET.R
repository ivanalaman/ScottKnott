##
## Example: Split-Plot Experiment in Time (SPET)
##

## Input classes accepted: formula, aov, lm, and lmerMod.

## Note: uppercase letters denote factors; their levels are in lowercase.

library(ScottKnott)
data(SPET)

## From: formula
## Main factor: year
sk1 <- with(
  SPET,
  SK(y ~ blk + tra*year + Error(blk/tra),
    data=dfm,
    which='year'
  )
)
summary(sk1)

## Nested: crotjuncea/year
sk2 <- with(
  SPET,
  SK(y ~ blk + tra*year + Error(blk/tra),
    data=dfm,
    which='tra:year',
    fl1=2
  )
)
summary(sk2)

## Nested: year1/tra - it is necessary to specify how to combine the error terms
sk3 <- with(
  SPET,
  SK(y ~ blk + tra*year + Error(blk/tra),
    data=dfm,
    which='year:tra',
    error='Within/blk:tra',
    fl1=1
  )
)
summary(sk3)

## From: lm
lm1 <- with(
  SPET,
  lm(y ~ blk*tra + tra*year,
    data=dfm
  )
)

## Nested: tra1/year
sk4 <- SK(lm1,
          which='tra:year',
          fl1=1)
summary(sk4)

## Nested: year1/tra - it is necessary to specify how to combine the error terms
sk5 <- SK(lm1,
          which='year:tra',
          error='Within/blk:tra',
          fl1=1)
summary(sk5)

## Nested: year2/tra - it is necessary to specify how to combine the error terms
sk6 <- SK(lm1,
          which='year:tra',
          error='Within/blk:tra',
          fl1=2)
summary(sk6)

## From: aov
av1 <- with(
  SPET,
  aov(y ~ blk + tra*year + Error(blk/tra),
    data=dfm
  )
)
summary(av1)

## Main factor: year
sk7 <- SK(av1,
          which='year')
summary(sk7)

## Main factor: tra
## It is necessary to specify the appropriate error term for the test
sk8 <- SK(av1,
          which='tra',
          error='blk:tra')
summary(sk8)

## Nested: crotjuncea/year
sk9 <- SK(av1,
          which='tra:year',
          fl1=2)
summary(sk9)

## Nested: guandu/year
sk10 <- SK(av1,
           which='tra:year',
           fl1=4)
summary(sk10)

## Nested: year1/tra - it is necessary to specify how to combine the error terms
sk11 <- SK(av1,
           which='year:tra',
           error='Within/blk:tra',
           fl1=1)
summary(sk11)

op <- par(mar=c(6, 3, 3, 2))
plot(sk10,
     id.las=2,
     xlab='',
     dispersion='sd',
     d.col='red',
     d.lty=3)

## Nested: year2/tra - it is necessary to specify how to combine the error terms
sk12 <- SK(av1,
           which='year:tra',
           error='Within/blk:tra',
           fl1=2)
summary(sk12)
op <- par(mar=c(7, 3, 3, 2))
plot(sk12,
     id.las=2,
     xlab='',
     dispersion='sd',
     d.col='red',
     d.lty=3)
par(op)
