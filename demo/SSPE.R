##
## Example: Split-split-plot Experiment (SSPE)
##

## The parameters can be: formula, aov, lm or lmer.

## Note: Upper case for factors and lowercase for levels

library(ScottKnott)
data(SSPE)

## From: formula
## Main factor: P
## It is necessary to inform the appropriate error for the test
sk1 <- with(SSPE,
            SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
               dfm,
               which='P',
               error='blk:P'))
summary(sk1)

## Nested: p2/SP
## It is necessary to inform the appropriate error for the test
sk2 <- with(SSPE,
            SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
               dfm,
               which='P:SP',
               error='blk:P:SP',
               fl1=2))
summary(sk2)

## Nested: p2/SSP
sk3 <- with(SSPE,
            SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
               dfm,
               which='P:SSP',
               fl1=2))
summary(sk3)
plot(sk3,
     di='sd',
     d.col='red',
     d.lty=3)


## From: lm
lm1 <- with(SSPE,
            lm(y ~ blk*P + blk*P*SP + P*SP*SSP,
               data=dfm))
summary(lm1)

## Main factor: P
## It is necessary to inform the appropriate error for the test
sk4 <- SK(lm1,
          which='P',
          error='blk:P')
summary(sk4)

## Main factor: SP
sk5 <- SK(lm1,
          which='SP',
          error='blk:P:SP')
summary(sk5)

## Main factor: SSP
sk6 <- SK(lm1,
          which='SSP')
summary(sk6)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
sk7 <- SK(lm1,
          which='P:SP',
          error='blk:P:SP',
          fl1=1)
summary(sk7)

## From: aov
av1 <- with(SSPE,
            aov(y ~  blk + P*SP*SSP + Error(blk/P/SP),
                data=dfm))
summary(av1)

## Main factor: P 
## It is necessary to inform the appropriate error for the test
sk8 <- SK(av1,
          which='P',
          error='blk:P')
summary(sk8)

## Main factor: SSP
sk9 <- SK(av1,
          which='SSP')
summary(sk9)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
sk10 <- SK(av1,
           which='P:SP',
           error='blk:P:SP',
           fl1=1)
summary(sk10)

## Nested: p2/SP
sk11 <- SK(av1,
           which='P:SP',
           error='blk:P:SP',
           fl1=2)
summary(sk11)

## Nested: Pi/SPi/SSP (at various levels of P and SP)
sk12 <- SK(av1,
           which='P:SP:SSP',
           fl1=1,
           fl2=1)
summary(sk12)

sk13 <- SK(av1,
           which='P:SP:SSP',
           fl1=2,
           fl2=1)
summary(sk13)

sk14 <- SK(av1,
           which='P:SP:SSP',
           fl1=3,
           fl2=3)
summary(sk14)

sk15 <- SK(av1,
           which='P:SP:SSP',
           fl1=2,
           fl2=3)
summary(sk15)

## Nested: sp1/P
## It is necessary to inform the appropriate error for the test
sk16 <- SK(av1,
           which='SP:P',
           error='blk:P:SP/blk:P',
           fl1=1)

summary(sk16)

## Nested: ssp1/SP
sk17 <- SK(av1,
           which='SSP:SP',
           error='Within/blk:P:SP',
           fl1=1)
summary(sk17)

## Nested: ssp1/sp1/P
## It is necessary to inform the appropriate error for the test
sk18 <- SK(av1,
           which='SSP:SP:P',
           error='Within/blk:P:SP/blk:P',
           fl1=1,
           fl2=1)
summary(sk18)
