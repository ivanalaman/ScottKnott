##
## Example: Randomized Complete Block Design (RCBD)
##

## The parameters can be: formula, aov, lm or lmer.

library(ScottKnott)
data(RCBD)

## From: data.frame (dfm), which='tra'
sk1 <- with(RCBD,
            SK(y ~ blk + tra,
               dfm,
               which='tra'))
summary(sk1)
plot(sk1,
     di='sd',
     d.col='red',
     d.lty=3)

## From: formula, which='blk' implicit (due to be the first arg of the model)
sk2 <- with(RCBD,
            SK(y ~ blk + tra,
               dfm))
summary(sk2)
plot(sk2,,
     di='sd',
     d.col='red',
     d.lty=3)

av1 <- with(RCBD,
            aov(y ~ blk + tra,
                data=dfm))
summary(av1)

## From: aov, which='blk' implicit (due to be the first arg of the model)
sk3 <- SK(av1)
summary(sk3)

## From: aov, which='blk' explicit
sk4 <- SK(x=av1,
          which='blk')
summary(sk4)

## From: aov, which='tra' explicit
sk5 <- SK(x=av1,
          which='tra')
summary(sk5)
