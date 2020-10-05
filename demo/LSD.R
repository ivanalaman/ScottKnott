##
## Example: Latin Squares Design (LSD)
##

## The parameters can be: formula, aov, lm or lmer.

library(ScottKnott)
data(LSD)

## From: formula
## Testing tra
sk1 <- with(LSD,
            SK(y ~ rows + cols + tra,
               dfm,
               which='tra',
               sig.level=.052))
summary(sk1)

## From: formula
## Testing rows
sk2 <- with(LSD,
            SK(y ~ rows + cols + tra,
               dfm,
               which='rows'))
summary(sk2)

## From: aov
av1 <- with(LSD,
            aov(y ~ rows + cols + tra,
                data=dfm))
summary(av1)

## From: aov
## Testing tra
sk3 <- SK(av1,
          which='tra',
          sig.level=.052)
summary(sk3)

## From: aov
## Testing cols
sk4 <- SK(av1,
          which='cols')
summary(sk4)
