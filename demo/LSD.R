##
## Example: Latin Squares Design (LSD)
##

## Input classes accepted: formula, aov, lm, and lmerMod.

library(ScottKnott)
data(LSD)

## From: formula - testing: tra
sk1 <- with(
  LSD,
  SK(y ~ rows + cols + tra,
    data=dfm,
    which='tra',
    sig.level=0.052
  )
)
summary(sk1)

## From: formula - testing: rows
sk2 <- with(
  LSD,
  SK(y ~ rows + cols + tra,
    data=dfm,
    which='rows'
  )
)
summary(sk2)

## From: aov
av1 <- with(
  LSD,
  aov(y ~ rows + cols + tra,
    data=dfm
  )
)
summary(av1)

## From: aov - testing: tra
sk3 <- SK(av1,
          which='tra',
          sig.level=0.052)
summary(sk3)

## From: aov - testing: cols
sk4 <- SK(av1,
          which='cols')
summary(sk4)
