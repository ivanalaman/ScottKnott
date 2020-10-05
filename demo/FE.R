##
## Example: Factorial Experiment (FE)
##

## The parameters can be: formula, aov, lm or lmer.

## Note: Upper case for factors and lowercase for levels

library(ScottKnott)
data(FE)

## From: formula
## Nested: k1/p2/N
## The indices (1, 2, ...) are used to set the level of the factor
sk1 <- with(FE,
            SK(y ~ blk + N*P*K,
               dfm,
               which='K:P:N',
               fl1=1,
               fl2=2))
summary(sk1)

## Nested: k2/p1/N
sk2 <- with(FE,
            SK(y ~ blk + N*P*K,
               dfm,
               which='K:P:N',
               fl1=2,
               fl2=1))
summary(sk2)

## From aov
av1 <- with(FE,
            aov(y ~ blk + N*P*K ,
                data=dfm))
summary(av1)

## Main factor: N
sk3 <- SK(av1,
          which='N')
summary(sk3)

## Nested: k1/P
sk4 <- SK(av1,
          which='K:P',
          fl1=1)
summary(sk4)

## Nested: k1/p2/N
sk4 <- SK(av1,
          which='K:P:N',
          fl1=1,
          fl2=2)
summary(sk4)

# Changing the order of factors (for test only)
av2 <- with(FE,
            aov(y ~ blk + K*N*P,
                data=dfm))
summary(av2)

## Nested: p1/n1/K
sk5 <- SK(av2,
          which='P:N:K',
          fl1=1,
          fl2=1)
summary(sk5)
