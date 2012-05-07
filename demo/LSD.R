##
## Example: Latin Squares Design (LSD)
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

library(ScottKnott)
data(LSD)

## From: design matrix (dm) and response variable (y)
sk1 <- with(LSD, SK(x=dm, y=y, model='y ~ rows + cols + tra',
                    which='tra'))
summary(sk1)
plot(sk1)

## From: data.frame
sk2 <- with(LSD, SK(x=dfm, model='y ~ rows + cols + tra',
                    which='tra'))
summary(sk2)
plot(sk2, title='Factor levels')

## From: aov
av1 <- with(LSD, aov(y ~ rows + cols + tra, data=dfm))
summary(av1)

sk3 <- SK(av1,
          which='tra')
summary(sk3)
plot(sk3, title='Factor levels')

## From: aov, sig.level=8%
sk4 <- SK(av1,
          which='tra', sig.level=.08)
summary(sk4)
plot(sk4, title='Factor levels (sig.level=0.08)')