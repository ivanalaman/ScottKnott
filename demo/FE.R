##
## Example: Factorial Experiment (FE)
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

## Note: The factors are in uppercase and its levels in lowercase!

library(ScottKnott)
data(FE)

## From: design matrix (dm) and response variable (y)
## Main factor: N
sk1 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N*P*K', 
                   which='N'))
summary(sk1)
plot(sk1, title='Main effect: N')

## Main factor: P
sk2 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N*P*K',
                   which='P'))
summary(sk2)
plot(sk2, title='Main effect: P')

## Main factor: K
sk3 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N*P*K',
                   which='K'))
summary(sk3)
plot(sk3, title='Main effect: K')

## Nested: p1/N
nsk1 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:P', fl2=1))
summary(nsk1)
plot(nsk1, title='Effect: p1/N')

## Nested: p2/N
nsk2 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:P', fl2=2))
summary(nsk2)
plot(nsk2, title = 'Effect: p2/N')

## Nested: k1/N
nsk3 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:K', fl2=1))
summary(nsk3)
plot(nsk3, title='Effect: k1/N')

## Nested: k2/N
nsk4 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:K', fl2=2))
summary(nsk4)
plot(nsk4, title = 'Effect: k2/N')

## Nested: k1/P
nsk5 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='P:K', fl2=1))
summary(nsk5)
plot(nsk5, title='Effect: k1/P')

## Nested: k2/P
nsk6 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='P:K', fl2=2))
summary(nsk6)
plot(nsk6, title = 'Effect: k2/P')

## Nested: k1/p1/N
nsk7 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:P:K', fl2=1, fl3=1))
summary(nsk7)
plot(nsk7, title='Effect: k1/p1/N')

## Nested: k2/p2/N
nsk8 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                         which='N:P:K', fl2=2, fl3=2))
summary(nsk8)
plot(nsk8, title='Effect: k2/p2/N')

## Nested: k1/n1/P
nsk9 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P*N*K',
                         which='P:N:K', fl2=1, fl3=1))
summary(nsk9)
plot(nsk9, title='Effect: k1/n1/P')

## Nested: k2/n2/P
nsk8 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P*N*K',
                         which='P:N:K', fl2=2, fl3=2))
summary(nsk8)
plot(nsk8, title='Effect: k2/n2/P')

## Nested: p1/n1/K
nsk10 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K*N*P',
                          which='K:N:P', fl2=1, fl3=1))
summary(nsk10)
plot(nsk10, title='Effect: p1/n1/K')

## Nested: p2/n2/K
nsk11 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K*N*P',
                          which='K:N:P', fl2=2, fl3=2))
summary(nsk11)
plot(nsk11, title='Effect: p2/n2/K')


## From: data.frame
## Nested: k2/p1/N
nsk12 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                          which='N:P:K', fl2=1, fl3=2))
summary(nsk12)
plot(nsk12, title='Effect: k2/p1/N')

## Nested: k1/p2/N
nsk13 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                          which='N:P:K', fl2=2, fl3=1))
summary(nsk13)
plot(nsk13, title='Effect: k1/p2/N')


## From aov
nav1 <- with(FE, aov(y ~ blk + N*P*K , data=dfm))
summary(nav1)

## Main factor: N
nsk14 <- SK(nav1,
            which='N')
summary(nsk14)
plot(nsk14, title='Main effect: N')

## Nested: k1/P
nsk15 <- SK.nest(nav1,
                 which='P:K', fl2=1)
summary(nsk15)
plot(nsk15, title='Effect: k1/P')

## Nested: k2/p1/N
nsk16 <- SK.nest(nav1,
                 which='N:P:K', fl2=1, fl3=2)
summary(nsk16)
plot(nsk16, title='Effect: k2/p1/N')

# Changing the order of factors
nav2 <- with(FE, aov(y ~ blk + K*N*P, data=dfm))
summary(nav2)

## Nested: p1/n1/K
nsk17 <- SK.nest(nav2,
                 which='K:N:P', fl2=1, fl3=1)
summary(nsk17)
plot(nsk17, title='Effect: p1/n1/K')