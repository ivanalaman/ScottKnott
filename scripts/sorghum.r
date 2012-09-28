load('./../data/sorghum.rda')
# 
#MODELOS BIFATORIAIS
#Tentando para qualquer delineamento
load('./../data/FE.rda')
dafe <- FE$dfm
summary(dafe)
dat <- dafe[,-3]
summary(dat)
# 
#DBC
modfe <- aov(y ~ blk + N*P,data=dat)
summary(modfe)
source('./../R/SK.nest.aov.R')
# source('./../R/SK.nest.aovlist.R') 
# source('./../R/summary.SK.nest.R') 
# 
# 
# library(debug)
# mtrace(SK.nest.aovlist)
# mtrace.off()
# 
#P0/N
# print(ScottKnott::SK.nest.aov(modfe,which='N:P',fl2=1))#ok, no entanto, filosofia de aninhamento oposto ao que o R adota
# 
print(SK.nest.aov(modfe, which='P:N',fl1=1))#ok e mantendo a filosofia de aninhamento do R.
# 
#P1/N
# print(ScottKnott::SK.nest.aov(modfe,which='N:P',fl2=2))#ok
# 
# print(SK.nest.aov(modfe, which='P:N',fl1=2))#ok
# 
#invertendo
#N0/P
# print(ScottKnott::SK.nest.aov(update(modfe, ~blk + P*N),
#                               which='P:N',fl2=1))#erro!precisa de outro modelo
# 
# print(SK.nest.aov(modfe, which='N:P',fl1=1))#OKKKKKKKKKKKKK
# 
#N1/P
# print(ScottKnott::SK.nest.aov(modfe,
#                               which='P:N',fl2=2))#erro!precisa de outro modelo
# 
# print(SK.nest.aov(modfe, which='N:P',fl1=2))#OKKKKKKKKKKKKK
# 
#DIC
# modfeDIC <- aov(y ~ P*N,data=dat)
# summary(modfeDIC)
# 
#N0/P
# print(ScottKnott::SK.nest.aov(modfeDIC,which='P:N',fl2=1))#ok
# 
# print(SK.nest.aov(modfeDIC, which='N:P',fl1=1))#ok
# 
#N1/P
# print(ScottKnott::SK.nest.aov(modfeDIC,which='P:N',fl2=2))#ok
# 
# print(SK.nest.aov(modfeDIC, which='N:P',fl1=2))#ok
# 
#invertendo
#P1/N
# print(ScottKnott::SK.nest.aov(update(modfeDIC,~N*P),
#                               which='N:P',fl2=1))#erro!precisa de outro modelo
# 
# print(SK.nest.aov(modfeDIC, which='P:N',fl1=1))#OKKKKKKKKKKKKK
# 
#P2/N
# print(ScottKnott::SK.nest.aov(update(modfeDIC,~N*P),
#                               which='N:P',fl2=2))#erro!precisa de outro modelo
# 
# print(SK.nest.aov(modfeDIC, which='P:N',fl1=2))#OKKKKKKKKKKKKK
# 
#MODELOS TRIFATORIAIS
#Avaliando as interações duplas
modfetri <- aov(y ~ blk + P*K*N,data=dafe)
summary(modfetri)
# 
#Avaliando as interações duplas
#K0/P
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K',fl2=1))#antes
# 
#$AGORA!!
# SK.nest.aov(modfetri, which='K:P',fl1=1)
# 
#K1/P	
# SK.nest.aov(modfetri, which='K:P',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K',fl2=2))#Primeira mudança no modelo
# 
#P0/K
# SK.nest.aov(modfetri, which='P:K',fl1=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri,~blk + K*P*N),
#                               which='K:P',fl2=1))#antes
# 
#P1/K
# SK.nest.aov(modfetri, which='P:K',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri,~blk + K*P*N),
#                               which='K:P',fl2=2))#antes
#  
#K0/N
# SK.nest.aov(modfetri, which='K:N',fl1=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K',fl2=1))#Segunda mudança no modelo
# 
#K1/N
# SK.nest.aov(modfetri, which='K:N',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K',fl2=2))#antes
# 
#N0/K
# SK.nest.aov(modfetri, which='N:K',fl1=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N',fl2=1))#Terceira mudança no modelo
# 
#N1/K
# SK.nest.aov(modfetri, which='N:K',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N',fl2=2))#antes
# 
#P0/N
# SK.nest.aov(modfetri, which='P:N',fl1=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P',fl2=1))#Quarta mudança no modelo
# 
#P1/N
# SK.nest.aov(modfetri, which='P:N',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P',fl2=2))#antes
# 
#N0/P
# SK.nest.aov(modfetri, which='N:P',fl1=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*N*K),
# 			      which='P:N',fl2=1))#Quinta mudança no modelo
# 
#N1/P
# SK.nest.aov(modfetri, which='N:P',fl1=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*N*K),
# 			      which='P:N',fl2=2))#antes
# 
#Para um modelo trifatorial
#K0/P0/N
# SK.nest.aov(modfetri, which='K:P:N',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P:K',fl2=1,fl3=1))#antes
# 
#K1/P0/N
# SK.nest.aov(modfetri, which='K:P:N',fl1=2,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P:K',fl2=1,fl3=2))#antes
# 
#K0/P1/N
# SK.nest.aov(modfetri, which='K:P:N',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P:K',fl2=2,fl3=1))#antes
# 
#K1/P1/N
# SK.nest.aov(modfetri, which='K:P:N',fl1=2,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*P*K),
# 			      which='N:P:K',fl2=2,fl3=2))#antes
# 
#P0/K0/N
# SK.nest.aov(modfetri, which='P:K:N',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K:P',fl2=1,fl3=1))#antes
# 
#P1/K0/N
# SK.nest.aov(modfetri, which='P:K:N',fl1=2,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K:P',fl2=1,fl3=2))#antes
# 
#P0/K1/N
# SK.nest.aov(modfetri, which='P:K:N',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K:P',fl2=2,fl3=1))#antes
# 
#P1/K1/N
# SK.nest.aov(modfetri, which='P:K:N',fl1=2,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + N*K*P),
# 			      which='N:K:P',fl2=2,fl3=2))#antes
# 
#N0/P0/K
# SK.nest.aov(modfetri, which='N:P:K',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~blk + K*P*N), 
# 				   which='K:P:N',fl2=1,fl3=1))#antes
# 
#N1/P0/K
# SK.nest.aov(modfetri, which='N:P:K',fl1=2,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*P*N), 
# 				   which='K:P:N',fl2=1,fl3=2))#antes
# 
#N0/P1/K
# SK.nest.aov(modfetri, which='N:P:K',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*P*N), 
# 				   which='K:P:N',fl2=2,fl3=1))#antes
# 
N1/P1/K
SK.nest.aov(modfetri, which='N:P:K',fl1=2,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*P*N), 
# 				   which='K:P:N',fl2=2,fl3=2))#antes
# 
P0/N0/K
# SK.nest.aov(modfetri, which='P:N:K',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N:P',fl2=1,fl3=1))#antes
# 
P0/N1/K
# SK.nest.aov(modfetri, which='P:N:K',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N:P',fl2=2,fl3=1))#antes
# 
P1/N0/K
# SK.nest.aov(modfetri, which='P:N:K',fl1=2,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N:P',fl2=1,fl3=2))#antes
# 
P1/N1/K
# SK.nest.aov(modfetri, which='P:N:K',fl1=2,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + K*N*P),
# 			      which='K:N:P',fl2=2,fl3=2))#antes
# 
K0/N0/P
# SK.nest.aov(modfetri, which='K:N:P',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~blk + P*N*K), 
# 			      which='P:N:K',fl2=1,fl3=1))#antes
# 
K0/N1/P
# SK.nest.aov(modfetri, which='K:N:P',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~blk + P*N*K),
# 			      which='P:N:K',fl2=2,fl3=1))#antes
# 
K1/N0/P
# SK.nest.aov(modfetri, which='K:N:P',fl1=2,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~blk + P*N*K),
# 			      which='P:N:K',fl2=1,fl3=2))#antes
# 
K1/N1/P
# SK.nest.aov(modfetri, which='K:N:P',fl1=2,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*N*K),
# 			      which='P:N:K',fl2=2,fl3=2))#antes
# 
N0/K0/P
# SK.nest.aov(modfetri, which='N:K:P',fl1=1,fl2=1)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K:N',fl2=1,fl3=1))#antes
# 
N0/K1/P
# SK.nest.aov(modfetri, which='N:K:P',fl1=1,fl2=2)
# 
# print(ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K:N',fl2=2,fl3=1))#antes
# 
# 
N1/K0/P
# N1K0P <- SK.nest.aov(modfetri, which='N:K:P',fl1=2,fl2=1)
# N1K0P1 <- (ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K:N',fl2=1,fl3=2))#antes
# 
# summary.SK.nest(N1K0P)
# summary(N1K0P1)
# 
# par(mfrow=c(1,2))
# plot.SK(N1K0P)
# plot(N1K0P1)
# 
# 
N1/K1/P
# N1K1P <- SK.nest.aov(modfetri, which='N:K:P',fl1=2,fl2=2)
# N1K1P1 <- (ScottKnott::SK.nest.aov(update(modfetri, ~ blk + P*K*N),
# 			      which='P:K:N',fl2=2,fl3=2))#antes
# 
# summary.SK.nest(N1K1P)
# summary(N1K1P1)
# 
# par(mfrow=c(1,2))
# plot.SK(N1K1P)
# plot(N1K1P1)
# 
Testando a função com outra base de dados
# library(easyanova)
# data(data5)
# summary(data5)
# data5$Vitamin_1 <- factor(data5$Vitamin_1)
# data5$Vitamin_2 <- factor(data5$Vitamin_2)
# 
# modgeral1 <- aov(Gains ~ Vitamin_1*Vitamin_2, data=data5)
# summary(modgeral1)
# 
Vit1_0/Vitamin_2
# vit10vit2  <- SK.nest.aov(modgeral1, which='Vitamin_1:Vitamin_2',fl1=1)
# vit10vit2_1 <- (ScottKnott::SK.nest.aov(update(modgeral1, ~Vitamin_2*Vitamin_1),
# 					       which='Vitamin_2:Vitamin_1',fl2=1))#antes
# 
# summary(vit10vit2)
# summary(vit10vit2_1)
# 
# par(mfrow=c(1,2))
# plot(vit10vit2)
# plot(vit10vit2_1)
# 
Vit1_4/Vitamin_2
# vit14vit2  <- SK.nest.aov(modgeral1, which='Vitamin_1:Vitamin_2',fl1=2)
# vit14vit2_1 <- (ScottKnott::SK.nest.aov(update(modgeral1, ~Vitamin_2*Vitamin_1),
# 					       which='Vitamin_2:Vitamin_1',fl2=2))#antes
# 
# 
# summary(vit14vit2)
# summary(vit14vit2_1)
# 
# par(mfrow=c(1,2))
# plot(vit14vit2)
# plot(vit14vit2_1)
# 
FINISHEDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD FATORIAL         #############
# 
PARCELAS SUBDIVIDIDAS
# load('./../data/SPE.rda')
# source('./../R/SK.nest.aovlist.R')
# 
# dasp <- SPE$dfm
# modspe <- aov(y ~ blk + Error(blk/P) + SP*P,data=dasp)
# summary(modspe)
# modspe[['blk:P']]$df.residual
Desdobrando
P1/SP
# 
# print(ScottKnott::SK.nest.aovlist(modspe,which='SP:P',error='Within',fl2=1))
# 
# print(SK.nest.aovlist(modspe,which='P:SP',error='Within',fl1=1))#OKK
# 
P2/SP
# 
# print(ScottKnott::SK.nest.aovlist(modspe,which='SP:P',error='Within',fl2=2))
# 
# print(SK.nest.aovlist(modspe,which='P:SP',error='Within',fl1=2))#OKK
#  
P3/SP
# print(ScottKnott::SK.nest.aovlist(modspe,which='SP:P',error='Within',fl2=3))
# 
# print(SK.nest.aovlist(modspe,which='P:SP',error='Within',fl1=3))#OKK 
# 
SP1/P
# modspe2 <- aov(y ~ blk + Error(blk/P) + P*SP,data=dasp) 
# print(ScottKnott::SK.nest.aovlist(modspe2,which='P:SP',error='Within',fl2=1))#erro
# 
# print(SK.nest.aovlist(modspe,which='SP:P',error='Within',fl1=1))#OKK 
# 
SP2/P
# print(ScottKnott::SK.nest.aovlist(modspe2,which='P:SP',error='Within',fl2=2))#erro
# 
# print(SK.nest.aovlist(modspe,which='SP:P',error='Within',fl1=2))#OKK 
#  
SP3/P
# print(ScottKnott::SK.nest.aovlist(modspe2,which='P:SP',error='Within',fl2=3))#erro
# 
# print(SK.nest.aovlist(modspe,which='SP:P',error='Within',fl1=3))#OKK 
#  
SP4/P
# print(ScottKnott::SK.nest.aovlist(modspe2,which='P:SP',error='Within',fl2=4))#erro
# 
# print(SK.nest.aovlist(modspe,which='SP:P',error='Within',fl1=4))#OKK 
# 
PARCELAS SUBSUBDIVIDIDAS
# load('./../data/SSPE.rda')
# 
# dassp <- SSPE$dfm
# modsspe <- aov(y ~ blk + P*SP*SSP + Error(blk/P/SP),data=dassp)
# summary(modsspe)
# 
Avaliando as interações duplas
SP1/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='SP:P',error='blk:P:SP',fl1=1))#OK
# 
SP2/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='SP:P',error='blk:P:SP',fl1=2))#errooo
#  
SP3/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='SP:P',error='blk:P:SP',fl1=3))#errooo
#   
P1/SP
# modsspe2 <- aov(y ~ blk + SP*SSP*P + Error(blk/P/SP),data=dassp) 
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SP:P',error='blk:P:SP',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl1=1))#errooo
# 
P2/SP
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SP:P',error='blk:P:SP',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl1=2))#errooo
#  
P3/SP
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SP:P',error='blk:P:SP',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='P:SP',error='blk:P:SP',fl1=3))#errooo
# 
SSP1/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:P',error='Within',fl1=1))#errooo
#  
SSP2/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:P',error='Within',fl1=2))#errooo
#  
SSP3/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:P',error='Within',fl1=3))#errooo
#  
SSP4/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl2=4))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:P',error='Within',fl1=4))#errooo
#  
SSP5/P
# print(ScottKnott::SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl2=5))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:P',error='Within',fl1=5))#errooo
#  
P1/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SSP:P',error='Within',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl1=1))#errooo
#  
P2/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SSP:P',error='Within',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl1=2))#errooo
#  
P3/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe2,which='SSP:P',error='Within',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='P:SSP',error='Within',fl1=3))#errooo
# 
SSP1/SP
# print(ScottKnott::SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:SP',error='Within',fl1=1))#errooo
#  
SSP2/SP
# print(ScottKnott::SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:SP',error='Within',fl1=2))#errooo
#  
SSP3/SP
# print(ScottKnott::SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:SP',error='Within',fl1=3))#errooo
#  
SSP4/SP
# print(ScottKnott::SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl2=4))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:SP',error='Within',fl1=4))#errooo
#  
SSP5/SP
# print(ScottKnott::SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl2=5))
# 
# print(SK.nest.aovlist(modsspe,which='SSP:SP',error='Within',fl1=5))#errooo
#  
SP1/SSP
# modsspe3 <- aov(y ~ blk + SSP*SP*P + Error(blk/P/SP),data=dassp)  
# print(ScottKnott::SK.nest.aovlist(modsspe3,which='SSP:SP',error='Within',fl2=1))
# 
# print(SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl1=1))#errooo
#  
SP2/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,which='SSP:SP',error='Within',fl2=2))
# 
# print(SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl1=2))#errooo
#  
SP3/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,which='SSP:SP',error='Within',fl2=3))
# 
# print(SK.nest.aovlist(modsspe,which='SP:SSP',error='Within',fl1=3))#errooo
#  
Avaliando as interações triplas
P1/SP1/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=1,fl3=1))#antes
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=1,fl2=1)
# 
P2/SP1/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=1,fl3=2))#antes
#  
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=2,fl2=1)
# 
# 
P3/SP1/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=1,fl3=3))#antes
# 
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=3,fl2=1) 
# 
P1/SP2/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=2,fl3=1))#antes
# 
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=1,fl2=2)
# 
P1/SP3/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=3,fl3=1))#antes
#  
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=1,fl2=3)
# 
# 
P2/SP2/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=2,fl3=2))#antes
# 
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=2,fl2=2) 
# 
P2/SP3/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=3,fl3=2))#antes
# 
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=2,fl2=3) 
#  
#P3/SP2/SSP
# print(ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=2,fl3=3))#antes
# 
# SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=3,fl2=2) 
#  
#P3/SP3/SSP
# P3SP3SSP <- (ScottKnott::SK.nest.aovlist(modsspe3,
# 			      which='SSP:SP:P',error='Within',fl2=3,fl3=3))#antes
# 
# P3SP3SSPt <- SK.nest.aovlist(modsspe, which='P:SP:SSP',error='Within',fl1=3,fl2=3) 
#  
# summary(P3SP3SSP)
# summary.SK.nest(P3SP3SSPt)
#                                              
# plot(P3SP3SSPt)
# 
source('./../R/SK.aov.R')
source('./../R/MaxValue.R') 
source('./../R/summary.SK.R') 
summary(SK.aov(modfe,which='P'))




