#### Analysis of Marilia article

## Perfil

sex=c("F"=2575,"M"=4058)
s=chisq.test(sex)
s$stdres

age=c("A"=1273, "B"=1029, "C"=796, "D"=835,"E"=908, "F"=1795)
(a=chisq.test(age))
a$stdres

ethnicity=c("Black"=235, "Brown"=1245,"Indigenous"=39,"White"=4508,"Yellow"=47)
(e=chisq.test(ethnicity))
e$stdres

educ=c("A"=79,"B"=2430,"C"=386,"D"=359,"E"=607,"F"=70,"G"=178)
(ed=chisq.test(educ))
ed$stdres

### Characteristics of lonomic accidents

zone=c("U"=3102,"R"=3229,"IU"=102)
(z=chisq.test(zone))
z$stdres

time=c("A"=2907,"B"=1710,"C"=544,"D"=219,"E"=266,"F"=579)
(t=chisq.test(time))
t$stdres

work=c("y"=1262,"n"=4860)
(w=chisq.test(work))
w$stdres

body=c("A"=210, "B"=843, "C"=639,"D"=2542,"E"=812,"F"=336,"G"=155,"H"=315,"I"=577,"J"=68)
(b=chisq.test(body))
b$stdres


### Clinical aspects 

sev=c("A"=4841,"B"=1364,"C"=236)
(s=chisq.test(sev))
s$stdres

case=c("cure"=6178,"death"=12)
(c=chisq.test(case))
c$stdres

use=c("y"=1484, "n"=4887, "I"=265)
(u=chisq.test(use))
u$stdres


### Month distrbution

dist=c("J"=1074, "F"=1209, "M"=1195, "A"=838, "Ma"=480, "J"=273, "Jul"=165, "Aug"=92, "S"=109,"O"=132,"N"=331,"D"=738)
(d=chisq.test(dist))
d$stdres


### Year distribution

ye=c("2007"=373, "2008"=587, "2009"=580, "2010"=448, "2011"=510, "2012"=437, "2013"=411, "2014"=446, "2015"=453,"2016"=545,"2017"=809,"2018"=1036)
(y=chisq.test(ye))
y$stdres


###

citys<-c(17, 79, 58, 20, 113, 69, 52, 91, 76, 46, 1152, 155, 25, 33, 21, 49, 17, 632, 46, 41, 77, 16, 1025, 1612, 10, 970, 133)
chisq.c

#############################

dados<-data.frame(apenas_lonomia[,1:5])
colnames(dados)<-c("estado", "ano", "n", "pop", "taxa")
names(dados)

library(readr)
library(lme4)
library(ggplot2)
library(bbmle)
library(DHARMa)
library(AICcmodavg)
library(RVAideMemoire)
library(MASS)
library(INLA)
library(sjPlot)
library(visreg)

## Gráficos descritivos
ggplot(dados, aes(x=estado, y=n, fill=estado)) + geom_boxplot() + xlab("State") + ylab("Number of accidentes") + theme(legend.position="none")
ggplot(dados, aes(x=estado, y=taxa, fill=estado)) + geom_boxplot() + xlab("State") + ylab("Number of accidentes") + theme(legend.position="none")

ggplot(dados, aes(x=pop, y=n, colour=estado)) +   geom_point()
ggplot(dados, aes(x=pop, y=taxa, colour=estado)) +   geom_point()


## Modelo 1
model.1 <- glm(n ~ estado + as.factor(ano) + offset(log(pop)), family = poisson(link = "log"), data =dados)
model.1
summary(model.1)

plot(model.1)
