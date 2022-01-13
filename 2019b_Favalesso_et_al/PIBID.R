library(plyr)
library(car)
library(dunn.test)
library(graphics)
library(ggplot2)

# Eu usei o banco de dados do PIBID:

pibid=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

pibidnf=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

names(pibid) 

names(pibidnf) 

# "Turma"     "Aula"      "n"         "Avaliador" "Notas"    

attach(pibid)

attach(pibidnf)

detach(pibid)

# EST. DESCRITIVA:

# Avaliadores:

summary(pibid$Notas[pibid$Avaliador=="A1"])
sd(pibid$Notas[pibid$Avaliador=="A1"])  
median(pibid$Notas[pibid$Avaliador=="A1"]) 

summary(pibid$Notas[pibid$Avaliador=="A2"])
sd(pibid$Notas[pibid$Avaliador=="A2"])  
median(pibid$Notas[pibid$Avaliador=="A2"]) 

summary(pibid$Notas[pibid$Avaliador=="A3"])
sd(pibid$Notas[pibid$Avaliador=="A3"]) 
median(pibid$Notas[pibid$Avaliador=="A3"]) 


# Para as turmas:  

summary(pibidnf$Notas[pibidnf$Turma=="E1"]) 
sd(pibidnf$Notas[pibidnf$Turma=="E1"]) 
median(pibidnf$Notas[pibidnf$Turma=="E1"]) 

summary(pibidnf$Notas[pibidnf$Turma=="E2"]) 
sd(pibidnf$Notas[pibidnf$Turma=="E2"])
median(pibidnf$Notas[pibidnf$Turma=="E2"]) 


summary(pibidnf$Notas[pibidnf$Turma=="C"]) 
sd(pibidnf$Notas[pibidnf$Turma=="C"])
median(pibidnf$Notas[pibidnf$Turma=="C"]) 

# Aulas:

summary(pibid$Notas[pibid$Aula=="A1"]) 
sd(pibid$Notas[pibid$Aula=="A1"])

summary(pibid$Notas[pibid$Aula=="A2"]) 
sd(pibid$Notas[pibid$Aula=="A2"])

summary(pibid$Notas[pibid$Aula=="A3"]) 
sd(pibid$Notas[pibid$Aula=="A3"])

summary(pibid$Notas[pibid$Aula=="A4"]) 
sd(pibid$Notas[pibid$Aula=="A4"])

# Construir gr?ficos do tipo Box-plot em fun?ao das vari?veis qualitativas

# Nota final das turmas avaliadas (1 = 1? ano, 2? = controle e 3? = 3? ano):
qplot(pibidnf$Turma, pibidnf$Notas,geom="boxplot",xlab="Turma",ylab="Notas")

# Avaliadores x Nota final
qplot(pibid$Avaliador, pibid$Notas,geom="boxplot",xlab="Avaliador",ylab="Notas")

# Gr?fico de linhas!

# AS NOTAS MELHORARAM AO LONGO DAS AULAS?

tapply(pibidnf$Notas[pibidnf$Turma=="C"], pibidnf$Aula[pibidnf$Turma=="C"], median)
TurmaC<-c(3.350, 5.415, 6.370, 2.465)

tapply(pibidnf$Notas[pibidnf$Turma=="E2"], pibidnf$Aula[pibidnf$Turma=="E2"], median)
TurmaE2<-c(6.480, 7.100, 6.700, 3.585)

tapply(pibidnf$Notas[pibidnf$Turma=="E1"], pibidnf$Aula[pibidnf$Turma=="E1"], median)
TurmaE1<-c(6.270, 7.265, 8.170, 3.330)

Aulas<-c("Aula 1", "Aula 2", "Aula 3", "Aula 4")

Turmas<-c("C", "C", "C", "C", "E2", "E2", "E2", "E2", "E1", "E1", "E1", "E1")

Mediana<-c("C"=3.350, 5.415, 6.370, 2.465,"E2"=6.480, 7.100, 6.700, 3.585,"E1"=6.270, 7.265, 8.170, 3.330)

Mediana<-data.frame(Aulas, Turmas, Mediana)

Mediana

# Aulas x Mediana (Turmas)
ggplot(Mediana, aes(x=Aulas, y=Mediana, group=Turmas, shape=Turmas, colour=Turmas)) +
geom_line() +   geom_point(size=3) + ylim (0,10) + 
scale_color_manual(values=c("#999999", "#333333", "#666666")) +
xlab("Aulas") + ylab("Nota mediana")


#TESTES DE HIPOTESE:

# As notas das turmas pode ser considerada como estatisticamente diferentes?

# 1?) O N entre as variaveis ? igual ou diferente?

# 2?) As variaveis possuem uma distribui??o normal?

?shapiro.test

# As notas das turmas possuem uma distribui??o normal?
shapiro.test(pibidnf$Notas[pibidnf$Turma=="C"])  #W = 0.9635, p-value = 0.04642
shapiro.test(pibidnf$Notas[pibidnf$Turma=="E1"]) #W = 0.8982, p-value = 0.0006319
shapiro.test(pibidnf$Notas[pibidnf$Turma=="E2"]) #W = 0.9632, p-value = 0.01415
# P < 0,05: Logo os dados n?o provem de uma distribui??o normal. 

# As notas dos avaliadores possuem uma distribui??o normal?
shapiro.test(Notas[Avaliador=="A1"]) # W = 0.9511, p-value = 2.277e-06
shapiro.test(Notas[Avaliador=="A2"]) # W = 0.9666, p-value = 0.0001062
shapiro.test(Notas[Avaliador=="A3"]) # W = 0.9575, p-value = 1.016e-05
# P < 0,05: Logo os dados n?o provem de uma distribui??o normal. 

# 3?) Teste de hipotese de Kruskal-wallis - compara as popula??es quanto a 
#     tend?ncia central dos dados - foi aplicado a fim de testar a H0: as notas
#     n?o variam entre as turmas; e Ho: as notas n?o variaram entre os avaliadores.

# Para as turmas:
kruskal.test(Notas~Turma, data=pibidnf) # chi-squared = 19.8971, df = 2, p-value = 4.78e-05
dunn.test(pibidnf$Notas, pibidnf$Turma, kw=FALSE) # Dunn teste: Compara??o dos testes dois a dois.

#             C         E1
# ---------+----------------------
#   E1      4.318079
#           0.0000
# 
#   E2      3.063671    -1.787689
#    |      0.0011       0.0369


# Para os avaliadores:
kruskal.test(Notas~Avaliador, data=pibid) # chi-squared = 1.2261, df = 2, p-value = 0.5417
dunn.test(Notas$pibid, Avaliador$pibid, kw=FALSE)

# Row Mean |         A1         A2
# ---------+----------------------
#    A2        -0.324756
#               0.3727
#
#    A3         0.754415    1.079172
#               0.2253      0.1403



#############################################################################
# PRE-PROJETO
#############################################################################

questionario=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

Q1=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

Q2=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

Q3=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

Q4=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)

names(questionario) # "Ensino"     "Quest?o"    "N?.quest?o" "Respostas"  "FR." 

names(Q1)

attach(questionario)

install.packages("gridExtra")
library(gridExtra)

attach(freqquest)

QS1=ggplot(Q1, aes(x=Ensino, y=FR, fill=Respostas)) +  
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Questão 1") + labs(title="a)") +
  theme(plot.title = element_text(hjust = 0))
QS1

QS2=ggplot(Q2, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 2") +
  labs(title="b)") +   theme(plot.title = element_text(hjust = 0))
QS2

QS3=ggplot(Q3, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 3") +
  labs(title="c)") + theme(plot.title = element_text(hjust = 0))
QS3

QS4=ggplot(Q4, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 4") +
  labs(title="d)") + theme(plot.title = element_text(hjust = 0))
QS4

grid.arrange(QS1, QS2, 
             QS3, QS4)

## LEGENDA EM BAIXO 

QS1=ggplot(Q1, aes(x=Ensino, y=FR., fill=Respostas)) +  
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 1") + labs(title="a)") +
  theme(plot.title = element_text(hjust = 0)) + theme(legend.position="bottom")
QS1

QS2=ggplot(Q2, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 2") +
  labs(title="b)") +   theme(plot.title = element_text(hjust = 0)) + 
  theme(legend.position="bottom")
QS2

QS3=ggplot(Q3, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 3") +
  labs(title="c)") + theme(plot.title = element_text(hjust = 0)) +
  theme(legend.position="bottom")
QS3

QS4=ggplot(Q4, aes(x=Ensino, y=FR., fill=Respostas)) +
  geom_bar(stat="identity", colour="black") + scale_fill_brewer(palette = "Greys") +
  xlab("Tipo de Ensino") + ylab("FR%") + labs(fill="Quest?o 4") +
  labs(title="d)") + theme(plot.title = element_text(hjust = 0)) +
  theme(legend.position="bottom")
QS4

grid.arrange(QS1, QS2, 
            QS3, QS4)

##------------------------------------------------------------------------------##

library(grid)
library(scales)

pushViewport(viewport(layout = grid.layout(1, 2)))
print(QS1 + labs(title="a") + theme(plot.title) vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(QS2, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))

pushViewport(viewport(layout = grid.layout(1, 2)))
print(QS1 + labs(title = "a") + theme(plot.title = element_text(hjust = 0)), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(QS2 + labs(title = "b") + theme(plot.title = element_text(hjust = 0)), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# C:

summary(pibid$Notas[1:45]) # aula 1
summary(pibid$Notas[46:93]) # aula 2
summary(pibid$Notas[94:147]) # aula 3
summary(pibid$Notas[148:201]) # aula 4


tapply(pibidnf$Notas[pibidnf$Turma=="C"], pibidnf$Aula[pibidnf$Turma=="C"], summary)

tapply(pibidnf$Notas[pibidnf$Turma=="E2"], pibidnf$Aula[pibidnf$Turma=="E2"], summary)

tapply(pibidnf$Notas[pibidnf$Turma=="E1"], pibidnf$Aula[pibidnf$Turma=="E1"], summary)


## --- ###


## Qui-quadrado

# nrow = linha
# byrow = coluna

Q1=matrix(c(0, 1, 2, 3, 18, 15, 10, 11, 7, 15, 1, 3), nrow=6, byrow=2)
colnames(Q1)=c("Convencional", "Técnico")
rownames(Q1)=c("0- Não respondeu", "1- Não há na escola", "2- Nunca é usado",
               "3- Raramente é usado", "4- De vez em quando", "5- Sempre é usado")
Q1

xq1=chisq.test(Q1) # x² = 11,601; P<0,05
                   # existe associaçaõ entre linhas e colubas
xq1
xq1$residuals

barplot(prop.table(Q1)*100, legend.text = rownames(Q1))

cq1=c(0,	2,	18,	10,	7, 1)
xe1=chisq.test(cq1)
xe1
xe1$residuals # FR% > em 5 e 6

tq1=c(1, 3,	15,	11,	15,3)
xe1t=chisq.test(tq1)
xe1t
xe1t$residuals # FR% > em 4

Q2=matrix(c(5, 4, 3, 42, 26, 6), nrow=3, byrow=2)
colnames(Q2)=c("Convencional", "Técnico")
rownames(Q2)=c("0- Não respondeu", "3 - Regular", "5 - Ótimo")
Q2

xq2=chisq.test(Q2, correct = T) # x² = 44,246; P<0,001
xq2
# existe associaçaõ entre linhas e colubas
xq2$residuals

barplot(prop.table(Q2)*100, legend.text = rownames(Q2))

xq2c=c(5,4, 3, 26)
xe2c=chisq.test(xq2c)
xe2c
xe2c$residuals

xq2t=c(0, 0, 42, 6)
xe2t=chisq.test(xq2t)
xe2t
xe2t$residuals

Q3=matrix(c(32,44,6,4), nrow=2, byrow=2)
colnames(Q3)=c("Convencional", "Técnico")
rownames(Q3)=c("Não", "Sim")
Q3

xq3=chisq.test(Q3, correct = T) # x² = 0,53657; P = 0,464
xq3
# existe associaçaõ entre linhas e colubas
xq3$residuals

barplot(prop.table(Q3)*100, legend.text = rownames(Q3))

a=c(32,6)
chisq.test(a)
b=c(44,4)
chisq.test(b)

Q4=matrix(c(4, 9, 1, 1, 8, 4, 14, 13, 8, 19, 3, 2), nrow=6, byrow=2)
colnames(Q4)=c("Convencional", "Técnico")
rownames(Q4)=c("0- Não respondeu", "1- Não há na escola", "2- Nunca é usado",
               "3- Raramente é usado", "4- De vez em quando", "5- Sempre é usado")

Q4

xq4=chisq.test(Q4) # x² = 28,099; P<0,05
xq4
# existe associaçaõ entre linhas e colubas
xq4$residuals 

barplot(prop.table(Q4)*100, legend.text = rownames(Q4))

c=c(4, 1, 8, 14, 8, 3)
xc=chisq.test(c)
xc
xc$residuals

d=c(9, 1, 4, 13, 19, 2)
xd=chisq.test(d)
xd
xd$residuals
