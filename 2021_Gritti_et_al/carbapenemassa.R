
## Mica
## 04/08/2020

## Packages --
library(readxl)
library(tidyverse)
library(questionr)

## Data --
data = read_excel("C:/Users/MMF/Dropbox/Artigos/Mica/Carbapenemasas_18082020.xlsx", 
                   sheet = "est") %>% data.frame()

names(data)

## Descriptivo general --
table(data$servicio[data$servicio != '-']) %>% chisq.test()
table(data$servicio[data$servicio != '-']) %>% chisq.residuals()

table(data$muestra) %>% chisq.test()
table(data$muestra) %>% chisq.residuals()

table(data$carbapenemasa) %>% chisq.test()
table(data$carbapenemasa) %>% chisq.residuals()

c(90, 25) %>% chisq.test()

table(data$infc_colo) %>% chisq.test()

table(data$microorg) %>% chisq.test()
table(data$microorg) %>% chisq.residuals()

## Infectado --
table(data$infeccion[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.test()

table(data$infeccion[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.residuals()

table(data$muestra[data$infeccion == "POSITIVA"]) %>% 
  chisq.test() ## expected = 4.6

table(data$muestra[data$infeccion == "POSITIVA"]) %>% 
  chisq.residuals() ## expected = 4.6

table(data$carbapenemasa[data$infeccion == "POSITIVA"]) %>% 
  chisq.test() 

## Colonizacion -- 
## Infectado --
table(data$colonizacion[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.test()

table(data$colonizacion[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.residuals()

table(data$carbapenemasa[data$colonizacion == "POSITIVA"]) %>% 
  chisq.test() 

table(data$carbapenemasa[data$colonizacion == "POSITIVA"]) %>% 
  chisq.residuals() 

## infec. vs. colonizacion --
table(data$infc_colo[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.test() ## min expected = 1

table(data$infc_colo[data$servicio != '-'], 
      data$servicio[data$servicio != '-']) %>% 
  chisq.residuals() ## min expected = 1

table(data$infc_colo, 
      data$muestra) %>% 
  chisq.test() ## !! Valores menores do que 1

teste = table(data$infc_colo, 
      data$carbapenemasa) %>% 
  chisq.test() ## !! Valores menores do que 1

## Antibioticos --

table(data$meropenem[data$meropenem != '-'])
c(56, 0) %>% chisq.test()

# 
table(data$cim_meropenem[data$cim_meropenem != '-']) %>% 
  chisq.test()

#
c(4, 0) %>% chisq.test(simulate.p.value = T, B = 2000)

#
table(data$MINOCICLINA[data$MINOCICLINA != '-']) %>%
  chisq.test()

#
table(data$GENTAMICINA[data$GENTAMICINA != '-']) %>%
  chisq.test()

#
table(data$CIPROFLOXACINA[data$CIPROFLOXACINA != '-']) %>%
  chisq.test()

#
nit = table(data$NITROFURANTOINA[data$NITROFURANTOINA != '-']) %>%
  chisq.test()
nit
nit$expected

#
table(data$TRIMETOPRIMA_SULFAMETOXASOL[data$TRIMETOPRIMA_SULFAMETOXASOL != '-']) %>%
  chisq.test()

#
table(data$COLISTIN[data$COLISTIN != '-']) %>%
  chisq.test()

#
table(data$TIGECICLINA[data$TIGECICLINA != '-']) %>%
  chisq.test()

## KPC --

c(32, 0) %>% chisq.test()

table(data$cim_meropenem[data$cim_meropenem != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

c(0, 4) %>% chisq.test(simulate.p.value = T, B = 2000)

table(data$AMIKACINA[data$AMIKACINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$MINOCICLINA[data$MINOCICLINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$GENTAMICINA[data$GENTAMICINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$CIPROFLOXACINA[data$CIPROFLOXACINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$NITROFURANTOINA[data$NITROFURANTOINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$TRIMETOPRIMA_SULFAMETOXASOL[data$TRIMETOPRIMA_SULFAMETOXASOL != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$COLISTIN[data$COLISTIN != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$TIGECICLINA[data$TIGECICLINA != '-' & data$carbapenemasa == 'KPC']) %>% 
  chisq.test()

table(data$microorg[data$microorg != '-' & data$carbapenemasa == 'KPC']) %>%
  chisq.test()

table(data$microorg[data$microorg != '-' & data$carbapenemasa == 'KPC']) %>%
  chisq.residuals()

chisq.test(c(29, 2, 1))
chisq.test(c(29, 2, 1))$residuals

chisq.test(c(30, 2))


## OXA-ACI --
c(24, 0) %>% chisq.test()

c(24, 0) %>% chisq.test()


table(data$AMIKACINA[data$AMIKACINA != '-' & data$carbapenemasa == 'OXA-ACI']) %>% 
  chisq.test()

table(data$MINOCICLINA[data$MINOCICLINA != '-' & data$carbapenemasa == 'OXA-ACI']) %>% 
  chisq.test()

c(20, 0) %>% chisq.test()

c(24, 0) %>% chisq.test()

c(24, 0) %>% chisq.test()


table(data$COLISTIN[data$COLISTIN != '-' & data$carbapenemasa == 'OXA-ACI']) %>% 
  chisq.test()

table(data$TIGECICLINA[data$TIGECICLINA != '-' & data$carbapenemasa == 'OXA-ACI']) %>% 
  chisq.test()


###
table(data$microorg[data$microorg != '-'])

((table(data$microorg[data$microorg != '-']))/sum(table(data$microorg[data$microorg != '-'])))*100


table(data$microorg[data$microorg != '-']) %>%
  chisq.test()

table(data$microorg[data$microorg != '-']) %>%
  chisq.residuals()

##

c(9, 15) %>% chisq.test()
c(32, 0) %>% chisq.test()
