
library(lmerTest)
library(lme4)
library(MASS)
library(glmmADMB)
library(blmeco)
library(snowfall)
library(R2admb)
library(ggplot2)


Data <- data.frame(apenas_lonomia)
str(Data)
colnames(Data)<-c("state", "year", "cases", "pop", "taxa")
Data$year <- as.factor(Data$year)
Data$state <- as.factor(Data$state)
names(Data)
Data
### Poisson Model ###
model <- glm(cases ~ state + year, offset= log(pop), family = poisson(link = "log"), data = Data)
summary(model)
# Check overdispersion
sum(resid(model,type="pearson")^2)/model$df.resid		# Overdispersion: it shouldn't be over 1.4
# 8.89: Highly overdispersed = Move to Quasi-Poisson or negative binomial

### Quasi-Poisson Model ###  used to estimate the dispersion parameter, i.e., degree of overdispersion
model.1 <- glm(cases ~ state + year + offset(log(pop)), family = quasipoisson(link = "log"), data = Data)
summary(model.1)
sum(resid(model.1,type="pearson")^2)/model.1$df.resid		# Overdispersion: it shouldn't be over 1.4

### Negative Binomial Model ###
model.2 <- glm.nb(cases ~ state + year + offset(log(pop)), data = Data)
summary(model.2)
sum(resid(model.2,type="pearson")^2)/model.2$df.resid		# Overdispersion: it shouldn't be over 1.4

#-------------------------------------------------------------------------------------------------------------------#

### Mixed Model to account non-independence ###
# Parallel processing
  sfInit(parallel=TRUE, cpus=parallel:::detectCores()-2)
# Load the required packages inside the cluster
  sfLibrary(lme4); sfLibrary(glmmADMB)

### Poisson Model ###
mix.model <- glmer(cases ~ state + year + offset(log(pop)) + (1|state), family = poisson(link = "log"), data = Data)
summary(mix.model)
dispersion_glmer(mix.model)		# Overdispersion: it shouldn't be over 1.4
ParametroDispersion(mix.model)

### Quasi-Poisson Model ###
mix.model.1 <- glmmadmb(cases ~ state + year + offset(log(pop)) + (1|state), data = Data, family="nbinom1")
summary(mix.model.1)
ParametroDispersion(mix.model.1)

### Negative Binomial Model ###

names(Data)

## model
Data$state <- factor(Data$state, levels=c("ES", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"))
mix.model.2 <- glmmadmb(cases ~ state + as.integer(year) + offset(log(pop)) + (1|state), data = Data, family="nbinom")
summary(mix.model.2) # results 
ParametroDispersion(mix.model.2)

(est <- cbind(Estimate = coef(mix.model.2), confint(mix.model.2))) # exp

aa<-data.frame(round(exp(est), 3)) # round exp
aa
names(aa)
dim(aa)
aa[c(-1,-28),]
barplot(aa[c(-1,-28),]$Estimate~rownames(aa[c(-1,-28),]))
names(Data)
boxplot(Data$rate~Data$state)

b.data<-Data
Data$state <- factor(Data$state, levels=c('RJ',	'PA',	'SE',	'PE',	'PB',	'PI',	'MA',	'CE',	'BA',	'MT',	'RN',	'GO',	'ES',	'AM',	'DF',
                                          'AC',	'SP',	'AL',	'AP',	'RR',	'RO',	'MG',	'MS',	'PR',	'TO',	'RS',	'SC'))
boxplot(Data$rate~Data$state)
names(Data)
ggplot(Data, aes(x=state, y=rate)) + geom_boxplot() + xlab("State") + ylab("Incidence rate per 100,000 hab.") + 
  scale_fill_manual(values=c("#CCFF99"))

###

devtools::install_github('abjur/abjData')

constroi_mapa_tematico <- function(dataset){
  dataset %>% 
    inner_join(abjData::br_uf_map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat,
                     map_id = id, fill = variavel),
                 color = 'gray30', map = ., data = .) + 
        theme_void() +
        coord_equal()
    }
}

library(rgdal)
library(raster)
Brasil <- shapefile("BR.shp")

plot(Brasil)
names(Brasil)
names(aa)
aa[c(-1,-28),]$ESTADOS<-c("RJ",	"PA",	"SE",	"PE",	"PB",	"PI",	"MA",	"CE",	"BA",	"MT",	"RN",	"GO",	 "ES","AM",	"DF",	"AC",	"SP",	"AL",	"AP",	"RR",	
              "RO",	"MG",	"MS",	"PR",	"TO",	"RS",	"SC")

a<- merge(Brasil, aa[c(-1,-28),], by="ESTADOS")



