#########################################################
#### Analisis del Presupuesto Bancos Públicos de CR #####
####       Código por Andrés Beita-Jiménez          #####
####       Última modificación: May 4, 2020         #####
#########################################################

### revisar si las librerías están instaladas e instalar faltantes ###
list.of.packages <- c("ggplot2", "tidyr", "dplyr", "corrplot", "MuMIn", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Cargar librerías ### 
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(MuMIn)
library(ggpubr)

### Importar datos ###
BCR<-read.csv("data/VariablesBCR.csv")
Variables<-read.csv("data/VariablesMacroJK.csv")

### Join data with no lag ###
#create a year to join with no lag
BCR$year.join<-BCR$Year
Variables$year.join<-Variables$Year
#join data BCR 
BCR.lag0<-left_join(BCR,Variables, by="year.join")
str(Variables)
#join data BN 
BN.lag0<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BCR
BCR.cor<-cor(BCR.lag0[,-6])
corrplot(BCR.cor, method="number")
#BN
BN.cor<-cor(BN.lag0[,-6])
corrplot(BN.cor, method="number")
##models with lag 0
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.presup.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.presup.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.presup.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~1
#correr modelo
BCR.presup.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.presup.lag0)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.ingreso.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.ingreso.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.ingreso.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~1
#correr modelo
BCR.ingreso.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.ingreso.lag0)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.egreso.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.egreso.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.egreso.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~1
#correr modelo
BCR.egreso.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.egreso.lag0)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.superavit.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.superavit.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.superavit.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~PIB+Compra
#correr modelo
BCR.superavit.lag0<-lm(form1,data=BCR.lag0)
summary(BCR.superavit.lag0)

### Join data with lag of 1 year ###
#create a year to join with no lag
BCR$year.join<-BCR$Year
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+1
#join data BCR 
BCR.lag1<-left_join(BCR,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BCR
BCR.cor<-cor(BCR.lag1[,-6])
corrplot(BCR.cor, method="number")
##models with lag 1
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.presup.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.presup.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.presup.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~Compra
#correr modelo
BCR.presup.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.presup.lag1)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.ingreso.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.ingreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.ingreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Compra
#correr modelo
BCR.ingreso.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.ingreso.lag1)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.egreso.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.egreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.egreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~Compra
#correr modelo
BCR.egreso.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.egreso.lag1)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.superavit.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.superavit.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.superavit.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~PIB+Compra
#correr modelo
BCR.superavit.lag1<-lm(form1,data=BCR.lag1)
summary(BCR.superavit.lag1)

### Join data with lag of 2 year ###
#create a year to join with 2 year lag
BCR$year.join<-BCR$Year
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+2
#join data BCR 
BCR.lag2<-left_join(BCR,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BCR
BCR.cor<-cor(BCR.lag2[,-6])
corrplot(BCR.cor, method="number")
##models with lag 2
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.presup.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.presup.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.presup.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~Compra+Inflacion
#correr modelo
BCR.presup.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.presup.lag2)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.ingreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.ingreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.ingreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Compra+Inflacion+TBP
#correr modelo
BCR.ingreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.ingreso.lag2)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.egreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.egreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.egreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~Inflacion+TBP
#correr modelo
BCR.egreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.egreso.lag2)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.superavit.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.superavit.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.superavit.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BCR.superavit.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.superavit.lag2)

### Join data with lag of 3 year ###
#create a year to join with 3 year lag
BCR$year.join<-BCR$Year
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+3
#join data BCR 
BCR.lag3<-left_join(BCR,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BCR
BCR.cor<-cor(BCR.lag3[,-6])
corrplot(BCR.cor, method="number")
##models with lag 1
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.presup.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.presup.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.presup.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~TBP+Inflacion
#correr modelo
BCR.presup.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.presup.lag3)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.ingreso.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.ingreso.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.ingreso.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~1
#correr modelo
BCR.ingreso.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.ingreso.lag3)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.egreso.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.egreso.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.egreso.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~1
#correr modelo
BCR.egreso.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.egreso.lag3)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.superavit.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.superavit.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.superavit.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BCR.superavit.lag3<-lm(form1,data=BCR.lag3)
summary(BCR.superavit.lag3)

AIC(BCR.presup.lag0,BCR.presup.lag1,BCR.presup.lag2,BCR.presup.lag3)
AIC(BCR.ingreso.lag0,BCR.ingreso.lag1,BCR.ingreso.lag2,BCR.ingreso.lag3)
AIC(BCR.egreso.lag0,BCR.egreso.lag1,BCR.egreso.lag2,BCR.egreso.lag3)
AIC(BCR.superavit.lag0,BCR.superavit.lag1,BCR.superavit.lag2,BCR.superavit.lag3)


###########################################################################
###########################################################################
### Join data with lag of 2 year ###
#create a year to join with 2 year lag
BCR$year.join<-BCR$Year
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+2
#join data BCR 
BCR.lag2<-left_join(BCR,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BCR
BCR.cor<-cor(BCR.lag2[,-6])
corrplot(BCR.cor, method="number")
##models with lag 2
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.presup.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.presup.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.presup.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~Compra+Inflacion
#correr modelo
BCR.presup.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.presup.lag2)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.ingreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.ingreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.ingreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Compra+Inflacion+TBP
#correr modelo
BCR.ingreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.ingreso.lag2)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.egreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.egreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.egreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~Inflacion+TBP
#correr modelo
BCR.egreso.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.egreso.lag2)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BCR.superavit.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.superavit.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BCR.superavit.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BCR.superavit.lag2<-lm(form1,data=BCR.lag2)
summary(BCR.superavit.lag2)

###############################
#####      Gráficos      ######
###############################

##Presupuesto
BCR.presup.lag2
summary(BCR.presup.lag2)
#compra
range(BCR.lag0$Compra)
newdata <- expand.grid(Compra=seq(from=min(BCR.lag2$Compra), to=max(BCR.lag2$Compra), by=1),
                       Inflacion=mean(BCR.lag2$Inflacion))

newdata2 <- cbind(newdata, predict(BCR.presup.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion, fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion", "fit", "LL", "UL")

plot.compra <- ggplot(newdata3, aes(Compra, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Tipo de cambio") +
  labs(x = "Tipo de cambio (₡)", y = "Presupuesto (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.compra

#Inflacion
range(BCR.lag0$Inflacion)
newdata <- expand.grid(Inflacion=seq(from=min(BCR.lag2$Inflacion), to=max(BCR.lag2$Inflacion), by=0.1),
                       Compra=mean(BCR.lag2$Compra))

newdata2 <- cbind(newdata, predict(BCR.presup.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion, fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion", "fit", "LL", "UL")

plot.inflacion <- ggplot(newdata3, aes(Inflacion, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20, fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Inflación") +
  labs(x = "Inflación", y = "Presupuesto (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.inflacion
ggarrange(plot.compra, plot.inflacion,
          ncol = 1, nrow = 2)

ggsave("PresupuestoBCRblue.png", width = 16, height = 20, units = "cm")

##Ingreso
BCR.ingreso.lag2
summary(BCR.ingreso.lag2)
#compra
range(BCR.lag0$Compra)
newdata <- expand.grid(Compra=seq(from=min(BCR.lag2$Compra), to=max(BCR.lag2$Compra), by=1),
                       Inflacion=mean(BCR.lag2$Inflacion),
                       TBP=mean(BCR.lag2$TBP))

newdata2 <- cbind(newdata, predict(BCR.ingreso.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra,newdata2$Inflacion,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion","TBP", "fit", "LL", "UL")

plot.compra <- ggplot(newdata3, aes(Compra, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Tipo de cambio") +
  labs(x = "Tipo de cambio (₡)", y = "Ingreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.compra

#Inflacion
range(BCR.lag0$Inflacion)
newdata <- expand.grid(Inflacion=seq(from=min(BCR.lag2$Inflacion), to=max(BCR.lag2$Inflacion), by=0.1),
                       Compra=mean(BCR.lag2$Compra),
                       TBP=mean(BCR.lag2$TBP))

newdata2 <- cbind(newdata, predict(BCR.ingreso.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion","TBP","fit", "LL", "UL")

plot.inflacion <- ggplot(newdata3, aes(Inflacion, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20, fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Inflación") +
  labs(x = "Inflación", y = "Ingreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.inflacion

#TBP
range(BCR.lag0$TBP)
newdata <- expand.grid(TBP=seq(from=min(BCR.lag2$TBP), to=max(BCR.lag2$TBP), by=0.1),
                       Compra=mean(BCR.lag2$Compra),
                       Inflacion=mean(BCR.lag2$Inflacion))

newdata2 <- cbind(newdata, predict(BCR.ingreso.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion","TBP","fit", "LL", "UL")

plot.TBP <- ggplot(newdata3, aes(TBP, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20, fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Inflación") +
  labs(x = "TBP", y = "Ingreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.TBP

ggarrange(plot.compra, plot.inflacion,plot.TBP,
          ncol = 1, nrow = 3)

ggsave("IngresoBCRblue3.png", width = 16, height = 30, units = "cm")

##Egreso
BCR.egreso.lag2
summary(BCR.egreso.lag2)

#Inflacion
range(BCR.lag0$Inflacion)
newdata <- expand.grid(Inflacion=seq(from=min(BCR.lag2$Inflacion), to=max(BCR.lag2$Inflacion), by=0.1),
                       Compra=mean(BCR.lag2$Compra),
                       TBP=mean(BCR.lag2$TBP))

newdata2 <- cbind(newdata, predict(BCR.egreso.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion","TBP","fit", "LL", "UL")

plot.inflacion <- ggplot(newdata3, aes(Inflacion, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20, fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Inflación") +
  labs(x = "Inflación", y = "Egreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.inflacion

#TBP
range(BCR.lag0$TBP)
newdata <- expand.grid(TBP=seq(from=min(BCR.lag2$TBP), to=max(BCR.lag2$TBP), by=0.1),
                       Compra=mean(BCR.lag2$Compra),
                       Inflacion=mean(BCR.lag2$Inflacion))

newdata2 <- cbind(newdata, predict(BCR.egreso.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra, newdata2$Inflacion,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra", "Inflacion","TBP","fit", "LL", "UL")

plot.TBP <- ggplot(newdata3, aes(TBP, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20, fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Inflación") +
  labs(x = "TBP", y = "Egreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.TBP

ggarrange(plot.inflacion,plot.TBP,
          ncol = 1, nrow = 2)

ggsave("EgresoBCRblue.png", width = 16, height = 20, units = "cm")

##Superavit
BCR.superavit.lag0
summary(BCR.superavit.lag2)

#Gobierno
range(BCR.lag0$Inflacion)
newdata <- data.frame(Gobierno = levels(BCR.lag2$Gobierno),
                       Partido=NA)
newdata$Gobierno <- as.factor(newdata$Gobierno)
newdata$Gobierno <- factor(newdata$Gobierno,levels = c("Arias","Chinchilla","Solis","Alvarado"))
newdata$Partido <- c("PAC","PLN","PLN","PAC")
newdata2 <- cbind(newdata, predict(BCR.superavit.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Gobierno,newdata2$Partido,fit, LL, UL)
names(newdata3) <- c("Gobierno","Partido","fit", "LL", "UL")

plot.gobierno <- ggplot(newdata3, aes(Gobierno, fit, colour=Partido)) +
  geom_point(size = 4) +
  geom_errorbar(data=newdata3, mapping=aes(x=Gobierno, ymin=LL, ymax=UL, colour=Partido), width=0.1, size=1) +
  #ggtitle("Inflación") +
  labs(x = "Administración", y = "Superavit (miles de millones ₡)") +
  theme_bw()+
  scale_colour_manual(values = c("gold", "green4"))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")

plot.gobierno


ggsave("SuperavitBCR.png", width = 18, height = 12, units = "cm")

summary(BCR.presup.lag2)
summary(BCR.ingreso.lag2)
summary(BCR.egreso.lag2)
summary(BCR.superavit.lag2)

BCR.lag2.newdata <- BCR.lag2 %>%
  select(Presupuesto,year.join, Compra, Inflacion)


#compra
range(BCR.lag0$Compra)
newdata <- BCR.lag2.newdata

newdata2 <- cbind(newdata, predict(BCR.presup.lag2, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Presupuesto,newdata2$year.join, fit, LL, UL)
names(newdata3) <- c("Presupuesto","Year", "fit", "LL", "UL")

plot.pres <- ggplot(newdata3, aes(Year, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  geom_line(aes(Year, Presupuesto),size = 1,colour="black")+
  #ggtitle("Tipo de cambio") +
  labs(x = "Año", y = "Presupuesto (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.pres

ggsave("Pres.png", width = 18, height = 12, units = "cm")
