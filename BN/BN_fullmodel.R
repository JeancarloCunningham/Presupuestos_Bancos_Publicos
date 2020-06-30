#########################################################
#### Analisis del Presupuesto Bancos Públicos de CR #####
####       Código por Andrés Beita-Jiménez          #####
####       Última modificación: May 17, 2020        #####
#########################################################

### Load packages ### 
library(tidyr)
library(dplyr)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
library(MuMIn)
library(ggpubr)

### Import data ###
BN<-read.csv("data/VariablesBN.csv")
Variables<-read.csv("data/VariablesMacroJK.csv")

### Join data with no lag ###
#create a year to join with no lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year

#join data BN 
BN.lag0<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag0[,-6])
corrplot(BN.cor, method="number")
##models with lag 0
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.presup.lag0<-lm(form1,data=BN.lag0)
summary(BN.presup.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BN.presup.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~1
#correr modelo
BN.presup.lag0<-lm(form1,data=BN.lag0)
summary(BN.presup.lag0)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.ingreso.lag0<-lm(form1,data=BN.lag0)
summary(BN.ingreso.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BN.ingreso.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~PIB
#correr modelo
BN.ingreso.lag0<-lm(form1,data=BN.lag0)
summary(BN.ingreso.lag0)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.egreso.lag0<-lm(form1,data=BN.lag0)
summary(BN.egreso.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BN.egreso.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~PIB
#correr modelo
BN.egreso.lag0<-lm(form1,data=BN.lag0)
summary(BN.egreso.lag0)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.superavit.lag0<-lm(form1,data=BN.lag0)
summary(BN.superavit.lag0)
options(na.action=na.fail)
#variables a incluir
dredge(BN.superavit.lag0)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BN.superavit.lag0<-lm(form1,data=BN.lag0)
summary(BN.superavit.lag0)

### Join data with lag of 1 year ###
#create a year to join with no lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+1
#join data BN 
BN.lag1<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag1[,-6])
corrplot(BN.cor, method="number")
##models with lag 1
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.presup.lag1<-lm(form1,data=BN.lag1)
summary(BN.presup.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.presup.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~Compra
#correr modelo
BN.presup.lag1<-lm(form1,data=BN.lag1)
summary(BN.presup.lag1)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.ingreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.ingreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.ingreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Compra+TBP
#correr modelo
BN.ingreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.ingreso.lag1)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.egreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.egreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.egreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~Compra+TBP
#correr modelo
BN.egreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.egreso.lag1)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.superavit.lag1<-lm(form1,data=BN.lag1)
summary(BN.superavit.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.superavit.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BN.superavit.lag1<-lm(form1,data=BN.lag1)
summary(BN.superavit.lag1)

### Join data with lag of 2 year ###
#create a year to join with 2 year lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+2
#join data BN 
BN.lag2<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag2[,-6])
corrplot(BN.cor, method="number")
##models with lag 2
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.presup.lag2<-lm(form1,data=BN.lag2)
summary(BN.presup.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BN.presup.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~Compra
#correr modelo
BN.presup.lag2<-lm(form1,data=BN.lag2)
summary(BN.presup.lag2)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.ingreso.lag2<-lm(form1,data=BN.lag2)
summary(BN.ingreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BN.ingreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Inflacion+PIB
#correr modelo
BN.ingreso.lag2<-lm(form1,data=BN.lag2)
summary(BN.ingreso.lag2)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.egreso.lag2<-lm(form1,data=BN.lag2)
summary(BN.egreso.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BN.egreso.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~PIB
#correr modelo
BN.egreso.lag2<-lm(form1,data=BN.lag2)
summary(BN.egreso.lag2)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.superavit.lag2<-lm(form1,data=BN.lag2)
summary(BN.superavit.lag2)
options(na.action=na.fail)
#variables a incluir
dredge(BN.superavit.lag2)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BN.superavit.lag2<-lm(form1,data=BN.lag2)
summary(BN.superavit.lag2)

### Join data with lag of 3 year ###
#create a year to join with 3 year lag
BN$year.join<-BN$Year
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+3
#join data BN 
BN.lag3<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag3[,-6])
corrplot(BN.cor, method="number")
##models with lag 1
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.presup.lag3<-lm(form1,data=BN.lag3)
summary(BN.presup.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BN.presup.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~TBP+Inflacion
#correr modelo
BN.presup.lag3<-lm(form1,data=BN.lag3)
summary(BN.presup.lag3)

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.ingreso.lag3<-lm(form1,data=BN.lag3)
summary(BN.ingreso.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BN.ingreso.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~TBP
#correr modelo
BN.ingreso.lag3<-lm(form1,data=BN.lag3)
summary(BN.ingreso.lag3)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.egreso.lag3<-lm(form1,data=BN.lag3)
summary(BN.egreso.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BN.egreso.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~TBP
#correr modelo
BN.egreso.lag3<-lm(form1,data=BN.lag3)
summary(BN.egreso.lag3)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.superavit.lag3<-lm(form1,data=BN.lag3)
summary(BN.superavit.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BN.superavit.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BN.superavit.lag3<-lm(form1,data=BN.lag3)
summary(BN.superavit.lag3)

AIC(BN.presup.lag0,BN.presup.lag1,BN.presup.lag2,BN.presup.lag3)
AIC(BN.ingreso.lag0,BN.ingreso.lag1,BN.ingreso.lag2,BN.ingreso.lag3)
AIC(BN.egreso.lag0,BN.egreso.lag1,BN.egreso.lag2,BN.egreso.lag3)
AIC(BN.superavit.lag0,BN.superavit.lag1,BN.superavit.lag2,BN.superavit.lag3)


###########################################################################
###########################################################################
### Join data with lag of 3 year ###
#create a year to join with 3 year lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+3
#join data BN 
BN.lag3<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag3[,-6])
corrplot(BN.cor, method="number")
##models with lag 1
#Presupuesto
#escribir formula
form1<-Presupuesto~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.presup.lag3<-lm(form1,data=BN.lag3)
summary(BN.presup.lag3)
options(na.action=na.fail)
#variables a incluir
dredge(BN.presup.lag3)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Presupuesto~TBP+Inflacion
#correr modelo
BN.presup.lag3<-lm(form1,data=BN.lag3)
summary(BN.presup.lag3)


### Join data with lag of 1 year ###
#create a year to join with no lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+1
#join data BN 
BN.lag1<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
#correlation plot para ver si hay variables relacionadas con el presupuesto
#BN
BN.cor<-cor(BN.lag1[,-6])
corrplot(BN.cor, method="number")
##models with lag 1

#Ingreso
#escribir formula
form1<-Ingreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.ingreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.ingreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.ingreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Ingreso~Compra+TBP
#correr modelo
BN.ingreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.ingreso.lag1)

#Egreso
#escribir formula
form1<-Egreso~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.egreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.egreso.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.egreso.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Egreso~Compra+TBP
#correr modelo
BN.egreso.lag1<-lm(form1,data=BN.lag1)
summary(BN.egreso.lag1)

#Superavit
#escribir formula
form1<-Superavit~Inflacion+PIB+TBP+Compra+Gobierno
#correr modelo
BN.superavit.lag1<-lm(form1,data=BN.lag1)
summary(BN.superavit.lag1)
options(na.action=na.fail)
#variables a incluir
dredge(BN.superavit.lag1)
#basado en la seleccion de variables
#escribir formula con variables seleccionadas
form1<-Superavit~Gobierno
#correr modelo
BN.superavit.lag1<-lm(form1,data=BN.lag1)
summary(BN.superavit.lag1)


###############################
#####      Gráficos      ######
###############################

##Presupuesto
BN.presup.lag3
summary(BN.presup.lag3)
#TBP
range(BN.lag0$TBP)
newdata <- expand.grid(TBP=seq(from=min(BN.lag3$TBP), to=max(BN.lag3$TBP), by=0.25),
                       Inflacion=mean(BN.lag3$Inflacion))

newdata2 <- cbind(newdata, predict(BN.presup.lag3, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$TBP, newdata2$Inflacion, fit, LL, UL)
names(newdata3) <- c("TBP", "Inflacion", "fit", "LL", "UL")

plot.TBP <- ggplot(newdata3, aes(TBP, fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  #ggtitle("Tipo de cambio") +
  labs(x = "TBP", y = "Presupuesto (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.TBP

#Inflacion
range(BN.lag0$Inflacion)
newdata <- expand.grid(Inflacion=seq(from=min(BN.lag3$Inflacion), to=max(BN.lag3$Inflacion), by=0.1),
                       TBP=mean(BN.lag3$TBP))

newdata2 <- cbind(newdata, predict(BN.presup.lag3, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$TBP, newdata2$Inflacion, fit, LL, UL)
names(newdata3) <- c("TBP", "Inflacion", "fit", "LL", "UL")

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
ggarrange(plot.TBP, plot.inflacion,
          ncol = 1, nrow = 2)

ggsave("PresupuestoBNblue.png", width = 16, height = 20, units = "cm")

##Ingreso
BN.ingreso.lag1
summary(BN.ingreso.lag1)
#compra
range(BN.lag0$Compra)
newdata <- expand.grid(Compra=seq(from=min(BN.lag1$Compra), to=max(BN.lag1$Compra), by=1),
                       Inflacion=mean(BN.lag1$Inflacion),
                       TBP=mean(BN.lag1$TBP))

newdata2 <- cbind(newdata, predict(BN.ingreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra","TBP", "fit", "LL", "UL")

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

#TBP
range(BN.lag0$TBP)
newdata <- expand.grid(TBP=seq(from=min(BN.lag1$TBP), to=max(BN.lag1$TBP), by=0.1),
                       Compra=mean(BN.lag1$Compra),
                       Inflacion=mean(BN.lag1$Inflacion))

newdata2 <- cbind(newdata, predict(BN.ingreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra","TBP","fit", "LL", "UL")

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

ggarrange(plot.compra,plot.TBP,
          ncol = 1, nrow = 2)

ggsave("IngresoBNblue3.png", width = 16, height = 20, units = "cm")

##Egreso
BN.egreso.lag1
summary(BN.egreso.lag1)

#compra
range(BN.lag0$Compra)
newdata <- expand.grid(Compra=seq(from=min(BN.lag1$Compra), to=max(BN.lag1$Compra), by=1),
                       Inflacion=mean(BN.lag1$Inflacion),
                       TBP=mean(BN.lag1$TBP))

newdata2 <- cbind(newdata, predict(BN.egreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra","TBP", "fit", "LL", "UL")

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


#TBP
range(BN.lag0$TBP)
newdata <- expand.grid(TBP=seq(from=min(BN.lag1$TBP), to=max(BN.lag1$TBP), by=0.1),
                       Compra=mean(BN.lag1$Compra),
                       Inflacion=mean(BN.lag1$Inflacion))

newdata2 <- cbind(newdata, predict(BN.egreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Compra,newdata2$TBP,fit, LL, UL)
names(newdata3) <- c("Compra","TBP","fit", "LL", "UL")

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

ggarrange(plot.compra,plot.TBP,
          ncol = 1, nrow = 2)

ggsave("EgresoBNblue.png", width = 16, height = 20, units = "cm")

##Superavit
BN.superavit.lag0
summary(BN.superavit.lag1)

#Gobierno
range(BN.lag0$Inflacion)
newdata <- data.frame(Gobierno = levels(BN.lag2$Gobierno),
                      Partido=NA)
newdata$Gobierno <- as.factor(newdata$Gobierno)
newdata$Gobierno <- factor(newdata$Gobierno,levels = c("Arias","Chinchilla","Solis","Alvarado"))
newdata$Partido <- c("PAC","PLN","PLN","PAC")
newdata2 <- cbind(newdata, predict(BN.superavit.lag1, newdata, type = "response", 
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


ggsave("SuperavitBN.png", width = 18, height = 12, units = "cm")

summary(BN.presup.lag3)
summary(BN.ingreso.lag1)
summary(BN.egreso.lag1)
summary(BN.superavit.lag1)
