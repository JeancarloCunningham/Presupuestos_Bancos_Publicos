#########################################################
#### Analisis del Presupuesto Bancos Públicos de CR #####
####       Código por Andrés Beita-Jiménez          #####
####      Última modificación: June 29, 2020        #####
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

## Import data ###
BN<-read.csv("data/VariablesBN.csv")
Variables<-read.csv("data/VariablesMacroJK.csv")

###########################################################################
###########################################################################
### Join data with lag of 3 year ###
#create a year to join with 3 year lag
BN$year.join<-BN$Year
Variables$year.join<-Variables$Year+3
#join data BN 
BN.lag3<-left_join(BN,Variables, by="year.join")

### exploratory analysis ###
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

ggsave("1_PresupuestoBN.png", width = 16, height = 20, units = "cm")

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

ggsave("2_IngresoBN.png", width = 16, height = 20, units = "cm")

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

ggsave("3_EgresoBN.png", width = 16, height = 20, units = "cm")

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


ggsave("4_SuperavitBN.png", width = 18, height = 12, units = "cm")

summary(BN.presup.lag3)
summary(BN.ingreso.lag1)
summary(BN.egreso.lag1)
summary(BN.superavit.lag1)

### gráficos de predicho vrs real ###

#Presupuesto
#seleccionar variables de interes
BN.lag3.newdata <- BN.lag3 %>%
  select(Presupuesto,year.join, TBP, Inflacion)

newdata <- BN.lag3.newdata
#predecir
newdata2 <- cbind(newdata, predict(BN.presup.lag3, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Presupuesto,newdata2$year.join, fit, LL, UL)
names(newdata3) <- c("Presupuesto","Año", "Prediccion", "LI", "LS")

plot.pres <- ggplot(newdata3, aes(Año, Prediccion)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  geom_line(aes(Año, Presupuesto),size = 1,colour="black")+
  #ggtitle("Tipo de cambio") +
  labs(x = "Año", y = "Presupuesto (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(newdata3$Año), max(newdata3$Año), by = 2),1))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.pres
#guardar gráfico
ggsave(paste("5_PresupBN_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("PresupuestoBN",Sys.Date(),".csv",sep = ""))

#Ingreso
#seleccionar variables de interes
BN.lag1.newdata <- BN.lag1 %>%
  select(Ingreso,year.join, Compra, TBP)

newdata <- BN.lag1.newdata
#predecir
newdata2 <- cbind(newdata, predict(BN.ingreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Ingreso,newdata2$year.join, fit, LL, UL)
names(newdata3) <- c("Ingreso","Año", "Prediccion", "LI", "LS")

plot.ingr <- ggplot(newdata3, aes(Año, Prediccion)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  geom_line(aes(Año, Ingreso),size = 1,colour="black")+
  #ggtitle("Tipo de cambio") +
  labs(x = "Año", y = "Ingreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(newdata3$Año), max(newdata3$Año), by = 2),1))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.ingr
#guardar gráfico
ggsave(paste("6_IngresoBN_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("IngresoBN",Sys.Date(),".csv",sep = ""))

#Egreso
#seleccionar variables de interes
BN.lag1.newdata <- BN.lag1 %>%
  select(Egreso,year.join, Compra, TBP)

newdata <- BN.lag1.newdata
#predecir
newdata2 <- cbind(newdata, predict(BN.egreso.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Egreso,newdata2$year.join, fit, LL, UL)
names(newdata3) <- c("Egreso","Año", "Prediccion", "LI", "LS")

plot.egr <- ggplot(newdata3, aes(Año, Prediccion)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  geom_line(aes(Año, Egreso),size = 1,colour="black")+
  #ggtitle("Tipo de cambio") +
  labs(x = "Año", y = "Egreso (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(newdata3$Año), max(newdata3$Año), by = 2),1))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.egr
#guardar gráfico
ggsave(paste("7_EgresoBN_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("EgresoBN",Sys.Date(),".csv",sep = ""))

#Superavit
#seleccionar variables de interes
BN.lag1.newdata <- BN.lag1 %>%
  select(Superavit,year.join, Gobierno)

newdata <- BN.lag1.newdata
#predecir
newdata2 <- cbind(newdata, predict(BN.superavit.lag1, newdata, type = "response", 
                                   se.fit=TRUE,re.form=NA))
fit <- newdata2$fit
LL <- newdata2$fit- 1.96 * newdata2$se.fit
UL <- newdata2$fit+ 1.96 * newdata2$se.fit

newdata3 <- data.frame(newdata2$Superavit,newdata2$year.join, fit, LL, UL)
names(newdata3) <- c("Superavit","Año", "Prediccion", "LI", "LS")

plot.sup <- ggplot(newdata3, aes(Año, Prediccion)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .20,fill="blue") +
  geom_line(size = 1,colour="blue") +
  geom_line(aes(Año, Superavit),size = 1,colour="black")+
  #ggtitle("Tipo de cambio") +
  labs(x = "Año", y = "Superavit (miles de millones ₡)") +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(newdata3$Año), max(newdata3$Año), by = 2),1))+
  theme(text = element_text(size=16))+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot.sup
#guardar gráfico
ggsave(paste("8_SuperavitBN_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("SuperavitBN",Sys.Date(),".csv",sep = ""))

