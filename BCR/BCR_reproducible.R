#########################################################
#### Analisis del Presupuesto Bancos Públicos de CR #####
####       Código por Andrés Beita-Jiménez          #####
####     Última modificación: June 29, 2020         #####
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

### Unir datos con un desfaze de 2 años ###
#primero crea un año diferente para unir con el desfaze
BCR$year.join<-BCR$Year
Variables$year.join<-Variables$Year+2
#unir datos del BCR 
BCR.lag2<-left_join(BCR,Variables, by="year.join")

##modelos BCR
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

ggsave("1_PresupuestoBCR.png", width = 16, height = 20, units = "cm")

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

ggsave("2_IngresoBCR.png", width = 16, height = 30, units = "cm")

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

ggsave("3_EgresoBCR.png", width = 16, height = 20, units = "cm")

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


ggsave("4_SuperavitBCR.png", width = 18, height = 12, units = "cm")

summary(BCR.presup.lag2)
summary(BCR.ingreso.lag2)
summary(BCR.egreso.lag2)
summary(BCR.superavit.lag2)

### gráficos de predicho vrs real ###

#Presupuesto
#seleccionar variables de interes
BCR.lag2.newdata <- BCR.lag2 %>%
  select(Presupuesto,year.join, Compra, Inflacion)

newdata <- BCR.lag2.newdata
#predecir
newdata2 <- cbind(newdata, predict(BCR.presup.lag2, newdata, type = "response", 
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
ggsave(paste("5_PresupBCR_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("PresupuestoBCR",Sys.Date(),".csv",sep = ""))

#Ingreso
#seleccionar variables de interes
BCR.lag2.newdata <- BCR.lag2 %>%
  select(Ingreso,year.join, Compra, Inflacion, TBP)

newdata <- BCR.lag2.newdata
#predecir
newdata2 <- cbind(newdata, predict(BCR.ingreso.lag2, newdata, type = "response", 
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
ggsave(paste("6_IngresoBCR_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("IngresoBCR",Sys.Date(),".csv",sep = ""))

#Egreso
#seleccionar variables de interes
BCR.lag2.newdata <- BCR.lag2 %>%
  select(Egreso,year.join, Inflacion, TBP)

newdata <- BCR.lag2.newdata
#predecir
newdata2 <- cbind(newdata, predict(BCR.egreso.lag2, newdata, type = "response", 
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
ggsave(paste("7_EgresoBCR_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("EgresoBCR",Sys.Date(),".csv",sep = ""))

#Superavit
#seleccionar variables de interes
BCR.lag2.newdata <- BCR.lag2 %>%
  select(Superavit,year.join, Gobierno)

newdata <- BCR.lag2.newdata
#predecir
newdata2 <- cbind(newdata, predict(BCR.superavit.lag2, newdata, type = "response", 
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
ggsave(paste("8_SuperavitBCR_PredvsReal",Sys.Date(),".png",sep = ""), width = 20, height = 12, units = "cm")
#guardar datos
write.csv(newdata3, paste("SuperavitBCR",Sys.Date(),".csv",sep = ""))
