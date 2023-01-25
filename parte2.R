data=estad_explained
data2=estad_explained2

carrera=factor(data$carrera_actual, labels = c("CA
","Externo
", "IC MA
", "IC ME
", "IICG
"))

hjuego=factor(data$horas_juego, labels = c("10-15","15-20","20-25", "5-10", ">25", "<5"))
hjuego=factor(hjuego, levels = levels(hjuego)[c(6,4,1,2,3,5)])

hstream=factor(data$horas_stream, labels = c("10-15","15-20","20-25", "5-10", ">25", "<5"), levels = c("Entre 10 y 15","Entre 15 y 20","Entre 20 y 25", "Entre 5 y 10", "Más de 25", "Menos de 5"))
hstream=factor(hstream, levels = levels(hstream)[c(6,4,1,2,3,5)])

stream=factor(data$contenido)

elo=factor(data$elo_maximo, labels = c("Destacado","Alto","Bajo", "Medio","Medio alto", "Medio bajo", "Intro.", "No juega") , levels = c("Destacado","Elo alto","Elo bajo", "Elo medio","Elo medio alto", "Elo medio bajo", "Introductorio", "Nunca he jugado ranked"))
elo=factor(elo, levels = levels(elo)[c(8,7,3,6,4,5,2,1)], ordered=T)

#librerias

library(survival)
library(ggplot2)
library(dplyr)
library(olsrr)
library(stargazer)
library(fitdistrplus)
library(paletteer)
library(MASS)
library(xtable)
library(texreg)

#estadísticas descriptivas

colorsc = c("#E67E22", "#2ECC71", "#3498DB", "#e74c3c",  "#9B59B6")
colorss = c("#e74c3c","#3498DB", "#2ECC71")

pie(table(carrera), col=colorsc, main="Carrera de los encuestados")
pie(table(data$sexo), col=colorss, main="Género de los encuestados")

barplot(table(data$nota_estad), col=paletteer_c("grDevices::Plasma", 10), main = "Distribución de notas Estadística I")

barplot(table(hjuego), col=paletteer_c("grDevices::Plasma", 6), main = "Horas de juego semanales, entre jugadores")
barplot(table(hstream), col=paletteer_c("grDevices::Plasma", 6), main = "Horas de consumo stream/videos semanales, entre jugadores")

barplot(table(elo), col=paletteer_c("grDevices::Plasma", 8), main = "Distribución elo de jugadores (Ranked)")

#interval regression - modelo base

Y <- with(data, Surv(nota_estad_low, nota_estad_high, event=rep(3,nrow(data)), type="interval"))

RLI <- survreg(Y ~ sexo_var + carrera + juega_online, data=data, dist="gaussian")

k=6

AICRLI = -2*logLik(RLI)+2*k
BICRLI = -2*logLik(RLI)+log(105)*k
AICRLI
BICRLI

externo = factor(data$carrera_actual, labels = c(0,1,0,0,0))

RLIE = survreg(Y ~ sexo_var + externo, data=data, dist="gaussian")

k2=2

AICRLIE = -2*logLik(RLIE)+2*k2
BICRLIE = -2*logLik(RLIE)+log(105)*k2
AICRLIE
BICRLIE

RLIS = survreg(Y ~ sexo_var, data=data, dist="gaussian")

k3=1

AICRLIS = -2*logLik(RLIS)+2*k3
BICRLIS = -2*logLik(RLIS)+log(105)*k3
AICRLIS
BICRLIS

RLIIN = survreg(Y ~ 1, data=data, dist="gaussian")

#interval regression - modelo extendido

dataj=na.omit(data)

carrera2=factor(dataj$carrera_actual, labels = c("CA
","Externo
", "IC MA
", "IC ME
", "IICG
"))

Y2 <- with(dataj, Surv(nota_estad_low, nota_estad_high, event=rep(3,nrow(dataj)), type="interval"))

RLIEE = survreg(Y2 ~ sexo_var + carrera2 + horas_juego + horas_stream + contenido + elo_maximo, data=dataj, dist="gaussian")

k4=24

AICRLIEE = -2*logLik(RLIEE)+2*k4
BICRLIEE = -2*logLik(RLIEE)+log(105)*k4
AICRLIEE
BICRLIEE

RLIEE2 = survreg(Y2 ~ 1, data=dataj, dist="gaussian")
FRLIEE2 =  survreg(Y2 ~ sexo_var + carrera2 + horas_juego + horas_stream + contenido + elo_maximo, data=dataj, dist="gaussian")

baselection = stepAIC(FRLIEE2, scale = 0,
        direction = "backward",
        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
        k = 2)

foselection = stepAIC(RLIEE2, scale = 0,
        direction = "forward",
        trace = 1, scope=FRLIEE2, keep = NULL, steps = 1000, use.start = FALSE,
        k = 2)

RLIEEE = survreg(Y2 ~ elo_maximo, data=dataj, dist="gaussian")


#

elo2=factor(data2$elo_maximo, labels = c("Destacado","Alto","Bajo", "Medio","Medio alto", "Medio bajo", "Intro.", "No juega R.", "No juega") , levels = c("Destacado","Elo alto","Elo bajo", "Elo medio","Elo medio alto", "Elo medio bajo", "Introductorio", "No juega", "Nunca he jugado ranked"))
elo2=factor(elo2, levels = levels(elo2)[c(9,8,7,3,6,4,5,2,1)], ordered=T)

hjuego2=factor(data2$horas_juego, labels = c("10-15","15-20","20-25", "5-10", ">25", "<5","No juega online"))
hjuego2=factor(hjuego2, levels = levels(hjuego2)[c(7,6,4,1,2,3,5)], ordered=T)

hstream2=factor(data2$horas_stream, labels = c("10-15","15-20","20-25", "5-10", ">25", "<5", "0"), levels = c("Entre 10 y 15","Entre 15 y 20","Entre 20 y 25", "Entre 5 y 10", "Más de 25", "Menos de 5", "No consume"))
hstream2=factor(hstream2, levels = levels(hstream2)[c(7,6,4,1,2,3,5)], ordered=T)

Y3 <- with(data2, Surv(nota_estad_low, nota_estad_high, event=rep(3,nrow(data2)), type="interval"))

RLIF1 = survreg(Y3 ~ 1, data=data2, dist="gaussian")

RLIF <- survreg(Y3 ~ sexo_var + carrera + horas_juego + horas_stream + contenido + elo_maximo , data=data2, dist="gaussian")
RLIF2 <- survreg(Y3 ~ sexo_var + carrera + hjuego2 + hstream2 + contenido + elo2 , data=data2, dist="gaussian")

hor = formula(Y3 ~ sexo_var + carrera + horas_juego + horas_stream + contenido + elo_maximo , data=data2, dist="gaussian")
hor2 = formula(Y3 ~ sexo_var + carrera + hjuego2 + hstream2 + contenido + elo2 , data=data2, dist="gaussian")

#Akaike linear

baselection2l = stepAIC(RLIF, scale = 0,
                        direction = "backward",
                        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)


foselection2l = stepAIC(RLIF1, scale = 0,
                        direction = "forward",
                        trace = 1, scope=hor, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)

foselection2l = stepAIC(RLIF1, scale = 0,
                        direction = "both",
                        trace = 1, scope=hor, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)

#Bayesian linear

baselection2l = stepAIC(RLIF, scale = 0,
                        direction = "backward",
                        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
                        k = log(105))


foselection2l = stepAIC(RLIF1, scale = 0,
                        direction = "forward",
                        trace = 1, scope=hor, keep = NULL, steps = 1000, use.start = FALSE,
                        k = log(105))

foselection2l = stepAIC(RLIF1, scale = 0,
                        direction = "both",
                        trace = 1, scope=hor, keep = NULL, steps = 1000, use.start = FALSE,
                        k = log(105))

#Akaike exponential

baselection2e = stepAIC(RLIF2, scale = 0,
                        direction = "backward",
                        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)


foselection2e= stepAIC(RLIF1, scale = 0,
                        direction = "forward",
                        trace = 1, scope=hor2, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)

foselection2e = stepAIC(RLIF1, scale = 0,
                        direction = "both",
                        trace = 1, scope=hor2, keep = NULL, steps = 1000, use.start = FALSE,
                        k = 2)

#Bayesian exponential

baselection2e = stepAIC(RLIF2, scale = 0,
                        direction = "backward",
                        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
                        k = log(105))


foselection2e= stepAIC(RLIF1, scale = 0,
                       direction = "forward",
                       trace = 1, scope=hor2, keep = NULL, steps = 1000, use.start = FALSE,
                       k = log(105))

foselection2e = stepAIC(RLIF1, scale = 0,
                        direction = "both",
                        trace = 1, scope=hor2, keep = NULL, steps = 1000, use.start = FALSE,
                        k = log(105))

#final models

FINAL= survreg(formula = Y ~ sexo_var + horas_stream, data = data2, 
                dist = "gaussian")

FINAL2= survreg(formula = Y ~ sexo_var, data = data2, 
                dist = "gaussian")

#test models

Relo2= survreg(formula = Y ~ sexo_var + elo2, data = data2, 
        dist = "gaussian")

Rhjuego2= survreg(formula = Y ~ sexo_var + hjuego2, data = data2, 
                 dist = "gaussian")

Rhstream2= survreg(formula = Y ~ sexo_var + hstream2, data = data2, 
                 dist = "gaussian")

SOLO = survreg(formula = Y ~ sexo_var, data = data2, 
               dist = "gaussian")

